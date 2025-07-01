;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Spider Giant", invented by the Esolang user
;; "PixelatedStarfish" and presented on November 16th, 2021, its
;; haecceity's provenance a novel dictionary's adhibition to Urban
;; Mueller's "brainfuck", desumed from tmemata originating in the song
;; "Spider" by the American rock band "They Might be Giants".
;; 
;; 
;; Concept
;; =======
;; The Spider Giant programming language establishes a syntactical
;; renovation upon the substratum provided by the brainfuck language,
;; applying to the entheus' single-symbol names a cambistry whence
;; emerges a series of phrases as allusions to the song "Spider" by the
;; American rock band "They Might Be Giants".
;; 
;; The novelty retains any other aspect in its ipsissima verba
;; designment; in particular, operating on a bilaterally infinite
;; dispansion of unsigned byte-valued cells, upon whose entirety a
;; mobile cell pointer exerts is influence, selecting at any instant
;; during a program's execution the currently active cell, this object
;; being entalented with the exclusive amenability for perquisitions
;; and modulations.
;; 
;; 
;; Instructions
;; ============
;; A mere reformulation of brainfuck, Spide Giant's competences do
;; neither remain alow nor rise aboon that of its stock-father's,
;; enumerating in its acquisition the acquainted octuple membership.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a requisite mete of nortelry's
;; adhibition concerning the language's operative components:
;; 
;;   ------------------------------------------------------------------
;;   Command                    | Effect
;;   ---------------------------+--------------------------------------
;;   We love you spider!        | Increments the current cell value by
;;                              | one. Upon a transgression of its
;;                              | upper bourne of 255, the state wraps
;;                              | around to the minimum of zero (0).
;;   ..................................................................
;;   Get rid of                 | Decrements the current cell value by
;;                              | one. Upon a transgression of its
;;                              | lower bourne of zero (0), the state
;;                              | wraps around to the maximum of 255.
;;   ..................................................................
;;   Spider                     | Translates the cell pointer one step
;;                              | to the right.
;;   ..................................................................
;;   He is our hero!            | Translates the cell pointer one step
;;                              | to the left.
;;   ..................................................................
;;   Must stop!                 | Prints the character whose ASCII code
;;                              | corresponds to the current cell value
;;                              | to the standard output conduit.
;;   ..................................................................
;;   Step on Spider!            | Queries the standard input conduit
;;                              | for a character and stores its ASCII
;;                              | code in the current cell.
;;   ..................................................................
;;   I promise not to kill you. | If the current cell contains the
;;                              | value zero (0), moves the instruction
;;                              | pointer (IP) forward to the position
;;                              | immediately succeeding the matching
;;                              | "Oh!" instruction; otherwise proceeds
;;                              | as usual.
;;   ..................................................................
;;   Oh!                        | If the current cell does not contain
;;                              | the value zero (0), moves the
;;                              | instruction pointer (IP) back to the
;;                              | position immediately succeeding the
;;                              | matching "I promise not to kill you."
;;                              | instruction; otherwise proceeds as
;;                              | usual.
;;   ------------------------------------------------------------------
;; 
;; == SPIDER GIANT AND BRAINFUCK ==
;; The fact of its cleronomy entalents the Spider Giant programming
;; language with patration in its potential for an equiparation with
;; the brainfuck entheus. The following table shall furnish the vincula
;; betwixt these two specimens in regards to their syntaxis and
;; operative elements:
;; 
;;   ---------------------------------------
;;   Spider Giant               | brainfuck
;;   ---------------------------+-----------
;;   Spider                     | >
;;   .......................................
;;   He is our hero!            | <
;;   .......................................
;;   We love you spider!        | +
;;   .......................................
;;   Get rid of                 | -
;;   .......................................
;;   Must stop!                 | .
;;   .......................................
;;   Step on Spider!            | ,
;;   .......................................
;;   I promise not to kill you. | [
;;   .......................................
;;   Oh!                        | ]
;;   ---------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization has been exercised in the programming
;; language Common Lisp, the process entirety a division into a twissel
;; of stages, the incipient, invested with amplified arduousness, that
;; of the Spider Giant program's transcription, thilk is edified upon a
;; series of words, into a more comfortable instructive format, ere the
;; actual interpretation tier unfolds.
;; 
;; == SPIDER GIANT TO BRAINFUCK CONVERSION: TOKEN QUEUE + PHRASES ==
;; The transcription from Spider Giant's more complex designment to the
;; eath evaluation potential of brainfuck introduces a parasceuastic
;; element in the execution stage.
;; 
;; This tier's conformation registers a dependency upon two dedicated
;; constituents: a token queue, responsible for the segregation of the
;; Spider Giant source code into words at whitespaces, as well as their
;; castaldy in a first-in first-out ordonnance; and, as a parhedral
;; adminiculum, a "phrase" as representation of the Spider Giant command
;; identifiers in a compound which lists their constituting words in
;; conjunction with the tantamount brainfuck symbols.
;; 
;; The following pseudocode tmema shall be entrusted with a formal
;; treatise's furnishment on the procedure the same from a piece of
;; Spider Giant source code begets an operative equivalent in the
;; brainfuck programming language:
;; 
;;   record Phrase
;;     words:  An ordered list of strings which represent the
;;             Spider Giant command identifier's tokens.
;;     length: The non-negative number of elements comprising the WORDS.
;;     action: A symbol which corresponds to the equivalent brainfuck
;;             instruction token for this Spider Giant command phrase.
;;   end record
;;   
;;   
;;   { A set of "Phrase" record instances.                             }
;;   let SPIDER_GIANT_PHRASES <-
;;   {
;;     makePhrase(("Spider"),                                   1, ">"),
;;     makePhrase(("He", "is", "our", "hero!"),                 4, "<"),
;;     makePhrase(("We", "love", "you", "spider!"),             4, "+"),
;;     makePhrase(("Get", "rid", "of"),                         3, "-"),
;;     makePhrase(("Must", "stop!"),                            2, "."),
;;     makePhrase(("Step", "on", "Spider!"),                    3, ","),
;;     makePhrase(("I", "promise", "not", "to", "kill", "you"), 5, "["),
;;     makePhrase(("Oh!"),                                      1, "]")
;;   }
;;   
;;   
;;   function convertSpiderGiantToBrainfuck (spiderGiantCode)
;;     Input:
;;       spiderGiantCode: A string representing the Spider Giant program
;;                        to translate into a piece of equivalent
;;                        brainfuck code.
;;     
;;     Output:
;;       brainfuckCode:   A string comprehending the brainfuck program
;;                        tanamount to the "spiderGiantCode".
;;     
;;     Process:
;;       let brainfuckCode <- prepare empty string
;;       let tokenizer     <- prepare tokenizer for spiderGiantCode
;;       
;;       load first token into tokenizer
;;       
;;       while tokenizer is not empty do
;;         let hasMatchedPhrase <- false
;;         
;;         for each phrase p in SPIDER_GIANT_PHRASES do
;;           { Attempt to ensure sufficient account of tokens in the   }
;;           { tokenizer for the matching agains the current phrase p. }
;;           if number of tokens in tokenizer < p.length then
;;             load tokens into tokenizer to match p.length
;;           end if
;;           
;;           if first p.length tokens in tokenizer match p.words then
;;             append p.action to brainfuckCode
;;             remove the first p.length tokens from the tokenizer
;;             hasMatchedPhrase <- true
;;           end if
;;         end for
;;         
;;         { No phrase matched? => Frontmost token is worthless. }
;;         if not hasMatchedPhrase then
;;           remove first token from tokenizer
;;         end if
;;         
;;         { Attempt to ensure non-empty tokenizer in order to obviate }
;;         { premature termination ere all tokens have been processed. }
;;         if tokenizer is empty then
;;           load next token into tokenizer
;;         end if
;;       end while
;;       
;;       return brainfuckCode
;;   end function
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-06-26
;; 
;; Sources:
;;   [esolang2024SpiderGiant]
;;   The Esolang contributors, "Spider Giant", April 29th, 2024
;;   URL: "https://esolangs.org/wiki/Spider_Giant"
;;   
;;   [lispworks2005clhyperspec_3.4.1]
;;   LispWorks, "3.4.1 Ordinary Lambda Lists",
;;     in "Common Lisp HyperSpec", 2005
;;   URL: "https://www.lispworks.com/documentation/HyperSpec/Body/
;;         03_da.htm"
;;   Notes:
;;     - Specifies the conformation of a lambda list in Common Lisp.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-type-definition-signature (signature)
  "Extracts from the type definition SIGNATURE the requisite information
   and returns two values:
     (1) The symbolic type name.
     (2) A list of zero or more formal parameters."
  (declare (type (or cons symbol) signature))
  (the (values symbol list)
    (typecase signature
      (symbol
        (values signature NIL))
      (cons
        (values
          (first signature)
          (first (rest signature))))
      (otherwise
        (error "Invalid type definition signature: ~s." signature)))))

;;; -------------------------------------------------------

(defmacro define-type-which-satisfies (signature (candidate-variable)
                                       &body body)
  "Defines a derived type whose agnomination and lambda list is desumed
   from the SIGNATURE, the subject of its docimasy [...]"
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    (multiple-value-bind (type-name lambda-list)
        (analyze-type-definition-signature signature)
      (declare (type symbol type-name))
      (declare (type list   lambda-list))
      `(deftype ,type-name ,lambda-list
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
           `(satisfies ,,predicate-variable))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type-which-satisfies
  (list-of (&optional (element-type '*)
                      (size         '*)))
  (candidate)
  "The ``list-of'' type defines a list composed of the SIZE tally of
   elements, each member of which complies to the ELEMENT-TYPE, both
   configurations defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (or (eq size '*)
        (=  (length (the list candidate))
            size))
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(define-type-which-satisfies
  (hash-table-of (&optional (key-type '*) (value-type '*)))
  (candidate)
  "The ``hash-table-of'' type defines a hash table whose conformation
   enumerates zero or more entries, each key among these complies with
   the KEY-TYPE and answers to a value of the VALUE-TYPE, for both is
   specified the generic sentinel ``*'' as a default."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   compass of which, among others, amplects the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variation on
   Spider Giant commands."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Spider Giant program as a
   one-dimensional simple array comprehending zero or more ``command''
   objects."
  '(simple-array command (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping betwixt
   matching jump points in a Spider Giant program, mediated by adminicle
   of the representative instructions' zero-based indices into the
   program's operation vector, and realized via a hash table whose keys
   and values both assume fixnum position specifications."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, its commorancy, as a corollary, the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Function-Head".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Function-Head
  "The ``Function-Head'' class serves in the ensconcement of a bespoke
   function definition's \"high-level\" specifications, which amplects
   the name, the return type, and an indication of its nature as a
   general or a \"setf-enabled\" operation."
  (name        (error "Missing function name.")
               :type      symbol
               :read-only T)
  (return-type T
               :type      T
               :read-only T)
  (is-setter-p NIL
               :type      boolean
               :read-only T))

;;; -------------------------------------------------------

(defun generate-function-name (head)
  "Creates and returns a Common Lisp \"defun\"-conformant representation
   of the function name as defined by its HEAD."
  (declare (type Function-Head head))
  (the (or (list-of symbol) symbol)
    (if (function-head-is-setter-p head)
      `(setf ,(function-head-name head))
      (function-head-name head))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Function-Parameter".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Function-Parameter
  "The ``Function-Parameter'' class is apportioned the dever of a
   function parameter's representation."
  (name          (error "Missing parameter name.")
                 :type      symbol
                 :read-only T)
  (type          T
                 :type      T
                 :read-only T)
  (is-optional-p NIL
                 :type      boolean
                 :read-only T)
  (default-value NIL
                 :type      T
                 :read-only T))

;;; -------------------------------------------------------

(defun generate-required-lambda-list-parameter-specifier (parameter)
  "Returns for the function PARAMETER a parameter specifier connable
   for its deployment in a lambda list."
  (declare (type Function-Parameter parameter))
  (the symbol
    (function-parameter-name parameter)))

;;; -------------------------------------------------------

(defun generate-optional-lambda-list-parameter-specifier (parameter)
  "Returns for the function PARAMETER a parameter specifier connable
   for its deployment in a lambda list."
  (declare (type Function-Parameter parameter))
  (the (or (list-of T) symbol)
    `(,(function-parameter-name parameter)
      ,(if (function-parameter-is-optional-p parameter)
         (function-parameter-default-value parameter)
         `(error "Missing value for parameter ~a."
            ',(function-parameter-name parameter))))))

;;; -------------------------------------------------------

(defun generate-declaration (parameter)
  "Returns for the function PARAMETER a list comprehending a twifold
   ``declare'' declaration; imprimis mentioning the PARAMETER type in
   a vinculum with its name, secondly ascertaining its status as an
   ``ignorable'' variable.
   ---
   Given a PARAMETER's partial decomposition into the jumelle of
     (NAME, TYPE),
   the following complex declaration's gendrure establishes this
   function's response:
     (declare (type TYPE NAME)
              (ignorable NAME))"
  (declare (type Function-Parameter parameter))
  (the (list-of T)
    `((declare
       (type
         ,(function-parameter-type parameter)
         ,(function-parameter-name parameter))
       (ignorable
         ,(function-parameter-name parameter))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Function-Parameter-List".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Function-Parameter-List
  (:constructor make-function-parameter-list (parameters)))
  "The ``Function-Parameter-List'' serves in the aggregation of zero or
   more function parameters in the guise of ``Function-Parameter''
   objects."
  (parameters (error "Missing parameters.")
              :type      (list-of Function-Parameter)
              :read-only T))

;;; -------------------------------------------------------

(defun all-parameters-are-required-p (parameters)
  "Determines whether all parameters partaking of the PARAMETERS list
   represent a required species, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Function-Parameter-List parameters))
  (the boolean
    (not
      (some #'function-parameter-is-optional-p
        (function-parameter-list-parameters parameters)))))

;;; -------------------------------------------------------

(defun generate-lambda-list (parameters)
  "Returns a for the function PARAMETERS a ``defun''-compatible lambda
   list."
  (declare (type Function-Parameter-List parameters))
  (the (list-of T)
    (cond
      ((null (function-parameter-list-parameters parameters))
        NIL)
      ((all-parameters-are-required-p parameters)
        `(,@(mapcar #'generate-required-lambda-list-parameter-specifier
              (function-parameter-list-parameters parameters))))
      (T
        `(&optional
          ,@(mapcar #'generate-optional-lambda-list-parameter-specifier
              (function-parameter-list-parameters parameters)))))))

;;; -------------------------------------------------------

(defun generate-declarations (parameters)
  "Returns a list comprehending for each parameter in the function
   PARAMETERS a list, the same bears a twifold ``declare'' declaration;
   imprimis mentioning the PARAMETER type in a vinculum with its name,
   secondly ascertaining its status as an ``ignorable'' variable.
   ---
   Given a parameter's partial decomposition into the jumelle of
     (NAME, TYPE),
   the following complex declaration's gendrure appertains to any
   element of the PARAMETERS list:
     (declare (type TYPE NAME)
              (ignorable NAME))"
  (declare (type Function-Parameter-List parameters))
  (the (list-of T)
    (loop
      for current-parameter
        of-type Function-Parameter
        in      (function-parameter-list-parameters parameters)
      append
        (copy-list
          (generate-declaration current-parameter)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of function definition macro.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluate-function-return-value (return-value)
  "Analyzes the declared function RETURN-VALUE and returns a connable
   representation thereof."
  (declare (type T return-value))
  (the T
    (cond
      ((null return-value)       T)
      ((eq   return-value :any)  T)
      ((eq   return-value :none) '(values))
      (T                         return-value))))

;;; -------------------------------------------------------

(defun parse-function-head (specification &optional (is-setter-p NIL))
  "Returns for the general function SPECIFICATION a connable
   ``Function-Head'' representation, optionally configured to adhere
   to a ``setf'' type via the IS-SETTER-P flag.
   ---
   For the SPECIFICATION, these weftages exhaust the homologation:
     ------------------------------------------------------------------
     Name and type | Interpretation
     --------------+---------------------------------------------------
     name          | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name)        | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :none)  | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :any)   | Defines a function norned by the NAME, returning
                   | a value of the comprehensive type ``T''.
     ..................................................................
     (name type)   | Defines a function norned by the NAME, returning
                   | an object of the specified TYPE.
     ------------------------------------------------------------------"
  (declare (type (or list symbol) specification))
  (declare (type boolean          is-setter-p))
  (the Function-Head
    (typecase specification
      (symbol
        (make-function-head
          :name        specification
          :return-type T
          :is-setter-p is-setter-p))
      (list
        (make-function-head
          :name        (first specification)
          :return-type (evaluate-function-return-value
                         (first
                           (rest specification)))
          :is-setter-p is-setter-p))
      (otherwise
        (error "Invalid function head specification: ~s."
          specification)))))

;;; -------------------------------------------------------

(defun parse-function-parameter (specification)
  "Parses the functional parameter SPECIFICATION and returns a connable
   ``Function-Parameter'' object representation thereof.
   ---
   The following weftages in conjunction with the causata shall be
   imposed:
     ------------------------------------------------------------------
     Forbisen            | Interpretation
     --------------------+---------------------------------------------
     name                | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name)              | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name type)         | Defines a required parameter norned by the
                         | NAME, and subsuming intto the TYPE.
     ..................................................................
     (name type default) | Defines an optional parameter norned by the
                         | NAME, subsuming into the TYPE, and endowed
                         | with the DEFAULT value.
     ------------------------------------------------------------------"
  (declare (type (or list symbol) specification))
  (the Function-Parameter
    (typecase specification
      (symbol
        (make-function-parameter
          :name          specification
          :type          T
          :is-optional-p NIL))
      (list
        (case (length (the list specification))
          (1
            (make-function-parameter
              :name          (first specification)
              :type          T
              :is-optional-p NIL))
          (2
            (make-function-parameter
              :name          (first  specification)
              :type          (second specification)
              :is-optional-p NIL))
          (3
            (make-function-parameter
              :name          (first  specification)
              :type          (second specification)
              :is-optional-p T
              :default-value (third specification)))
          (otherwise
            (error "Invalid parameter specification: ~s."
              specification)))))))

;;; -------------------------------------------------------

(defun parse-function-parameter-list (specifications)
  "Parses the SPECIFICATIONS and returns a ``Function-Paramter-List''
   representation of its obtained parameters."
  (declare (type (list-of (or list symbol)) specifications))
  (the Function-Parameter-List
    (make-function-parameter-list
      (mapcar #'parse-function-parameter specifications))))

;;; -------------------------------------------------------

(defmacro define-function (name-and-type (&rest lambda-list)
                           &body body)
  "Defines a new function whose agnomination, and contingent return
   type, are desumed from the NAME-AND-TYPE specification, while its
   formal parameters' reception constitutes the LAMBDA-LIST's dation,
   the forms to execute being the BODY's establishment, their entirety
   ensconced in an implicit ``progn'' form, the same itself resides
   inwith a ``the'' special form which communicates the function's
   return type.
   ---
   For the NAME-AND-TYPE, these weftages exhaust the homologation:
     ------------------------------------------------------------------
     Name and type | Interpretation
     --------------+---------------------------------------------------
     name          | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name)        | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :none)  | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :any)   | Defines a function norned by the NAME, returning
                   | a value of the comprehensive type ``T''.
     ..................................................................
     (name type)   | Defines a function norned by the NAME, returning
                   | an object of the specified TYPE.
     ------------------------------------------------------------------
   ---
   For the LAMBDA-LIST, the following patterns' admission is furnished:
     ------------------------------------------------------------------
     Lambda list         | Interpretation
     --------------------+---------------------------------------------
     name                | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name)              | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name type)         | Defines a required parameter norned by the
                         | NAME, and subsuming intto the TYPE.
     ..................................................................
     (name type default) | Defines an optional parameter norned by the
                         | NAME, subsuming into the TYPE, and endowed
                         | with the DEFAULT value.
     ------------------------------------------------------------------"
  (let ((head       (parse-function-head name-and-type NIL))
        (parameters (parse-function-parameter-list lambda-list)))
    (declare (type Function-Head           head))
    (declare (type Function-Parameter-List parameters))
    `(defun ,(generate-function-name head)
            ,(generate-lambda-list   parameters)
       ,(or (and (stringp (first body))
                 (>= (length body) 2)
                 (pop body))
            "")
       ,@(generate-declarations parameters)
       (the ,(function-head-return-type head)
         (progn
           ,@body)))))

;;; -------------------------------------------------------

(defmacro define-setter-function (name-and-type (&rest lambda-list)
                                  &body body)
  "Defines a new ``setf''-function whose agnomination, and contingent
   return type, are desumed from the NAME-AND-TYPE specification, while
   its formal parameters' reception constitutes the LAMBDA-LIST's
   dation, the forms to execute being the BODY's establishment, their
   entirety ensconced in an implicit ``progn'' form, the same itself
   resides inwith a ``the'' special form which communicates the
   function's return type.
   ---
   For the NAME-AND-TYPE, these weftages exhaust the homologation:
     ------------------------------------------------------------------
     Name and type | Interpretation
     --------------+---------------------------------------------------
     name          | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name)        | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :none)  | Defines a function norned by the NAME, returning
                   | no value.
     ..................................................................
     (name :any)   | Defines a function norned by the NAME, returning
                   | a value of the comprehensive type ``T''.
     ..................................................................
     (name type)   | Defines a function norned by the NAME, returning
                   | an object of the specified TYPE.
     ------------------------------------------------------------------
   ---
   For the LAMBDA-LIST, the following patterns' admission is furnished:
     ------------------------------------------------------------------
     Lambda list         | Interpretation
     --------------------+---------------------------------------------
     name                | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name)              | Defines a required parameter norned by the
                         | NAME, its type the comprehensive ``T''.
     ..................................................................
     (name type)         | Defines a required parameter norned by the
                         | NAME, and subsuming intto the TYPE.
     ..................................................................
     (name type default) | Defines an optional parameter norned by the
                         | NAME, subsuming into the TYPE, and endowed
                         | with the DEFAULT value.
     ------------------------------------------------------------------"
  (let ((head       (parse-function-head name-and-type T))
        (parameters (parse-function-parameter-list lambda-list)))
    (declare (type Function-Head           head))
    (declare (type Function-Parameter-List parameters))
    `(defun ,(generate-function-name head)
            ,(generate-lambda-list   parameters)
       ,(or (and (stringp (first body))
                 (>= (length body) 2)
                 (pop body))
            "")
       ,@(generate-declarations parameters)
       (the ,(function-head-return-type head)
         (progn
           ,@body)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (get-boolean-value-of boolean) ((object T))
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (not (null object)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of character 6) +WHITESPACE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTERS+
  (mapcar #'code-char '(9 10 11 12 13 32))
  "Defines the recognized whitespace characters.")

;;; -------------------------------------------------------

(define-function (character-is-whitespace-p boolean)
  ((candidate                character #\Null))
  "Determines whether the CANDIDATE represents a whitespace
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (get-boolean-value-of
    (member candidate +WHITESPACE-CHARACTERS+ :test #'char=)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (convert-to-simple-string simple-string)
  ((source string))
  "Creates and returns a simple string representation of the SOURCE."
  (coerce source 'simple-string))

;;; -------------------------------------------------------

(define-function (convert-to-simple-base-string simple-base-string)
  ((source string))
  "Creates and returns a simple base string representation of the
   SOURCE string."
  (coerce source 'simple-base-string))

;;; -------------------------------------------------------

(define-function (string-is-empty-p boolean) ((source simple-string))
  "Determines whether the SOURCE represents an empty string, such
   enumerates a componency of exactly zero members, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (zerop
      (length source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          simple-string
    :documentation "The piece of Spider Giant source code to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' class applies itself to the lexical analysis of a
     piece of Spider Giant source code, pursuing the telos of its
     tokens' recognition and extraction."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  "Updates the LEXER's current character, stores its new state in the
   same, and returns no value."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type simple-string       source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (schar source position))))
  (values))

;;; -------------------------------------------------------

(define-function (make-lexer Lexer) ((source string))
  "Creates and returns a fresh ``Lexer'' whose devotion appertains to
   the piece of Spider Giant SOURCE code's lexical analyzation."
  (make-instance 'Lexer :source
    (convert-to-simple-string source)))

;;; -------------------------------------------------------

(define-function (update-lexer-character :none) ((lexer Lexer))
  "Updates the LEXER's character based upon its current position into
   its source and returns no value."
  (with-slots (source position character) lexer
    (declare (type simple-string       source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (schar source position))))
  (values))

;;; -------------------------------------------------------

(define-function (locate-next-token fixnum) ((lexer Lexer))
  "Moves the LEXER's position cursor to the start of the nearest
   following token, or, upon its absence, to the underlying source's
   desinence, and produced position."
  (with-slots (source position) lexer
    (declare (type simple-string source))
    (declare (type fixnum        position))
    (setf position
      (or (position-if-not #'character-is-whitespace-p source
            :start position)
          (length source)))
    (update-lexer-character lexer)
    position))

;;; -------------------------------------------------------

(define-function (locate-end-of-next-token fixnum) ((lexer Lexer))
  "Moves the LEXER's position cursor to the start of the nearest
   following token, or, upon its absence, to the underlying source's
   desinence, and the thus produced position."
  (with-slots (source position) lexer
    (declare (type simple-string source))
    (declare (type fixnum        position))
    (setf position
      (or (position-if #'character-is-whitespace-p source
            :start position)
          (length source)))
    (update-lexer-character lexer)
    position))

;;; -------------------------------------------------------

(define-function (get-next-token string) ((lexer Lexer))
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any further
   request with a fresh empty string."
  (with-slots (source) lexer
    (declare (type simple-string source))
    (convert-to-simple-string
      (subseq source
        (locate-next-token        lexer)
        (locate-end-of-next-token lexer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Phrase".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Phrase
  (:constructor make-phrase (words effect
                             &aux (length (length words)))))
  "The ``Phrase'' class serves in the ensconcement of a Spider Giant
   command identifier in a more comfortable mode, amplecting both the
   phrase's tokens and the affiliated brainfuck command in one
   aggregate."
  (words  (error "The phrase is missing its words.")
          :type      (list-of simple-string)
          :read-only T)
  (length (error "The phrase is missing its length.")
          :type      fixnum
          :read-only T)
  (effect (error "The phrase is missing its command.")
          :type      command
          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of phrase list.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of Phrase 8) +PHRASES+))

;;; -------------------------------------------------------

(defparameter +PHRASES+
  (list
    (make-phrase '("Spider")
                 :move-right)
    (make-phrase '("He" "is" "our" "hero!")
                 :move-left)
    (make-phrase '("We" "love" "you" "spider!")
                 :increment)
    (make-phrase '("Get" "rid" "of")
                 :decrement)
    (make-phrase '("Must" "stop!")
                 :output)
    (make-phrase '("Step" "on" "Spider!")
                 :input)
    (make-phrase '("I" "promise" "not" "to" "kill" "you.")
                 :jump-forward)
    (make-phrase '("Oh!")
                 :jump-back))
  "Lists the recognized phrases, everichon among these representing a
   Spider Giant command identifier.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (commands)
  "Creates and returns a fresh ``program'' representation of the
   COMMANDS list."
  (declare (type (list-of command) commands))
  (the program
    (coerce commands
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token-Queue".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Queue) *) load-next-token-into-queue))

;;; -------------------------------------------------------

(defclass Token-Queue ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer.")
    :type          Lexer
    :documentation "The token acquisition provenance.")
   (head
    :initform      (cons "" NIL)
    :type          (list-of simple-string)
    :documentation "A reference to the queue's first cons cell.")
   (tail
    :type          (or null (cons simple-string *))
    :documentation "A reference to the queue's desinent cons cell,
                    employed for efficient insertions at the rear.")
   (size
    :initform      0
    :type          fixnum
    :documentation "The tally of tokens maintained by this queue."))
  (:documentation
    "The ``Token-Queue'' class serves in the castaldy of an arbitrary
     tally of tokens, loaded upon desideration and necessity from a
     conjoined lexer, appending to new members to its rear, while
     deleting from its front."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokens Token-Queue) &key)
  "Initializes the TOKENS queue's tail pointer to the head cons cell,
   loads and stores a first token, and returns no value."
  (declare (type Token-Queue tokens))
  (with-slots (head tail) tokens
    (declare (type (list-of simple-string)          head))
    (declare (type (or null (cons simple-string *)) tail))
    (setf tail
      (last head)))
  (load-next-token-into-queue tokens)
  (values))

;;; -------------------------------------------------------

(define-function (make-token-queue Token-Queue) ((lexer Lexer))
  "Creates and returns a fresh ``Token-Queue'' whose content is derived
   from the LEXER."
  (make-instance 'Token-Queue :lexer lexer))

;;; -------------------------------------------------------

(define-function (token-queue-is-empty-p boolean) ((tokens Token-Queue))
  "Determines whether the TOKENS queue is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (null
    (rest
      (slot-value tokens 'head))))

;;; -------------------------------------------------------

(define-function (get-token-queue-size fixnum) ((tokens Token-Queue))
  "Returns the tally of tokens partaking of the TOKENS queue."
  (slot-value tokens 'size))

;;; -------------------------------------------------------

(define-function (get-queued-tokens (list-of simple-string))
  ((tokens Token-Queue))
  "Returns the elements currently maintained by the TOKENS queue."
  (rest (slot-value tokens 'head)))

;;; -------------------------------------------------------

(define-function (add-token :none) ((tokens    Token-Queue)
                                    (new-token simple-string))
  "Appends the NEW-TOKEN to the TOKENS queue's rear, if the aspiring
   member does not represent an empty string, and returns no value."
  (unless (string-is-empty-p new-token)
    (with-slots (tail size) tokens
      (declare (type (or null (cons simple-string *)) tail))
      (declare (type fixnum                           size))
      (let ((inserted-cons (cons new-token NIL)))
        (declare (type (cons simple-string null) inserted-cons))
        (setf (rest tail) inserted-cons)
        (setf tail        (rest tail))
        (incf size))))
  (values))

;;; -------------------------------------------------------

(define-function (dequeue-token :none) ((tokens Token-Queue))
  "Removes the token at the TOKENS queue's front, if possible, and
   returns no value."
  (unless (token-queue-is-empty-p tokens)
    (with-slots (head size) tokens
      (declare (type (list-of simple-string) head))
      (declare (type fixnum                  size))
      (setf head (rest head))
      (decf size)))
  (values))

;;; -------------------------------------------------------

(define-function (load-next-token-into-queue :none)
  ((tokens Token-Queue))
  "Queries the next token from the TOKENS queue's underlying lexer,
   stores thilk, if not the empty string, and returns no value."
  (with-slots (lexer) tokens
    (declare (type Lexer lexer))
    (add-token tokens
      (get-next-token lexer)))
  (values))

;;; -------------------------------------------------------

(define-function (ensure-non-empty-queue :none) ((tokens Token-Queue))
  "Ascertains, as far as possible, no vacancy in the TOKENS queue by
   attempting to load and store the next token from the underlying lexer
   if the queue is empty, in any case returning no value."
  (when (token-queue-is-empty-p tokens)
    (load-next-token-into-queue tokens))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((tokens Token-Queue) (destination T))
  (declare (type Token-Queue tokens))
  (declare (type destination destination))
  (format destination "Token-Queue(~{~s~^, ~})"
    (get-queued-tokens tokens)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (load-requisite-tokens-for-phrase :none)
  ((tokens        Token-Queue)
   (probed-phrase Phrase))
  "Loads and stores the requisite number of tokens into the TOKENS queue
   necessary to probe thilk's content agains the PROBED-PHRASE's
   expected constituents and returns no value."
  (let ((number-of-necessary-tokens
          (- (phrase-length        probed-phrase)
             (get-token-queue-size tokens))))
    (declare (type fixnum number-of-necessary-tokens))
    (when (plusp number-of-necessary-tokens)
      (loop repeat number-of-necessary-tokens do
        (load-next-token-into-queue tokens))))
  (values))

;;; -------------------------------------------------------

(define-function (phrase-matches-p boolean)
  ((tokens        Token-Queue)
   (probed-phrase Phrase))
  "Determines whether the TOKENS queue front elements match the
   PROBED-PHRASE's expectations, on necessity loading further tokens
   into the queue, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (load-requisite-tokens-for-phrase tokens probed-phrase)
  (get-boolean-value-of
    (and
      (>= (get-token-queue-size tokens)
          (phrase-length        probed-phrase))
      (every #'string=
        (get-queued-tokens tokens)
        (phrase-words      probed-phrase)))))

;;; -------------------------------------------------------

(define-function (match-phrase (or null command))
  ((tokens Token-Queue)
   (phrase Phrase))
  "Attempts to match the PHRASE against the TOKENS queue's
   contemporaneous content, on success returning a covenable ``command''
   representation of the PHRASE's intent; otherwise responds with
   ``NIL''."
  (prog1
    (when (phrase-matches-p tokens phrase)
      (loop repeat (phrase-length phrase) do
        (dequeue-token tokens))
      (phrase-effect phrase))
    (ensure-non-empty-queue tokens)))

;;; -------------------------------------------------------

(define-function (parse-command (or null command))
  ((tokens Token-Queue))
  "Attempts to parse a Spider Giant command employing the TOKENS queue's
   contemporaneous content, returning on success a connable ``command''
   representation; otherwise responds with ``NIL''."
  (loop
    for current-phrase
      of-type Phrase
      in      +PHRASES+
    for obtained-effect
      of-type (or null command)
      =       (match-phrase tokens current-phrase)
    when obtained-effect do
      (return obtained-effect)
    end))

;;; -------------------------------------------------------

(define-function (parse-program program) ((tokens Token-Queue))
  "Parses the Spider Giant commands entailed in the TOKENS queue's
   tokens and returns a fresh ``program'' representation thereof."
  (make-program
    (loop
      until (token-queue-is-empty-p tokens)
      
      for next-command
        of-type (or null command)
        =       (parse-command tokens)
      
      if next-command
        collect next-command
      else do
        (dequeue-token tokens)
      end)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (prepare-empty-jump-table jump-table) ()
  "Creates and returns a fresh and initially vacant ``jump-table''."
  (make-hash-table :test #'eql))

;;; -------------------------------------------------------

(define-function (connect-jump-points :none) ((connections jump-table)
                                              (source      fixnum)
                                              (destination fixnum))
  "Connects the SOURCE and DESTINATION jump points, specifying the
   zero-based positions of the respective instructions inside of a
   Spider Giant program, in a bilaterally fashion, stores this
   combination in the CONNECTIONS table, and returns no value."
  (psetf
    (gethash source      connections) destination
    (gethash destination connections) source)
  (values))

;;; -------------------------------------------------------

(define-function (build-jump-table jump-table) ((program program))
  "Creates and returns a fresh ``jump-table'' which connects the
   Spider Giant PROGRAM's forward and back jump instructions in a
   bilateral mode by mediation of their zero-based indices."
  (let ((connections         (prepare-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type jump-table       connections))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for current-command  of-type command across program
      and current-position of-type fixnum  from   0 by 1
      
      if (eq current-command :jump-forward) do
        (push current-position forward-jump-points)
      else if (eq current-command :jump-back) do
        (if forward-jump-points
          (connect-jump-points connections
            (pop forward-jump-points)
            current-position)
          (error "Unmatched back jump point."))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p."
            (length forward-jump-points))))
    
    connections))

;;; -------------------------------------------------------

(define-function (locate-jump-destination fixnum)
  ((connections jump-table)
   (start-point fixnum))
  "Returns the destination reached by the START-POINT, as defined in
   the CONNECTIONS table; or signals an error of an unspecified type
   upon its disrespondency."
  (or (gethash start-point connections)
      (error "No destination jump point defined for the position ~d."
        start-point)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :type          unsigned-byte
    :documentation "The explicitly configured cells' states, in their
                    entirety ensconced as one aggregate in an unsigned
                    integer-encoded binary sequence.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer which at any instant selects the
                    the currently active cell.
                    ---
                    While the cell pointer operates on a more abstact
                    tier on the tape, admitting signed integer cell
                    indices, all explicitly configured cells' data is
                    amplected by the integer-encoded BITS sequence,
                    whence ensues the necessity to translate the cell
                    POINTER into an unsigned integer byte offset into
                    the BITS; for thilk exercise please consult the
                    SMALLEST-ACCESSED-CELL-INDEX alow.")
   (smallest-accessed-cell-index
    :initform      0
    :type          integer
    :documentation "The minimum cell index selected during a program's
                    execution by the cell POINTER, its purpose that of
                    a warklume for the supputation of the currently
                    selected cell's byte from the underlying BITS
                    sequence through a transformation of the cell
                    POINTER."))
  (:documentation
    "The ``Tape'' class serves in the Spider Giant memory's
     accoutrement, its bailiwick the establishment of a bilaterally
     bourneless dispansion of unsigned byte-valued cells, the currently
     active member, entalented with the sole amenability to
     perquisitions and modulations, designated by a mobile cell
     pointer's adminiculum."))

;;; -------------------------------------------------------

(define-function (prepare-pristine-tape Tape) ()
  "Creates and returns a fresh ``Tape'' residing at its inchoation in
   a zero-valued state anent all of its cells."
  (make-instance 'Tape))

;;; -------------------------------------------------------

(define-function (locate-byte-for-current-cell :any) ((tape Tape))
  "Returns an implementation-dependent byte specifier which selects the
   eight bits from the TAPE's integer-encoded binary sequence
   corresponding to the cell pointer's contemporaneously designated
   current cell."
  (with-slots (pointer smallest-accessed-cell-index) tape
    (declare (type integer pointer))
    (declare (type integer smallest-accessed-cell-index))
    (byte 8
      (* (- pointer smallest-accessed-cell-index) 8))))

;;; -------------------------------------------------------

(define-function (current-cell-value octet) ((tape Tape))
  "Returns the unsigned byte value maintained by the TAPE's currently
   selected cell."
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (ldb (locate-byte-for-current-cell tape) bits)))

;;; -------------------------------------------------------

(define-setter-function (current-cell-value :none) ((new-value integer)
                                                    (tape      Tape))
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping of the datum into the admissible
   unsigned byte range of [0, 255], and returns no value."
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (setf (ldb (locate-byte-for-current-cell tape) bits)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(define-function (current-cell-contains-zero-p boolean) ((tape Tape))
  "Determines whether the TAPE's currently selected cell contains the
   value zero (0), returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (get-boolean-value-of
    (zerop
      (current-cell-value tape))))

;;; -------------------------------------------------------

(define-function (move-cell-pointer-right :none) ((tape Tape))
  "Translates the TAPE's cell pointer one step in the dextral airt and
   returns no value."
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(define-function (move-cell-pointer-left :none) ((tape Tape))
  "Translates the TAPE's cell pointer one step in the sinistral airt and
   returns no value."
  (with-slots (pointer smallest-accessed-cell-index bits) tape
    (declare (type integer       pointer))
    (declare (type integer       smallest-accessed-cell-index))
    (declare (type unsigned-byte bits)
             (ignorable          bits))
    (decf pointer)
    (when (< pointer smallest-accessed-cell-index)
      (decf smallest-accessed-cell-index)
      (setf bits
        (ash bits 8))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function (execute-spider-giant-program :none)
  ((program           program)
   (displays-prompt-p boolean T))
  "Executes the Spider Giant PROGRAM and returns no value.
   ---
   If the DISPLAYS-PROMPT-P flag is activated, as constitutes the
   deportment's default, each request for a user input is preceded by a
   prompt message issued to the standard output conduit; otherwise, this
   epiphenomenon waives its emergence."
  (let ((ip          0)
        (connections (build-jump-table program))
        (tape        (prepare-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type jump-table connections))
    (declare (type Tape       tape))
    
    (symbol-macrolet
        ((program-has-completed-p
          (the boolean
            (not (array-in-bounds-p program ip))))
         (current-command
          (the command
            (aref program ip))))
      (declare (type boolean program-has-completed-p))
      (declare (type command current-command))
      
      (loop until program-has-completed-p do
        (case current-command
          (:move-right
            (move-cell-pointer-right tape))
          
          (:move-left
            (move-cell-pointer-left tape))
          
          (:increment
            (incf (current-cell-value tape)))
          
          (:decrement
            (decf (current-cell-value tape)))
          
          (:output
            (format *standard-output* "~c"
              (code-char
                (current-cell-value tape))))
          
          (:input
            (when displays-prompt-p
              (format *standard-output* "~&>> "))
            (finish-output *standard-output*)
            (setf (current-cell-value tape)
              (char-code
                (read-char *standard-input* NIL
                  (code-char 0))))
            (clear-input *standard-input*))
          
          (:jump-forward
            (when (current-cell-contains-zero-p tape)
              (setf ip
                (locate-jump-destination connections ip))))
          
          (:jump-back
            (unless (current-cell-contains-zero-p tape)
              (setf ip
                (locate-jump-destination connections ip))))
          
          (otherwise
            (error "Invalid command encountered at position ~d: ~s."
              ip current-command)))
        
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(define-function (interpret-spider-giant :none)
  ((code              string)
   (displays-prompt-p boolean T))
  "Interprets the piece of Spider Giant source CODE and returns no
   value.
   ---
   If the DISPLAYS-PROMPT-P flag is activated, as constitutes the
   deportment's default, each request for a user input is preceded by a
   prompt message issued to the standard output conduit; otherwise, this
   epiphenomenon waives its emergence."
  (execute-spider-giant-program
    (parse-program
      (make-token-queue
        (make-lexer code)))
    displays-prompt-p)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Spider-Giant converter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-function
  (convert-brainfuck-to-spider-giant (or null simple-string))
  ((brainfuck-code string)
   (destination    destination NIL))
  "Generates for the piece of BRAINFUCK-CODE an equivalent Spider Giant
   program, writes thilk to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh simple base string comprehending
   the result."
  (if destination
    (loop
      for current-character of-type character across brainfuck-code
      do
        (format destination "~@?"
          (case current-character
            (#\>       "~&Spider")
            (#\<       "~&He is our hero!")
            (#\+       "~&We love you spider!")
            (#\-       "~&Get rid of")
            (#\.       "~&Must stop!")
            (#\,       "~&Step on Spider!")
            (#\[       "~&I promise not to kill you.")
            (#\]       "~&Oh!")
            (otherwise ""))))
    (with-output-to-string (spider-giant-code)
      (declare (type string-stream spider-giant-code))
      (convert-to-simple-base-string
        (convert-brainfuck-to-spider-giant
          brainfuck-code
          spider-giant-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output conduit.
(interpret-spider-giant
  "
  We love you spider!
  I promise not to kill you.
  Get rid of
  Get rid of
  Spider
  Get rid of
  I promise not to kill you.
  Spider
  Spider
  We love you spider!
  Spider
  Get rid of
  Get rid of
  Get rid of
  Get rid of
  Get rid of
  He is our hero!
  He is our hero!
  Oh!
  He is our hero!
  Get rid of
  Get rid of
  He is our hero!
  Get rid of
  Get rid of
  Get rid of
  Oh!
  Spider
  Get rid of
  Must stop!
  Spider
  Spider
  Spider
  We love you spider!
  Must stop!
  Spider
  Spider
  Must stop!
  Must stop!
  We love you spider!
  We love you spider!
  We love you spider!
  I promise not to kill you.
  Must stop!
  Spider
  Oh!
  He is our hero!
  He is our hero!
  He is our hero!
  He is our hero!
  Must stop!
  We love you spider!
  We love you spider!
  We love you spider!
  Must stop!
  Get rid of
  Get rid of
  Get rid of
  Get rid of
  Get rid of
  Get rid of
  Must stop!
  He is our hero!
  He is our hero!
  Get rid of
  Must stop!
  Spider
  Spider
  Spider
  Spider
  We love you spider!
  Must stop!
  ")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-spider-giant
  "Step on Spider!
   I promise not to kill you.
   Must stop!
   Step on Spider!
   Oh!")
