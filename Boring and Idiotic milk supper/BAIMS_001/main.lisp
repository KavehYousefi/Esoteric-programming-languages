;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Boring and Idiotic milk supper", designed by the Esolang
;; user "Lemonz" and presented in the year 2022, the kenspeckle
;; attribute of which involves metaphors for the consumption of milk,
;; and the appertaining side effects, in the guise of commands.
;; 
;; 
;; Concept
;; =======
;; The Boring and Idiotic milk supper, abbreviated BAIMS, programming
;; language pursues the fantastically elaborated simulation of the
;; casual milk consumption ceremonies.
;; 
;; == LINGUISTIC PROPAGATION OF A THEME ==
;; The BAIMS language begets a diction of its own as an expression of
;; its modeled subject. Please note that, while contributing respect to
;; this kenspeckle linguistic produce, the actual documentation in this
;; place communicates via a more standardized tongue.
;; 
;;   ------------------------------------------------------------------
;;   BAIMS term  | Translation | Context
;;   ------------+-------------+---------------------------------------
;;   stomach     | accumulator | Designates one of the memory cells,
;;               |             | or accumulators.
;;   ..................................................................
;;   action      | command     | Appertains to the command identifiers,
;;               |             | such as "sip" and "insulin".
;;   ..................................................................
;;   consequence | function    | Describes the effect of a command
;;               |             | (action), such as the incrementing of
;;               |             | the current accumulator (stomach) by
;;               |             | a single unit via the command (action)
;;               |             | "sip".
;;   ..................................................................
;;   curfew      | syntax      | Addresses a valid BAIMS program's
;;               |             | ordonnance.
;;   ------------------------------------------------------------------
;; 
;; == A DAY IN A MILK DRINKER'S LIFE ==
;; The language's tenet resolves to a modelling of the cotidian
;; endeavors which the enjoyment of milk --- and its natural
;; epiphenoma --- transpire upon a consumer's actions.
;; 
;; From this metaphor are borrowed the linguistic elements that, as its
;; commands, serve in a program's construction.
;; 
;; == CELLS CONTRIBUTE THE CURRENCY OF COMPUTATIONS ==
;; Every BAIMS program operates on an infinite contingency of
;; integer-valued cells, or accumulators, that store non-negative
;; objects in the range [0, 500], answering to a pointer for the
;; currently active entity's designation.
;; 
;; 
;; Architecture
;; ============
;; The program memory's design ostends a theoretical infinite tally of
;; cells, nevened also accumulators or "stomachs", each such a scalar
;; integer's salvatory, the gamut of which occupies the closed marches
;; [0, 500].
;; 
;; Any accumulator assumes the default value of zero (0), permitting
;; incrementations and decrementations by appropriate instructions.
;; 
;; A cell pointer selects at any instant the active accumulator,
;; apportioning to it the amenability to indagations and modifications.
;; 
;; 
;; Data Types
;; ==========
;; A bivious partitioning governs BAIMS's type system, manifesting in
;; the non-negative integers' acquisition of a paravant significance,
;; whereas ASCII characters operate in the rather paravail communication
;; contexts.
;; 
;; 
;; Instructions
;; ============
;; The BAIMS programming language's operational department embraces an
;; account of sixteen participants, the perimeter of the same amplects
;; incrementing, decrementing, input, output, and basic control flow
;; mechanism.
;; 
;; == OVERVIEW ==
;; The sixteen operations whose coefficacy originates the BAIMS
;; instruction set shall be a cursory presentation's material:
;; 
;;   ------------------------------------------------------------------
;;   Command      | Causatum
;;   -------------+----------------------------------------------------
;;   sip          | Increments the current accumulator by the value 1.
;;                | An error of the type "MilkOverloadError" is
;;                | signaled if the accumulator exceeds the upper march
;;                | of 500.
;;   ..................................................................
;;   drink        | Increments the current accumulator by the value 6.
;;                | An error of the type "MilkOverloadError" is
;;                | signaled if the accumulator exceeds the upper march
;;                | of 500.
;;   ..................................................................
;;   chug         | Increments the current accumulator by the value 33.
;;                | An error of the type "MilkOverloadError" is
;;                | signaled if the accumulator exceeds the upper march
;;                | of 500.
;;   ..................................................................
;;   fart         | Decrements the current accumulator by the value 1.
;;                | An error of the type "MilkUnderLoadError" is
;;                | signaled if the accumulator descends below the
;;                | lower march of zero (0).
;;   ..................................................................
;;   toilet       | Decrements the current accumulator by the value 6.
;;                | An error of the type "MilkUnderLoadError" is
;;                | signaled if the accumulator descends below the
;;                | lower march of zero (0).
;;   ..................................................................
;;   diarrhea     | Decrements the current accumulator by the value 33.
;;                | An error of the type "MilkUnderLoadError" is
;;                | signaled if the accumulator descends below the
;;                | lower march of zero (0).
;;   ..................................................................
;;   replacement  | Changes to the accumulator to the right.
;;   ..................................................................
;;   revert       | Changes to the accumulator to the left.
;;   ..................................................................
;;   scream       | Prints to the standard output the character whose
;;                | ASCII code corresponds to the current accumulator's
;;                | value.
;;                | An error of the type "FatScreamError" is signaled
;;                | if the accumulator value is greater than or equal
;;                | to the value 350.
;;                | An error of the type "TooMuchScreamingError" is
;;                | signaled if the "scream" command has been invoked
;;                | more than 10,000 times during a program's
;;                | execution.
;;   ..................................................................
;;   examination  | Prints to the standard output the current
;;                | accumulator's numeric value, immediately succeeded
;;                | by the " L" label as a designator of the metric
;;                | unit "liters".
;;   ..................................................................
;;   hunger       | Queries the standard input of an ASCII character
;;                | and stores its character code in the current
;;                | accumulator.
;;                | An error of the type "NoInputError" is signaled if
;;                | either no input can be provided or the end-of-file
;;                | (EOF) sentinel has been delivered.
;;   ..................................................................
;;   quantity     | Queries the standard input for an integer number
;;                | greater than or equal to zero and stores the same
;;                | in the current accumulator.
;;                | An error of the type "NoInputError" is signaled if
;;                | either no input can be provided or the end-of-file
;;                | (EOF) sentinel has been delivered.
;;                | An error of the type "MilkUnderloadError" is
;;                | signaled if the response provides a negative
;;                | integer.
;;                | An error of the type "MilkOverloadError" is
;;                | signaled if the response provides an integer
;;                | greater than the value 500.
;;   ..................................................................
;;   insulin      | If the current accumulator's value equals zero (0),
;;                | moves the instruction pointer (IP) forward to
;;                | position immediately following the matching
;;                | "glucagon" instruction. Aliter, progresses as
;;                | usual.
;;                | An error of the type "MismatchedBracketsError"
;;                | is signaled if the "insulin" instance cannot be
;;                | matched against a corresponding "glucagon" entity.
;;   ..................................................................
;;   glucagon     | If the current accumulator's value does not equal
;;                | zero (0), moves the instruction pointer (IP) back
;;                | to the position immediately following the matching
;;                | "insulin" instruction. Aliter, progresses as usual.
;;                | An error of the type "MismatchedBracketsError"
;;                | is signaled if the "glucagon" instance cannot be
;;                | matched against a corresponding "insulin" entity.
;;   ..................................................................
;;   thought      | Starts a comment block which extends to the
;;                | "nevermind" instruction.
;;                | Comments can be nested.
;;                | An error of the type "IncorrectCommentClosureError"
;;                | is signaled if the "thought" instance cannot be
;;                | matched against a corresponding "nevermind" entity.
;;   ..................................................................
;;   nevermind    | End a comment block which has been commenced by a
;;                | "thought" instruction.
;;                | Comments can be nested.
;;                | An error of the type "IncorrectCommentClosureError"
;;                | is signaled if the "nevermind" instance cannot be
;;                | matched against a corresponding "thought" entity.
;;   ..................................................................
;;   bored        | Terminates the program immediately.
;;   ..................................................................
;;   bye          | Terminates the program immediately.
;;   .................................................................. 
;;   constipation | Terminates the program immediately.
;;   ..................................................................
;;   dehydrated   | Terminates the program immediately.
;;   ..................................................................
;;   diabetes     | Terminates the program immediately.
;;   ..................................................................
;;   full         | Terminates the program immediately.
;;   ..................................................................
;;   homework     | Terminates the program immediately.
;;   ..................................................................
;;   overdose     | Terminates the program immediately.
;;   ..................................................................
;;   studying     | Terminates the program immediately.
;;   ------------------------------------------------------------------
;; 
;; The nine termination commands may appear at any location in a
;; program, but abstain from a mandate's imposition in their presence.
;; 
;; == OPERATIONAL SYMMETRIES ==
;; A conspicuous symmetry's governance ostends in a subset of the
;; available instructions, whose equiparance shall be a display's
;; cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command 1   | Command 2   | Effect of command1/command2
;;   ------------+-------------+---------------------------------------
;;   sip         | fart        | Change accumulator by +1/-1.
;;   ..................................................................
;;   drink       | toilet      | Change accumulator by +6/-6.
;;   ..................................................................
;;   chug        | diarrhea    | Change accumulator by +33/-33.
;;   ..................................................................
;;   replacement | revert      | Move to right/left accumulator.
;;   ..................................................................
;;   scream      | examination | Output character/number.
;;   ..................................................................
;;   hunger      | quantity    | Query input as character/number.
;;   ..................................................................
;;   insulin     | glucagon    | Start/End loop.
;;   ..................................................................
;;   thought     | nevermind   | Start/End comment.
;;   ------------------------------------------------------------------
;; 
;; A rearrangement of the correspondence twains with the intended
;; causatum as the pivot amounts to the following:
;;   
;;   ------------------------------------------------------------------
;;   Intention                  | Alternatives | Command
;;   ---------------------------+--------------+-----------------------
;;                              | +1           | sip
;;   Change accumulator by ...  >--------------+-----------------------
;;                              | -1           | fart
;;   ..................................................................
;;                              | +6           | drink
;;   Change accumulator by ...  >--------------+-----------------------
;;                              | -6           | toilet
;;   ..................................................................
;;                              | +33          | chug
;;   Change accumulator by ...  >--------------+-----------------------
;;                              | -33          | diarrhea
;;   ..................................................................
;;                              | right        | replacement
;;   Move to accumulator to ... >--------------+-----------------------
;;                              | left         | revert
;;   ..................................................................
;;                              | character    | scream
;;   Output accumulator as ...  >--------------+-----------------------
;;                              | number       | examination
;;   ..................................................................
;;                              | character    | hunger
;;   Query input as ...         >--------------+-----------------------
;;                              | number       | quantity
;;   ..................................................................
;;                              | start        | insulin
;;   Mark loop ...              >--------------+-----------------------
;;                              | end          | glucagon
;;   ..................................................................
;;   Mark comment ...           | start        | thought
;;                              >--------------+-----------------------
;;                              | end          | nevermind
;;   ------------------------------------------------------------------
;; 
;; == PREDEFINED ERRORS ==
;; A predefined set of errors' dedication to particular causes permits
;; an amenability, and contingent response, to certain etiologies.
;; 
;; The following tabular exposition serves in the equiparation of the
;; concluded anomalies and their error types. However, preceding the
;; listing, a parasceuastic diorism of the utilized notation shall be
;; offered:
;; 
;;   ------------------------------------------------------------------
;;   Identifier      | Description
;;   ----------------+-------------------------------------------------
;;   stomach         | The current accumulator's value.
;;   ................|.................................................
;;   scream(stomach) | An invocation of the "scream" function that
;;                   | shall print the current accumulator's (stomach)
;;                   | value.
;;   ..................................................................
;;   screamCount     | The number of times that the "scream" function
;;                   | has hitherto been invoked during a program's
;;                   | execution.
;;   ..................................................................
;;   input           | The character or integer number returned by the
;;                   | queried standard input via a "hunger" or
;;                   | "quantity" command invocation, and which may be
;;                   | nil or the end-of-file (EOF) sentinel.
;;   ------------------------------------------------------------------
;; 
;; Proceeding from this gnarity's dation, the errors embrace:
;; 
;;   ------------------------------------------------------------------
;;   Situation            | Consequent error type
;;   ---------------------+--------------------------------------------
;;   stomach < 0          | MilkUnderloadError
;;   ..................................................................
;;   stomach > 500        | MilkOverloadError
;;   ..................................................................
;;   scream(stomach),     | FatScreamingError
;;   with stomach >= 350  |
;;   ..................................................................
;;   screamCount > 10,000 | TooMuchScreamingError
;;   ..................................................................
;;   input = nil          | NoInputError
;;   ..................................................................
;;   input = EOF          | NoInputError
;;   ..................................................................
;;   Incorrectly matched  | MismatchedBracketsError
;;   brackets             | 
;;   ..................................................................
;;   Incorrectly closed   | IncorrectCommentClosureError
;;   comment              | 
;;   ..................................................................
;;   Runtime error        | NastyRuntimeError
;;   ..................................................................
;;   Memory exhaustion    | OutOfMemoryError
;;   ..................................................................
;;   Compiler error       | CompilerError
;;   ------------------------------------------------------------------
;; 
;; Each error species reponds to an explicating message's stringency.
;; 
;; The table below shall educate about this correlation; as, however,
;; the tabular format's acquisition is defrayed in spatial impositions,
;; the texts' nimious extent frequently requires a single sentence's
;; distribution across several lines. Such divisions are designated by
;; a hyphen preceding the wrapped section; while a separate sentence
;; shall not bear this suffix.
;; 
;; Located in a forbisen's context, we peruse the epexegesis for the
;; sentence pair
;; 
;;   Problem achieved: You screamed so much you forgot how to breath.
;;   You died.
;; 
;; If spreading over several lines, the following design applies:
;; 
;;   |  Problem achieved: You screamed so
;;   |- much you forgot how to breath.
;;   |  You died.
;; 
;; Please note the hyphen's contribution to establishing a vinculum
;; betwixt the first sentence's inchoation
;; 
;;   Problem achieved: You screamed so
;; 
;; and its conclusion
;; 
;;   much you forgot how to breath.
;; 
;; The absence of this mark before
;; 
;;   You died.
;; 
;; serves to emphasize the second sentence's independence.
;; 
;; In the face of this styling, the type-message correspondences hold:
;; 
;;   ------------------------------------------------------------------
;;   Error type                   | Error message
;;   -----------------------------+------------------------------------
;;   MilkUnderloadError           |  Problem achieved: Your stomach was
;;                                |- so empty your cells gave you up.
;;                                |  You died.
;;   ..................................................................
;;   MilkOverloadError            |  Problem achieved: Your stomach was
;;                                |- so filled with milk you exploded.
;;                                |  You died.
;;   ..................................................................
;;   FatScreamingError            |  Problem achieved: The amplitudes
;;                                |- your larynx made were so fat your
;;                                |- ears shattered to injure your
;;                                |- brain.
;;                                |  You died.
;;   ..................................................................
;;   TooMuchScreamingError        |  Problem achieved: You screamed so
;;                                |- much you forgot how to breath.
;;                                |  You died.
;;   ..................................................................
;;   NoInputError                 |  Problem achieved: You didnt get
;;                                |- the milk that dad promised he was
;;                                |- getting.
;;                                |  You died.
;;   ..................................................................
;;   MismatchedBracketsError      |  Problem achieved: Your hormones
;;                                |- were screwed up and you were
;;                                |- diagnosed for diabetes type 2.
;;                                |  You died.
;;   ..................................................................
;;   IncorrectCommentClosureError |  Problem achieved: Your brain
;;                                |- stumbled on a paradox and you fell
;;                                |- into a logical black hole.
;;                                |  You died.
;;   ..................................................................
;;   NastyRuntimeError            |  Problem achieved: Your curfew was
;;                                |- so messed up you forgot how to
;;                                |- drink milk.
;;                                |  You died.
;;   ..................................................................
;;   OutOfMemoryError             |  Problem achieved: The local
;;                                |- hospital ran out of donors for
;;                                |- you.
;;                                |  You died.
;;   ..................................................................
;;   CompilerError                |  Problem achieved: I actually dont
;;                                |- know how that happened.
;;                                |  You died.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in Common Lisp employs a rather convergent
;; champarty of the lexer and parser utilities in order to beget an
;; instruction vector, the evaluation of which segues into an
;; interpreter's ambit.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-09
;; 
;; Sources:
;;   [esolang2022BAIMS]
;;   The Esolang contributors, "Boring and Idiotic milk supper", 2022
;;   URL: "https://esolangs.org/wiki/Boring_and_Idiotic_milk_supper"
;;   
;;   [steele1990cltl2ed]
;;   Guy L. Steele Jr., "Common Lisp the Language", 2nd Edition, 1990
;;   URL: "https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node333.html"
;;   Notes:
;;     - Describes the printing of conditions.
;;     - States that the condition report method will be invoked if the
;;       global variable ``*print-escape*'' resolves to ``NIL'', which
;;       transpires, for instance, in the following cases:
;;       o If the ``format'' function directive ``~A'' is employed for
;;         printing the condition; for example:
;;           (format T "~a" my-condition)
;;       o If the ``princ'' function is employed to display the
;;         condition, such as:
;;           (princ my-condition)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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
                  (typep key   key-type)
                  (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized BAIMS instruction
   variants."
  '(member
    :sip
    :drink
    :chug
    :fart
    :replacement
    :revert
    :toilet
    :diarrhea
    :scream
    :examination
    :insulin
    :glucagon
    :thought
    :nevermind
    :hunger
    :quantity
    :terminate))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of instruction names
   to representative objects in terms of a hash table that correlates
   string keys to ``instruction'' values."
  '(hash-table-of string instruction))

;;; -------------------------------------------------------

(deftype baims-program ()
  "The ``baims-program'' type defines an executable BAIMS programs as a
   vector of zero or more ``instruction'' objects."
  '(vector instruction *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines an association betwixt jump positions
   in a BAIMS instruction vector, mapping, by adminiculum of a hash
   table, the forward jump indices to that of the back jumps, and vice
   versa."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype accumulator-table ()
  "The ``accumulator-table'' type defines the program memory as a
   contingently infinite mapping of integer cell indices to integer
   values, realized as a hash table that employs for both agencies the
   ``integer'' type.
   ---
   The choice of the more liberal ``integer'' type for the entry values
   in lieu of the more pertinent unsigned integers is vindicated by the
   fact that a checking for accumulator inconsitencies may be
   facilitated through deferred application."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operatinos,
   embracing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of errors.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-baims-error (condition-name (superclass)
                              message
                              &optional (documentation ""))
  "Defines and returns a new condition type nevened by the
   CONDITION-NAME and subsumed into the SUPERCLASS, with a
   ``default-initarg'' ``:message'' that bears the MESSAGE value and an
   optional DOCUMENTATION string for the ``:documentation'' option.
   ---
   A concomitant of this diorism, a new function with the CONDITON-NAME
   based agnomination
     signal-CONDITION-NAME
   is declared, which upon its invocation signals an error of the same
   type."
  (let ((options-argument-name (gensym)))
    (declare (type symbol options-argument-name))
    `(prog1
       (define-condition ,condition-name (,superclass)
         ()
         (:default-initargs
           :message ,message)
         (:documentation ,documentation))
       
       (defun ,(intern (format NIL "SIGNAL-~:@(~a~)" condition-name))
           (&rest ,options-argument-name)
         ,(format NIL "Signals an error of the type ``~:(~a~)''."
            condition-name)
         (declare (type (list-of T) ,options-argument-name))
         (apply #'error (quote ,condition-name)
                ,options-argument-name)))))

;;; -------------------------------------------------------

(define-condition BAIMS-Error (error)
  ((message
    :initarg       :message
    :initform      (error "Missing message.")
    :reader        baims-error-message
    :type          string
    :documentation "The ``format''-compliant control string employed in
                    the communication of a elucidating notification.
                    ---
                    The ``BAIMS-Error'' report function does not intend
                    to supply any format arguments; in corollary, the
                    MESSAGE, maugre its definition as a format control,
                    should not incorporate dependencies on inputs.")
   (context
    :initarg       :context
    :initform      NIL
    :reader        baims-error-context
    :type          T
    :documentation "An optional object whose presence serves as a
                    contextualized environment for the error's
                    provenance, frequently, but not mandatorily,
                    supplying a reference to the interpreter for
                    condition handling purposes."))
  (:report
    (lambda (condition stream)
      (declare (type BAIMS-Error condition))
      (declare (type destination stream))
      (format stream
        (baims-error-message condition))))
  (:documentation
    "The ``BAIMS-Error'' condition establishes the interface for all
     error definitions in the context of a \"Boring and Idiotic milk
     supper\" program."))

;;; -------------------------------------------------------

(define-condition Accumulator-Error (BAIMS-Error)
  ()
  (:documentation
    "The ``Accumulator-Error'' condition establishes a sub-interface of
     the ``BAIMS-Error'' type inteded for the communication of anomalous
     situations in an accumulator's perimeter."))

;;; -------------------------------------------------------

(define-baims-error Milk-Underload-Error (Accumulator-Error)
  "Problem achieved: Your stomach was so empty your cells ~
   gave you up.~%~
   You died.")

;;; -------------------------------------------------------

(define-baims-error Milk-Overload-Error (Accumulator-Error)
  "Problem achieved: Your stomach was so filled with milk ~
   you exploded.~%~
   You died."
  "The ``Milk-Overload-Error'' condition serves to signal an anomaly
   concerning the accumulator's surfeiture by reaching a value over 500
   liters of milk.")

;;; -------------------------------------------------------

(define-condition Syntax-Error (BAIMS-Error)
  ()
  (:documentation
    "The abstract ``Syntax-Error'' condition institutionalizes a foundry
     for errors committed to the relationship with syntactical anomalies
     in a BAIMS program's interpretation."))

;;; -------------------------------------------------------

(define-baims-error Mismatched-Brackets-Error (Syntax-Error)
  "Problem achieved: Your hormones were screwed up and you were ~
   diagnosed for diabetes type 2.~%~
   You died."
  "The ``Mismatched-Brackets-Error'' serves in the communication of an
   anomalous circumstance involving mismatched jump/loop demarcations,
   or brackets.")

;;; -------------------------------------------------------

(define-baims-error Fat-Screaming-Error (Syntax-Error)
  "Problem achieved: The amplitudes your larynx made were so fat your ~
   ears shattered to injure your brain.~%~
   You died."
  "The ``Fat-Screaming-Error'' condition serves in apprizing about an
   attempt to output an accumulator whose value is greater than or equal
   to 350 as a character.")

;;; -------------------------------------------------------

(define-baims-error Too-Much-Screaming-Error (Syntax-Error)
  "Problem achieved: You screamed so much you forgot how to breath.~%~
   You died."
  "The ``Too-Much-Screaming-Error'' condition serves to either apprize
   about an undly amount of over 10000 character output operations, or
   communicate a stack overflow error.")

;;; -------------------------------------------------------

(define-baims-error Incorrect-Comment-Closure-Error (Syntax-Error)
  "Problem achieved: Your brain stumbled on a paradox and you fell ~
   into a logical black hole.~%~
   You died."
  "The ``Incorrect-Comment-Closure-Error'' condition serves in the
   communication of a misshapen comment termination.")

;;; -------------------------------------------------------

(define-baims-error No-Input-Error (Syntax-Error)
  "Problem achieved: You didnt get the milk that dad promised he was ~
   getting.~%~
   You died."
  "The ``No-Input-Error'' condition serves to signal an anomaly during
   an input request, appertaining to the lacuna of such or the occasion
   of an end-of-file (EOF) situation.")

;;; -------------------------------------------------------

(define-baims-error Nasty-Runtime-Error (BAIMS-Error)
  "Problem achieved: Your curfew was so messed up you forgot how to ~
   drink milk.~%~
   You died."
  "The ``Nasty-Runtime-Error'' condition serves to signal a grave
   anomaly during runtime.")

;;; -------------------------------------------------------

(define-baims-error Out-Of-Memory-Error (BAIMS-Error)
  "Problem achieved: The local hospital ran out of donors for you.~%~
   You died."
  "The ``Out-Of-Memory-Error'' condition serves to signal an anomaly
   caused by the exhaustion of the available memory.")

;;; -------------------------------------------------------

(define-baims-error Compiler-Error (BAIMS-Error)
  "Problem achieved: I actually dont know how that happened.~%~
   You died."
  "The ``Compiler-Error'' condition serves to signal an anomaly during
   the compilation stage.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized identifiers with the corresponding
   instructions.")

;;; -------------------------------------------------------

(flet ((register-identifier (name instruction)
        "Associates the NAME with the INSTRUCTION in the +IDENTIFIERS+
         table and returns no value."
        (declare (type string      name))
        (declare (type instruction instruction))
        (setf (gethash name +IDENTIFIERS+) instruction)
        (values)))
  (register-identifier "sip"          :sip)
  (register-identifier "drink"        :drink)
  (register-identifier "chug"         :chug)
  (register-identifier "fart"         :fart)
  (register-identifier "replacement"  :replacement)
  (register-identifier "revert"       :revert)
  (register-identifier "toilet"       :toilet)
  (register-identifier "diarrhea"     :diarrhea)
  (register-identifier "scream"       :scream)
  (register-identifier "examination"  :examination)
  (register-identifier "insulin"      :insulin)
  (register-identifier "glucagon"     :glucagon)
  (register-identifier "thought"      :thought)
  (register-identifier "nevermind"    :nevermind)
  (register-identifier "hunger"       :hunger)
  (register-identifier "quantity"     :quantity)
  (register-identifier "overdose"     :terminate)
  (register-identifier "dehydrated"   :terminate)
  (register-identifier "diabetes"     :terminate)
  (register-identifier "full"         :terminate)
  (register-identifier "bye"          :terminate)
  (register-identifier "constipation" :terminate)
  (register-identifier "homework"     :terminate)
  (register-identifier "bored"        :terminate)
  (register-identifier "studying"     :terminate)
  (values))

;;; -------------------------------------------------------

(defun get-instruction (name)
  "Returns the instruction corresponding with the NAME, or signals an
   error of the type ``Compiler-Error'' upon its disrespondecy."
  (declare (type string name))
  (the instruction
    (or (gethash name +IDENTIFIERS+)
        (signal-compiler-error))))

;;; -------------------------------------------------------

(defun matches-instruction-p (word expected-instruction)
  "Determines whether the WORD corresponds to the EXPECTED-INSTRUCTION,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string      word))
  (declare (type instruction expected-instruction))
  (multiple-value-bind (instruction-for-word contains-word-p)
      (gethash word +IDENTIFIERS+)
    (declare (type (or null instruction) instruction-for-word))
    (declare (type T                     contains-word-p))
    (the boolean
      (not (null
        (and contains-word-p
             (eq instruction-for-word expected-instruction)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word,
   demarcated by a whitespace boundary, and returns two values:
     (1) The consumed word as a string.
     (2) The position in the SOURCE immediately following the desinent
         character of the consumed word."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (word (make-string-output-stream))
      (declare (type string-stream word))
      (loop
        for position of-type fixnum from start below (length source)
        until
          (whitespace-character-p (char source position))
        do
          (write-char (char source position) word)
        finally
          (return
            (values
              (get-output-stream-string word)
              position))))))

;;; -------------------------------------------------------

(defun read-instruction (source start)
  "Proceeding from the START position into the SOURCE, reads a word,
   demarcated by a whitespace boundary, and returns two values:
     (1) The ``instruction'' associated with the consumed word.
     (2) The position in the SOURCE immediately following the desinent
         character of the consumed word.
   ---
   If no instruction name matches the consumed token, an error of the
   type ``Compiler-Error'' is signaled."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values instruction fixnum)
    (with-open-stream (word (make-string-output-stream))
      (declare (type string-stream word))
      (loop
        for position of-type fixnum from start below (length source)
        until
          (whitespace-character-p (char source position))
        do
          (write-char (char source position) word)
        finally
          (return
            (values
              (get-instruction
                (get-output-stream-string word))
              position))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position in the SOURCE, skips a sequence of
   zero or more accolent whitespaces and returns the position
   immediately succeeding the desinent of these whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun skip-comment (source start)
  "Proceeding from the START position into the SOURCE, and expecting the
   same to have been moved past a comment's commencing keyword
   \"thought\", skips a contingently nested comment block and returns
   the position into the SOURCE immediately following the traversed
   section.
   ---
   An error of the type ``Incorrect-Comment-Closure-Error'' is signaled
   if the comments are not properly demarcated."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position start)
        (nesting  1))
    (declare (type fixnum position))
    (declare (type fixnum nesting))
    (the (or null fixnum)
      (loop
        do    (setf position (skip-whitespaces source position))
        while (< position (length source))
        do
          (multiple-value-bind (word new-position)
              (read-word source position)
            (declare (type string word))
            (declare (type fixnum new-position))
            
            (setf position new-position)
            
            (cond
              ;; End of comment?
              ((matches-instruction-p word :nevermind)
                (decf nesting)
                (cond
                  ;; All comment blocks closed?
                  ((zerop nesting)
                    (loop-finish))
                  ;; Too many comment terminators encountered?
                  ((minusp nesting)
                    (signal-incorrect-comment-closure-error))
                  ;; Continue search for comment terminators.
                  (T
                    NIL)))
              
              ;; Start of a new comment?
              ((matches-instruction-p word :thought)
                (incf nesting))
              
              ;; Any other content is ignored.
              (T
                NIL)))
        finally
          (if (zerop nesting)
            (return position)
            (signal-incorrect-comment-closure-error))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts and returns from the piece of \"Boring and Idiotic milk
   supper\" source code a one-dimensional simple array of instructions."
  (declare (type string code))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((accept-instruction (instruction new-position)
            "Updates the POSITION cursor to the NEW-POSITION and returns
             the INSTRUCTION without modifications."
            (declare (type instruction instruction))
            (declare (type fixnum      new-position))
            (setf position new-position)
            (the instruction instruction)))
      (the baims-program
        (coerce
          (loop
            with instruction
              of-type (or null instruction) = NIL
            do
              (setf position (skip-whitespaces code position))
            while
              (< position (length code))
            do
              (setf instruction
                (multiple-value-call #'accept-instruction
                  (read-instruction code position)))
            if (eq instruction :thought) do
              (setf position (skip-comment code position))
            else if (eq instruction :nevermind) do
              (signal-incorrect-comment-closure-error)
            else
              collect instruction)
          '(simple-array instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0   500) +STOMACH-CAPACITY+))
(declaim (type (integer 0   349) +MAXIMUM-SCREAM-AMPLITUDE+))
(declaim (type (integer 0 10000) +MAXIMUM-NUMBER-OF-SCREAMS+))

;;; -------------------------------------------------------

(defparameter +STOMACH-CAPACITY+ 500
  "Determines the maximum capacity of the stomach, which corresponds to
   the maximum permissive value in an accumulator.")

(defparameter +MAXIMUM-SCREAM-AMPLITUDE+ 349
  "Determines the maximum accumulator value that can be utilized during
   a \"scream\", that is, while displaying the corresponding ASCII
   character.")

(defparameter +MAXIMUM-NUMBER-OF-SCREAMS+ 10000
  "Determines the maximum number of times the \"scream\" command's
   invocation is subject to homologation, ere an error transpires.")

;;; -------------------------------------------------------

(defun build-jump-table (instructions)
  "Builds and returns the jump table for the INSTRUCTIONS, associating
   each forward jump instruction's position in the sequence with the
   correlated back jump instruction's location, and vice versa."
  (declare (type baims-program instructions))
  (let ((jump-table        (make-hash-table :test #'eql))
        (jump-start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-start-points))
    (loop
      for instruction of-type instruction across instructions
      and position    of-type fixnum      from   0
      do
        (case instruction
          (:insulin
            (push position jump-start-points))
          (:glucagon
            (if jump-start-points
              (let ((start-point (pop jump-start-points))
                    (end-point   position))
                (declare (type fixnum start-point))
                (declare (type fixnum end-point))
                (setf (gethash start-point jump-table) end-point)
                (setf (gethash end-point   jump-table) start-point))
              (signal-mismatched-brackets-error)))
          (otherwise
            NIL))
      finally
        (when jump-start-points
          (signal-mismatched-brackets-error)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defstruct (Interpreter
  (:constructor make-interpreter
    (instructions
     &aux (jump-table (build-jump-table instructions)))))
  "The ``Interpreter'' class encapsulates the faculty and state of
   accompassing the evaluation of a \"Boring and Idiotic milk supper\"
   program's instructions."
  (instructions (error "Missing program.")    :type baims-program)
  (ip           0                             :type fixnum)
  (jump-table   (error "Missing jump table.") :type jump-table)
  (accumulators (make-hash-table :test #'eql) :type accumulator-table)
  (accumulator-pointer 0                      :type integer)
  (number-of-screams   0                      :type (integer 0 *)))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer forward by one step and
   returns no value."
  (declare (type Interpreter interpreter))
  (incf (interpreter-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun jump-to-opposite-boundary (interpreter)
  "Expecting its instruction pointer (IP) to reside at a jump point,
   relocates the INTERPRETER's IP to the opposite jump point and returns
   no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (or
      (gethash
        (interpreter-ip         interpreter)
        (interpreter-jump-table interpreter))
      (signal-nasty-runtime-error :context interpreter)))
  (values))

;;; -------------------------------------------------------

(defun current-instruction (interpreter)
  "Returns the instruction located at the INTERPRETER's instruction
   pointer (IP), or signals an error of the type ``Nasty-Runtime-Error''
   upon its violation of the program's boundaries."
  (declare (type Interpreter interpreter))
  (the instruction
    (if (array-in-bounds-p
          (interpreter-instructions interpreter)
          (interpreter-ip           interpreter))
      (aref
        (interpreter-instructions interpreter)
        (interpreter-ip           interpreter))
      (signal-nasty-runtime-error :context interpreter))))

;;; -------------------------------------------------------

(defun terminate-program (interpreter)
  "Designates the INTEPRETER's program as terminated by relocating its
   instruction pointer (IP) beyond the desinent location in its
   instruction s equence and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (length (interpreter-instructions interpreter)))
  (values))

;;; -------------------------------------------------------

(defun instructions-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's processing activity has ceased
   as a consequence of its instruction pointer's (IP) traversal beyond
   the instruction sequence's boundaries, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (>= (interpreter-ip interpreter)
          (length
            (interpreter-instructions interpreter)))))))

;;; -------------------------------------------------------

(defun current-accumulator (interpreter)
  "Returns the current INTERPRETER accumulator's value."
  (declare (type Interpreter interpreter))
  (the integer
    (gethash
      (interpreter-accumulator-pointer interpreter)
      (interpreter-accumulators        interpreter)
      0)))

;;; -------------------------------------------------------

(defun (setf current-accumulator) (new-value interpreter)
  "Stores the NEW-VALUE into the INTERPRETER's current accumulator and
   returns no value."
  (declare (type integer     new-value))
  (declare (type Interpreter interpreter))
  (setf
    (gethash
      (interpreter-accumulator-pointer interpreter)
      (interpreter-accumulators        interpreter)
      0)
    new-value)
  (values))

;;; -------------------------------------------------------

(defun check-accumulator (interpreter)
  "Determines the validity of the INTERPRETER's current accumulator,
   on confirmation returning no value; aliter an appropriate error is
   signaled."
  (declare (type Interpreter interpreter))
  (cond
    ((minusp (current-accumulator interpreter))
      (signal-milk-underload-error :context interpreter))
    ((> (current-accumulator interpreter) +STOMACH-CAPACITY+)
      (signal-milk-overload-error :context interpreter))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun check-scream (interpreter)
  "Determines the feasability of a \"scream\" instruction invocation's
   request, indagating the maximum number of screams issued so far and
   the current accumulator's value, on confirmation returning no value,
   otherwise signaling an appropriate error."
  (declare (type Interpreter interpreter))
  (cond
    ((> (interpreter-number-of-screams interpreter)
        +MAXIMUM-NUMBER-OF-SCREAMS+)
      (signal-too-much-screaming-error :context interpreter))
    ((> (current-accumulator interpreter)
        +MAXIMUM-SCREAM-AMPLITUDE+)
      (signal-fat-screaming-error :context interpreter))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun process-instructions (interpreter)
  "Processes the instructions in the INTERPRETER's context and returns
   no value.
   ---
   An approximate patration anenst the error handling's completeness has
   been induced, with a certain subset's compliance with the entelechy
   to restart a process if failed. Forbisens of such leniency constitute
   the ``Milk-Overload-Error'', ``Milk-Underload-Error'' and
   ``No-Input-Error'' specimens, which are concerned with nimiety or
   niggardliness in the current accumulator's gauge or the lacuna of
   data provision."
  (declare (type Interpreter interpreter))
  
  (handler-case
    (loop until (instructions-exhausted-p interpreter) do
      (case (current-instruction interpreter)
        (:sip
          (incf (current-accumulator interpreter) 1)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:drink
          (incf (current-accumulator interpreter) 6)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:chug
          (incf (current-accumulator interpreter) 33)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:fart
          (decf (current-accumulator interpreter) 1)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:toilet
          (decf (current-accumulator interpreter) 6)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:diarrhea
          (decf (current-accumulator interpreter) 33)
          (check-accumulator interpreter)
          (advance-ip interpreter))
        
        (:replacement
          (incf (interpreter-accumulator-pointer interpreter))
          (advance-ip interpreter))
        
        (:revert
          (decf (interpreter-accumulator-pointer interpreter))
          (advance-ip interpreter))
        
        (:scream
          (check-scream interpreter)
          (format T "~c" (code-char (current-accumulator interpreter)))
          (advance-ip interpreter))
        
        (:examination
          (format T "~d L" (current-accumulator interpreter))
          (advance-ip interpreter))
        
        (:hunger
          (format T "~&Please input a character: ")
          (let ((input (read-char *standard-input* NIL NIL)))
            (declare (type (or null character) input))
            (clear-input *standard-input*)
            (if input
              (setf (current-accumulator interpreter)
                    (char-code input))
              (signal-no-input-error :context interpreter)))
          (advance-ip interpreter))
        
        (:quantity
          (format T "~&Please input an integer: ")
          (let ((input (read-line *standard-input* NIL NIL)))
            (declare (type (or null string) input))
            (clear-input *standard-input*)
            (cond
              (input
                (let ((numeric-input
                        (ignore-errors
                          (parse-integer input))))
                  (declare (type (or null integer) numeric-input))
                  (if numeric-input
                    (setf (current-accumulator interpreter)
                          numeric-input)
                    (signal-nasty-runtime-error)))
                (check-accumulator interpreter))
              (T
                (signal-no-input-error :context interpreter))))
          (advance-ip interpreter))
        
        (:insulin
          (when (zerop (current-accumulator interpreter))
            (jump-to-opposite-boundary interpreter))
          (advance-ip interpreter))
        
        (:glucagon
          (unless (zerop (current-accumulator interpreter))
            (jump-to-opposite-boundary interpreter))
          (advance-ip interpreter))
        
        (:terminate
          (terminate-program interpreter))
        
        (otherwise
          (signal-nasty-runtime-error :context interpreter))))
    
    (Compiler-Error (c-error)
      (declare (type Compiler-Error c-error))
      (format T "~a" c-error)
      (terminate-program interpreter))
    
    (Incorrect-Comment-Closure-Error (icc-error)
      (declare (type Incorrect-Comment-Closure-Error icc-error))
      (format T "~a" icc-error)
      (terminate-program interpreter))
    
    (Milk-Overload-Error (mo-error)
      (declare (type Milk-Overload-Error mo-error))
      (format T "~a" mo-error)
      (setf (current-accumulator interpreter)
            +STOMACH-CAPACITY+)
      (case (current-instruction interpreter)
        ((:hunger :quantity)
          (process-instructions interpreter))
        (otherwise
          (terminate-program interpreter))))
    
    (Milk-Underload-Error (mu-error)
      (declare (type Milk-Underload-Error mu-error))
      (format T "~a" mu-error)
      (setf (current-accumulator interpreter) 0)
      (case (current-instruction interpreter)
        ((:hunger :quantity)
          (process-instructions interpreter))
        (otherwise
          (terminate-program interpreter))))
    
    (Mismatched-Brackets-Error (mb-error)
      (declare (type Mismatched-Brackets-Error mb-error))
      (format T "~a" mb-error)
      (terminate-program interpreter))
    
    (No-Input-Error (ni-error)
      (declare (type No-Input-Error ni-error))
      (format T "~a" ni-error)
      (process-instructions interpreter))
    
    (Out-Of-Memory-Error (oom-error)
      (declare (type Out-Of-Memory-Error oom-error))
      (format T "~a" oom-error)
      (terminate-program interpreter))
    
    (error (generic-error)
      (declare (type error generic-error))
      (format T "~a" generic-error)))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-BAIMS (code)
  "Interprets the piece of \"Boring and Idiotic milk supper\" source
   code and returns no value."
  (declare (type string code))
  (process-instructions
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hello world".
(interpret-BAIMS
  "chug chug chug drink fart scream fart fart fart scream drink sip scream scream sip sip sip scream
diarrhea diarrhea diarrhea drink drink drink sip sip scream toilet toilet toilet fart fart chug
chug chug drink sip sip scream toilet fart fart scream sip sip sip scream toilet scream toilet
fart fart scream overdose")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-BAIMS
  "hunger insulin scream hunger glucagon overdose")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-BAIMS
  "quantity
   insulin
     replacement
     chug drink drink drink fart fart scream
     diarrhea sip sip toilet toilet toilet
     revert
   glucagon
   replacement 
   chug drink drink drink fart fart fart scream")

;;; -------------------------------------------------------

;; Adds two non-negative integer inputs.
(interpret-BAIMS
  "quantity replacement quantity revert
   insulin
     replacement sip revert fart
   glucagon
   replacement examination 
   bored")

;;; -------------------------------------------------------

;; Example for a restarting error: If the user enters a negative
;; integer, the processes of query and checking repeats.
(interpret-BAIMS "quantity examination")

;;; -------------------------------------------------------

;; Example for an ultimately failing error: The reduction of the
;; accumulator below zero (0) cannot be remedied, thus the program
;; terminates immediately without an attempted repetition.
(interpret-BAIMS "toilet")
