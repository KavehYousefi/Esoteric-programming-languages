;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "AntiDupCall", invented by the Esolang user "Orisphera" and
;; presented on January 5th, 2023, its haecceity's commorancy that of
;; an integer stack's data castaldy in champarty with a rather dioristic
;; species of iterance construct. 
;; 
;; 
;; Concept
;; =======
;; The AntiDupCall programming language is edified upon an
;; integer-valued stack's firmament as the program memory, whose basic
;; arithmetic capabilities entertain the compernage to a rather
;; convolute conception of the sole iterance mechanism.
;; 
;; == THE STACK: SIGNED INTEGER NUMBERS GREATER THAN OR EQUAL TO -1 ==
;; The aefauld agent ordained to the data castaldy's provision is
;; realized in a stack whose capacity does not ostend any natural
;; limit's tholance, and whose elements' membership subscribes to
;; integer numbers greater than or equal to -1, but destitute of an
;; imposition along the spectrum's overthwart laterality, which, as a
;; corollay, serves in the accommodation of the integeral interval of
;; [-1, +infinity].
;; 
;; == THE STACK MUST BE EMPTY AT THE PROGRAM'S CESSATION ==
;; A kenspeckle proprium of this language, a program's termination
;; involves a desinent antecedent inwith whose bailiwick the stack is
;; probed for its exhaustion. If this vacancy fails an attest, an
;; abortive error of the type "NonEmptyStackError" will be signaled.
;; 
;; == THE LOOP: ITERANCE BASED ON A PRIVATE VALUE ==
;; The diorisms' woning inwith the sole iterance construct serves as the
;; provenance of the separate loop specimens' intricacy in constitution
;; and its consequent segregation into strata:
;; 
;;   (1) CURRENT LOOP VALUE ASSIGNMENT:
;;       The loop's current value is set to its "base value".
;;   
;;   (2) LOOP CONTINUANCE CRITERIA ASSESSMENT:
;;       The justification of a first or subsequent execution of the
;;       body form's is preceded by a twifold indagation into the
;;       stack, expecting the same to contain at least one element, and
;;       the loop's current value, imposing its state to be at least as
;;       large as the base value.
;;   
;;   (3) CURRENT LOOP VALUE MODULATION:
;;       The loop's current value is incremented by the top stack
;;       element, upon thilk is exercised a concomitant removal from
;;       the entailing salvatory.
;;   
;;   (4) LOOP BODY EXECUTION:
;;       The loop body statements enjoy their execution.
;; 
;; The subsequent sections' cynosure shall be realized in the just
;; produced tesseratomy's exposition.
;; 
;; == (1) CURRENT LOOP VALUE ASSIGNEMNT ==
;; Every loop's endowment encompasses a fixed "base value", an integral
;; datum whose concrete choice accompts for the interpreter
;; implementation's own deliberations, with a value of one (1) reckoned
;; a sensible stipulation.
;; 
;; A close compernage to this, a mutable current value partakes of a
;; more active agency, the latter's assignment to the former's state
;; represents a parasceuastic act at the entrance. This transaction
;; exclusively occurs upon the complex' activation, but not as a
;; sequela to the ensuing cycles of its body's execution.
;; 
;; == (2) LOOP CONTINUANCE CRITERION ASSESSMENT ==
;; The current value's configuration limns a prevenience to the
;; veridically iterative ceremonials, among these the continuation
;; antecedent's docimasy provides the incipient member. During its
;; course, a twissel of conditions, whose confirmation must be a mutual
;; fact for a conjoined perpetuation, experience a fathom:
;; 
;;   (a) The stack must not be empty.
;;   
;;   (b) The loop's current value must be greater than or equal to its
;;       base value.
;; 
;; A failure at the ingress to the loop or ere any fresh cycle's
;; activation inflicts the iteration with an immediate abortion.
;; 
;; == (3) CURRENT LOOP VALUE MODULATION ==
;; Ensuing from the aboon covenance's satisfaction, preceding any
;; iteration effort, a second prefatorial exercise, in this encheson
;; unconditional in its haecceity, prepares the body statements: The
;; stack's top element, by the aforementioned conditions' indagation
;; already designated with the mark of sickerness to abstain from a
;; vacancy, is removed and its value added to the loop's current value.
;; 
;; == (4) LOOP BODY EXECUTION ==
;; Proceeding from the continuation stipulations' satisfaction (see
;; step -> (2)) and the current loop value's modulation (point -> (3)),
;; the ultimity of the ensconced instructions' execution ensues, ere
;; an iterum cycle's trial, commencing with the step -> (2), is
;; entertained.
;; 
;; == PARLECUE OF THE LOOP PRINCIPLE IN PSEUDOCODE ==
;; The following encapsulation of an iterance construct's data shall
;; furnish an adminiculum to an eath handling:
;; 
;;   record Loop
;;     baseValue    : integer         = 1
;;     currentValue : integer         = baseValue
;;     body         : statement[0..*] = empty sequence
;;   end record
;; 
;; Ensuing from the aboon diorism, the iteration construct, instigated
;; by a "[" token's consumption, and demarcated in its ensconced code
;; tmema via a matching "]" symbol, ostends a lealty to this principle:
;; 
;;   loop.currentValue <- loop.baseValue
;; 
;;   while ((not stack.isEmpty ()) and
;;          (loop.currentValue >= loop.baseValue) do
;;     let loopValueModifier <- stack.pop()
;;     
;;     loop.currentValue <- loop.currentValue + loopValueModifier
;;     
;;     { Execute loop body. }
;;     
;;   end while
;; 
;; 
;; Instructions
;; ============
;; AntiDupCall's instruction set's governance registers an edification
;; upon a tesseratomy of members, whose bailiwicks do not rise aboon
;; rudiments of arithmetics and an aefauld iterance construction.
;; 
;; == OVERVIEW ==
;; The following tabular apercu's vouchsafement shall be that of a
;; requisite gnarity's mete's conveyance concerning the language's
;; operative warklooms:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   <       | Pushes the value -1 onto the stack.
;;   ..................................................................
;;   +       | If the stack is not empty, increments its top element's
;;           | value by the amount one (1); otherwise accompasses no
;;           | action.
;;   ..................................................................
;;   [       | Demarcates the start of a loop.
;;           |---------------------------------------------------------
;;           | A slight mickleness limns in this iterance construct's
;;           | dioristic operations, thilk shall be the alow pseudocode
;;           | tmema's cynosure:
;;           | 
;;           |   loop.currentValue <- loop.baseValue
;;           |   
;;           |   while ((not stack.isEmpty()) and
;;           |          (loop.currentValue >= loop.baseValue)) do
;;           |     let loopValueModifier <- stack.pop()
;;           |     
;;           |     loop.currentValue <- loop.currentValue
;;           |                          + loopValueModifier
;;           |     
;;           |     execute loop.body until matching "]" token
;;           |   end while
;;   ..................................................................
;;   ]       | Demarcates the end of the loop inchoated in the matching
;;           | "[" instruction, which please consign to your personal
;;           | conspectuity.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes a realization in the
;; programming language Common Lisp, the operative efforts' investments
;; such to be exercised immediately on the AntiDupCall's source code.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-10-20
;; 
;; Sources:
;;   [esolang2025AntiDupCall]
;;   The Esolang contributors, "AntiDupCall", June 15th, 2025
;;   URL: "https://esolangs.org/wiki/AntiDupCall"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun construe-as-a-boolean-value (object)
  "Construes the OBJECT in its agency as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, responding for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, returns ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination is derived from the
   TYPE-NAME, the formal arguments constituting a verbatim appropriation
   from the LAMBDA-LIST, stevens the object to probe for its compliance
   with the underlying predicate via the CANDIDATE-NAME, evaluates the
   BODY forms, and construes the desinent BODY form's primary return
   value as the docimasy's conclusion, a \"generalized boolean\" value
   of \"true\" furnishing a compliance's attest, while a \"false\"
   response bears a rejection's signification.
   ---
   If the first BODY form resolves to a string object, the same is
   imputed to specify the derived type's documentation string, and is
   subsequently reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*))
  "The ``list-of'' type defines a list whose componency enumerates a
   membership comprehending zero or more elements of the ELEMENT-TYPE,
   for whom the default configuration is assigned in the generic
   sentinel ``*''."
  (flet ((element-is-of-the-expected-type-p (probed-element)
          "Determines whether the PROBED-ELEMENT complies with the
           ELEMENT-TYPE, returning on confirmation a ``boolean''
           value of ``T'', otherwise ``NIL''."
          (declare (type T probed-element))
          (the boolean
            (construe-as-a-boolean-value
              (typep probed-element element-type)))))
    (and
      (listp candidate)
      (every #'element-is-of-the-expected-type-p
        (the list candidate)))))

;;; -------------------------------------------------------

(define-a-predicated-type hash-table-of
    (candidate
     &optional (key-type   '*)
               (value-type '*))
  "The ``hash-table'' type defines a hash table whose componency is
   edified upon an accompt of zero or more entries, each such a twissel
   comprehending a key of the KEY-TYPE and an associated value which
   subscribes to the VALUE-TYPE, for both is assigned the generic
   sentinel ``*'' as the default configuration."
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

(deftype loop-table ()
  "The ``loop-table'' type defines a unidirectional association betwixt
   a loop start (\"[\") or end (\"]\") instruction and a representative
   object, realized as a hash table which maps the zero-based index of
   the instruction in the AntiDupCall program code to a ``ADC-Loop''
   instances.
   ---
   Please heed that a connected loop start and end instruction, albeit
   located at different positions in the source code, always refers to
   the same ``ADC-Loop'', and thus the identical hash table entry
   value."
  '(hash-table-of fixnum ADC-Loop))

;;; -------------------------------------------------------

(deftype integer-stack ()
  "The ``integer-stack'' type defines a list-based stack whose elements
   subsume into a subset of the signed integer type imposed with the
   inclusive minimum of -1, but destitute of any upper bourne, by which
   diorism their occupied interval conflates with [-1, +infinity]."
  '(list-of (integer -1 *)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference assumed by whom amplects, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype prefix-parameters ()
  "The ``prefix-parameters'' type defines the concept of prefix
   parameters to a ``format'' directive in the mold of a list
   comprehending zero or more characters or integer numbers."
  '(list-of (or character integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "ADC-Loop".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (ADC-Loop
  (:constructor prepare-an-adc-loop (start-point)))
  "The ``ADC-Loop'' class serves in the encapsulation of an AntiDupCall
   iterance complex' requisite data, including in particular the
   zero-based positions of the matching loop start and end instructions
   in the underlying source code, the base and current value, as well as
   a Boolean flag as a signification of as the iteration's current
   execution."
  (start-point        NIL :type (or null fixnum) :read-only T)
  (end-point          NIL :type (or null fixnum) :read-only NIL)
  (base-value           1 :type (integer * *)    :read-only T)
  (current-value        0 :type (integer * *)    :read-only NIL)
  (has-been-entered-p NIL :type boolean          :read-only NIL))

;;; -------------------------------------------------------

(defun reset-adc-loop (loop)
  "Resets the LOOP's current value to the base state and returns no
   value."
  (declare (type ADC-Loop loop))
  (psetf
    (adc-loop-current-value      loop) (adc-loop-base-value loop)
    (adc-loop-has-been-entered-p loop) NIL)
  (values))

;;; -------------------------------------------------------

(defun adc-loop-value-permits-a-continuance-p (loop)
  "Determines whether the LOOP's current value homologates an iterum
   execution of the LOOP, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type ADC-Loop loop))
  (the boolean
    (construe-as-a-boolean-value
      (>= (adc-loop-current-value loop)
          (adc-loop-base-value    loop)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of loop table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-loop-table ()
  "Creates and returns a fresh, initially vacant ``loop-table''."
  (the loop-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun build-the-loop-table-for (source)
  "Creates and returns a fresh ``loop-table'' whose iteration start and
   end points are mapped to their common ``ADC-Loop'' representations."
  (declare (type simple-string source))
  (let ((loop-table (prepare-an-empty-loop-table))
        (loop-stack NIL))
    (declare (type loop-table  loop-table))
    (declare (type (list-of T) loop-stack))
    (loop
      for current-token    of-type character across source
      and current-position of-type fixnum    from   0 by 1
      
      if (char= current-token #\[) do
        (push (prepare-an-adc-loop current-position) loop-stack)
        (setf (gethash current-position loop-table)
              (first loop-stack))
      else if (char= current-token #\]) do
        (if loop-stack
          (let ((current-loop (pop loop-stack)))
            (declare (type ADC-Loop current-loop))
            (psetf
              (adc-loop-end-point current-loop)     current-position
              (gethash current-position loop-table) current-loop))
          (error "Unmatched loop end point (\"]\") at position ~d."
            current-position))
      end
      
      finally
        (when loop-stack
          (error "Unmatched loop start point~p (\"[\") at ~
                  position~:p ~{~d~^, ~}."
            (length loop-stack)
            (mapcar #'adc-loop-start-point
              (nreverse loop-stack)))))
    
    (the loop-table loop-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer stack operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-the-integer-stack (stack &optional (destination T))
  "Outputs the content of the integer STACK to the DESTINATION,
   returning for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh string comprehending
   the result."
  (declare (type integer-stack stack))
  (declare (type destination   destination))
  (the (or null string)
    (format destination "[top> ~{~d~^, ~} <bottom]" stack)))

;;; -------------------------------------------------------

(defun stack-directive (destination
                        format-argument
                        colon-modifier-is-specified-p
                        at-sign-modifier-is-specified-p
                        &rest prefix-parameters)
  "Prints the FORMAT-ARGUMENT, thilk must represent an ``integer-stack''
   object, to the DESTINATION, requiring both the
   COLON-MODIFIER-IS-SPECIFIED-P and AT-SIGN-MODIFIER-IS-SUPPLIED-P
   flags to resolve to a \"generalized boolean\" value of \"false\",
   and the PREFIX-PARAMETERS to establish an empty list, and returns no
   value.
   ---
   This operation's concinnity with the custom ``format'' directive
   interface, communicated via the syntax \"~/stack-directive/\",
   vouches for a successful integration into the selfsame context."
  (declare (type destination       destination))
  (declare (type integer-stack     format-argument))
  (declare (type T                 colon-modifier-is-specified-p))
  (declare (type T                 at-sign-modifier-is-specified-p))
  (declare (type prefix-parameters prefix-parameters))
  (cond
    (colon-modifier-is-specified-p
      (error "The stack formatting directive does not homologate the ~
              colon modifier \":\"."))
    (at-sign-modifier-is-specified-p
      (error "The stack formatting directive does not homologate the ~
              at-sign modifier \"@\"."))
    (prefix-parameters
      (error "The stack formatting directive does not homologate ~
              prefix-parameters."))
    (T
      (print-the-integer-stack format-argument destination)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Non-Empty-Stack-Error (error)
  ((offending-stack
    :initarg       :offending-stack
    :initform      (error "Missing offending stack.")
    :reader        non-empty-stack-error-offending-stack
    :type          integer-stack
    :documentation "The memory stack whose carency in a perfect ullage
                    served in this anomaly's infliction."))
  (:report
    (lambda (condition stream)
      (declare (type Non-Empty-Stack-Error condition))
      (declare (type destination           stream))
      (format stream "The program has terminated without the stack ~
                      being empty.~%~
                      The following elements reside therein: ~
                      ~/stack-directive/."
        (non-empty-stack-error-offending-stack condition))))
  (:documentation
    "The ``Non-Empty-Stack-Error'' condition type serves in the
     communication of an anomalous circumstance whose etiology emerges
     from the attempt to terminate an AntiDupCall program with a
     concomitant failure to clear the memory stack in the
     prevenience."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *program*))
(declaim (type fixnum        *ip*))
(declaim (type character     *current-token*))
(declaim (type boolean       *program-is-completed-p*))
(declaim (type loop-table    *loops*))
(declaim (type integer-stack *stack*))
(declaim (type boolean       *stack-is-empty-p*))
(declaim (type boolean       *stack-shall-be-printed-p*))

;;; -------------------------------------------------------

(defparameter *program* ""
  "The piece of AntiDupCall source code to execute.")

(defparameter *ip* 0
  "The current instruction pointer (IP) position into the executed
   *PROGRAM*.")

(define-symbol-macro *current-token*
  (the character
    (schar *program* *ip*)))

(define-symbol-macro *program-is-completed-p*
  (the boolean
    (not
      (array-in-bounds-p *program* *ip*))))

(defparameter *loops*
  (prepare-an-empty-loop-table)
  "Associates the loop start and end positions into the *PROGRAM* with
   their representative ``ADC-Loop'' objects.")

(defparameter *stack* NIL
  "The program memory as a stack of integer numbers.")

(define-symbol-macro *stack-is-empty-p*
  (the boolean
    (null *stack*)))

(defparameter *stack-shall-be-printed-p* T
  "A Boolean flag which determines whether the *STACK* shall be printed
   as an epiphenomenal reaction to a character's processing.")

;;; -------------------------------------------------------

(defun initialize-the-program (program)
  "Initializes the interpreter for its deployment with the AntiDupCall
   PROGRAM and returns no value."
  (declare (type string program))
  (psetf
    *program* (coerce program 'simple-string)
    *ip*      0
    *stack*   NIL)
  (setf *loops*
    (build-the-loop-table-for *program*))
  (values))

;;; -------------------------------------------------------

(defun get-the-current-adc-loop ()
  "Returns the ``ADC-Loop'' object associated with the current
   instruction pointer (*IP*) location; or signals an error of an
   unspecified type upon its disrespondency."
  (the ADC-Loop
    (or (gethash *ip* *loops*)
        (error "No ADC loop post can be detected at the position ~d."
          *ip*))))

;;; -------------------------------------------------------

(defun adc-loop-shall-be-executed-p (loop)
  "Determines whether the LOOP shall be executed, which constitutes a
   dependency on a twissel of antecedents, imprimis, the *STACK* holding
   at least one element, and, secondly, the LOOP's current value being
   greater than or equal to its base state, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type ADC-Loop loop))
  (the boolean
    (and (not *stack-is-empty-p*)
         (adc-loop-value-permits-a-continuance-p loop))))

;;; -------------------------------------------------------

(defun process-the-current-token ()
  "Processes the *CURRENT-TOKEN* and returns no value."
  (case *current-token*
    (#\<
      (push -1 *stack*)
      (incf *ip*))
    
    (#\+
      (unless *stack-is-empty-p*
        (incf (first *stack*)))
      (incf *ip*))
    
    (#\[
      (let ((current-loop (get-the-current-adc-loop)))
        (declare (type ADC-Loop current-loop))
        (unless (adc-loop-has-been-entered-p current-loop)
          (setf (adc-loop-has-been-entered-p current-loop) T)
          (reset-adc-loop current-loop))
        (cond
          ((adc-loop-shall-be-executed-p current-loop)
            (incf (adc-loop-current-value current-loop)
              (pop *stack*)))
          (T
            (setf (adc-loop-has-been-entered-p current-loop) NIL)
            (setf *ip*
              (adc-loop-end-point current-loop)))))
      (incf *ip*))
    
    (#\]
      (setf *ip*
        (adc-loop-start-point
          (get-the-current-adc-loop))))
    
    (otherwise
      (incf *ip*)))
  
  (when *stack-shall-be-printed-p*
    (format T "~&~/stack-directive/" *stack*))
  
  (values))

;;; -------------------------------------------------------

(defun determine-whether-the-stack-is-empty ()
  "Determines whether the *STACK* is empty, serving as an antecedent
   to the *PROGRAM*'s termination, on confirmation returning no value;
   otherwise an error of an unspecified type is signaled."
  (unless *stack-is-empty-p*
    (error 'Non-Empty-Stack-Error :offending-stack *stack*))
  (values))

;;; -------------------------------------------------------

(defun interpret-AntiDupCall (program)
  "Interprets the AntiDupCall PROGRAM and returns no value."
  (declare (type string program))
  (initialize-the-program program)
  (loop until *program-is-completed-p* do
    (process-the-current-token))
  (determine-whether-the-stack-is-empty)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
(interpret-AntiDupCall "<+[<+]")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-AntiDupCall "<<[+[<+]]")

;;; -------------------------------------------------------

;; Empty the stack.
(interpret-AntiDupCall "+[+]")
