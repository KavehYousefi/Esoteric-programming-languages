;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Inputer", invented by the Esolang user "ChuckEsoteric08"
;; and presented on August 12th, 2022, the foundry of which constitutes
;; the manifestation of a stack whose indagation and manipulation, in
;; particular with regards to user input, impel the programs' efficacy.
;; 
;; 
;; Concept
;; =======
;; The Inputer programming language's substratum is furnished by a stack
;; of unbounded signed integers, which, in conjunction with basic
;; arithmetics, input/output facilities, and control flow mechanisms,
;; permits the accompassing of utility to its program.
;; 
;; == INPUTER: INPUT AS THE AEFAULD DATA OBTENTION SOURCE ==
;; The language's agnomination, "Inputer", most probably originates from
;; the reliance upon user *input* for the chief avenue of its data's
;; provision, as literal objects may be introduced into the stack.
;; 
;; == A STACK GOVERNS THE PROGRAM ==
;; All effect partakes in relation with the foundational storage, the
;; stack of integers, infinite in its possible capacity, and specialized
;; for integer items' castaldy.
;; 
;; == INPUTER: VOLATILE + INPUT ==
;; Inputer as a concept derives from "Volatile", a language designed by
;; the user "A" and presented on April 5th, 2019, whose cynosure,
;; likewise, constitutes a stack of unbounded numbers, while destitution
;; limns its input and output facilities; the lacunae of which is
;; meliorated by this derivation.
;; 
;; 
;; Architecture
;; ============
;; The Inputer program memory is realized in its stack, theoretically
;; infinite regarding the capacity, whose elements are represented by
;; signed integers of unbounded size.
;; 
;; 
;; Data Types
;; ==========
;; Inputer's type hierarchy conflates with a singular species: signed
;; integers of any magnitude, their governance the subject of the
;; program's stack.
;; 
;; 
;; Syntax
;; ======
;; Anenst its syntactical department, an Inputer program embraces a
;; sequence of zero or more niladic commands, each a single character's
;; product, contingently segregated by whitespaces, but impermeable to
;; any other content.
;; 
;; == INSTRUCTIONS ==
;; Every instruction's expression proceeds by an aefauld character's
;; mediation, tallying a preponderance that subscribes to such austerity
;; as to require no further dependent; merely the goto and loop
;; facilities impose an ubiquitous twissel's presence.
;; 
;; Tokens not corresponding to instructions or whitespaces are not
;; homologated in a program and instigate an error's elicitation.
;; 
;; == WHITESPACES ==
;; The distribution of whitespaces, a term whose amplectation expands
;; across the space, horizontal tab, and newline character, constitutes
;; a variable of one's own deliberation in both quantity and spatial
;; regards.
;; 
;; == COMMENTS ==
;; The current language iteration does not attend to any contingency for
;; comments.
;; 
;; == GRAMMAR ==
;; The language's donet shall be the cynosure of an Extended Backus-Naur
;; Form (ENBF) explication:
;; 
;;   program     := commandList ;
;;   commandList := { whitespaces | command } ;
;;   command     := input
;;               |  add
;;               |  subtract
;;               |  multiply
;;               |  divide
;;               |  jump
;;               |  loop
;;               |  outputStack
;;               ;
;;   input       := "'" ;
;;   add         := "+" ;
;;   subtract    := "-" ;
;;   multiply    := "*" ;
;;   divide      := "/" ;
;;   jump        := "!" , commandList , "?" ;
;;   loop        := "(" , commandList , ")" ;
;;   outputStack := "#" ;
;;   whitespaces := { whitespace } ;
;;   whitespace  := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Inputer's instruction set tallies eight members, the competences of
;; which govern the bailiwicks of basic arithmetics, input and output,
;; as well as conditional goto and iteration facilities.
;; 
;; == OVERVIEW ==
;; A tabular apercu shall administer a foundational mete of gnarity
;; concerning the language's operational facilities.
;; 
;; Please heed that placeholder segments are emphasized by adminiculum
;; of underlining asterisks ("*"), and are intended to be substituted by
;; actual Inputer code.
;; 
;;   ------------------------------------------------------------------
;;   Commands   | Effect
;;   -----------+------------------------------------------------------
;;   '          | Queries the standard input for a signed or unsigned
;;              | integer number and pushes the same unto the stack.
;;   ..................................................................
;;   +          | Pops the two top stack elements, "a" and "b", from
;;              | the stack, computes the sum b + a, and pushes the
;;              | result unto the stack.
;;              |------------------------------------------------------
;;              | If any of the pop operations are applied to an empty
;;              | stack, an error of the type "EmptyStackError" is
;;              | issued.
;;              |------------------------------------------------------
;;              | Expressed in pseudocode, these causata ensue:
;;              |   let a      <- pop from stack
;;              |   let b      <- pop from stack
;;              |   let result <- b + a
;;              |   push result unto stack
;;   ..................................................................
;;   -          | Pops the two top stack elements, "a" and "b", from
;;              | the stack, computes the difference b - a, and pushes
;;              | the result unto the stack.
;;              |------------------------------------------------------
;;              | If any of the pop operations are applied to an empty
;;              | stack, an error of the type "EmptyStackError" is
;;              | issued.
;;              |------------------------------------------------------
;;              | Expressed in pseudocode, these causata ensue:
;;              |   let a      <- pop from stack
;;              |   let b      <- pop from stack
;;              |   let result <- b - a
;;              |   push result unto stack
;;   ..................................................................
;;   *          | Pops the two top stack elements, "a" and "b", from
;;              | the stack, computes the product b * a, and pushes the
;;              | result unto the stack.
;;              |------------------------------------------------------
;;              | If any of the pop operations are applied to an empty
;;              | stack, an error of the type "EmptyStackError" is
;;              | issued.
;;              |------------------------------------------------------
;;              | Expressed in pseudocode, these causata ensue:
;;              |   let a      <- pop from stack
;;              |   let b      <- pop from stack
;;              |   let result <- b * a
;;              |   push result unto stack
;;   ..................................................................
;;   /          | Pops the two top stack elements, "a" and "b", from
;;              | the stack, computes the quotient b / a, and pushes
;;              | the result unto the stack.
;;              |------------------------------------------------------
;;              | If any of the pop operations are applied to an empty
;;              | stack, an error of the type "EmptyStackError" is
;;              | issued.
;;              |------------------------------------------------------
;;              | Expressed in pseudocode, these causata ensue:
;;              |   let a      <- pop from stack
;;              |   let b      <- pop from stack
;;              |   let result <- b / a
;;              |   push result unto stack
;;   ..................................................................
;;   !          | Defines a return point for a subsequent "?" command.
;;              |------------------------------------------------------
;;              | The matching "?" always follows later in the program.
;;   ..................................................................
;;   ?          | Pops the top stack element, "a", and peeks the new
;;              | top stack element "b" without removal; if a does not
;;              | equal b, jumps back to the matching "!" command.
;;              | Otherwise proceeds as usual.
;;              |------------------------------------------------------
;;              | If any of the pop operations are applied to an empty
;;              | stack, an error of the type "EmptyStackError" is
;;              | issued.
;;              |------------------------------------------------------
;;              | Expressed in pseudocode, these causata ensue:
;;              |   let a <- pop from stack
;;              |   let b <- peek stack
;;              |   if a != b then
;;              |     jump back to matching "!"
;;              |   end if
;;   ..................................................................
;;   (commands) | Repeats the {commands} while the top stack element
;;    ********  | does not equal zero (0).
;;              |------------------------------------------------------
;;              | {commands} must be sequence comprehending zero or
;;              | more instructions.
;;              |------------------------------------------------------
;;              | The top stack element is probed, but not removed;
;;              | this is tantamount to the stack "peek" behavior.
;;              |------------------------------------------------------
;;              | If the stack is empty during the peeking operation,
;;              | an error of the type "EmptyStackError" is issued.
;;   ..................................................................
;;   #          | Outputs the entire stack content in an
;;              | implementation-dependent format.
;;              |------------------------------------------------------
;;              | This operation's purpose is assigned to debugging
;;              | activities.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Inputer protolog, conditioned by its lucid expositions and its
;; descendance from Volatile, a provenance admitting a noscitur a sociis
;; patration, does not thole many inroads of ambiguities.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has been realized in the programming
;; language Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-29
;; 
;; Sources:
;;   [esolang2022Inputer]
;;   The Esolang contributors, "Inputer", August 12th, 2022
;;   URL: "https://esolangs.org/wiki/Inputer"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest parameter-list)
     &body body)
  "Defines a derived type using the ``deftype'' predicate ``satisfies'',
   with the new type nevened by the TYPE-NAME, bearing the
   PARAMETER-LIST as its inputs, and probing an object agnominated by
   the CANDIDATE-VARIABLE utilzing the BODY forms, where the desinent
   form's primary result determines the candidate's eligibility,
   construing a non-``NIL'' value as a positive match, whereas ``NIL''
   signifies its failure."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@parameter-list)
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Definition of the type ``~s''." type-name))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic ``*'' sentinel."
  (and
    (hash-table-p candidate)
    (or
      (and (eq key-type   '*)
           (eq value-type '*))
      (loop
        for key
          of-type T
          being the hash-keys in (the hash-table candidate)
        using
          (hash-value value)
        always
          (and (typep key   key-type)
               (typep value value-type))))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variation of Inputer
   commands."
  '(member
    :input
    :add
    :subtract
    :multiply
    :divide
    :define-return-point
    :jump-to-return-point
    :start-loop
    :end-loop
    :output-stack))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral connection betwixt a
   forward and a back jump point, either in the context of a goto
   facility or an iterative construct, and realized in terms of a hash
   table which maps the fixnum jump positions in a parsed Inputer
   program to each other."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   which conform to the ELEMENT-TYPE, defaulting to the generic ``*''
   sentinel."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (loop
        for    element of-type T in (the list candidate)
        always (typep element element-type)))))

;;; -------------------------------------------------------

(deftype stack-of (&optional (element-type '*))
  "The ``stack-of'' type defines a last-in first-out (LIFO) data
   structure stack implementation as a list composed of zero or more
   elements which conform to the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype inputer-program ()
  "The ``inputer-program'' type defines an executable Inputer command
   sequence as a vector of zero or more ``command'' objects."
  '(vector command *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts from the piece of Inputer source CODE the incorporated
   commands and returns these as a one-dimensional simple array of
   ``command'' objects, compatible with the ``inputer-program'' type."
  (declare (type string code))
  (the inputer-program
    (coerce
      (loop
        for token    of-type character across code
        and position of-type fixnum    from   0 by 1
        append
          (case token
            (#\Newline NIL)
            (#\Space   NIL)
            (#\Tab     NIL)
            (#\'       (list :input))
            (#\+       (list :add))
            (#\-       (list :subtract))
            (#\*       (list :multiply))
            (#\/       (list :divide))
            (#\!       (list :define-return-point))
            (#\?       (list :jump-to-return-point))
            (#\(       (list :start-loop))
            (#\)       (list :end-loop))
            (#\#       (list :output-stack))
            (otherwise (error "Invalid token ~s at position ~d."
                         token position))))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Stack-Error (error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :type          (stack-of integer)
    :documentation "The stack which, being empty at the instance of its
                    access, has begotten this erroneous condition."))
  (:report
    (lambda (condition destination)
      (declare (type Empty-Stack-Error condition))
      (declare (type destination       destination))
      (format destination "Cannot pop from or peek into the ~
                           empty stack ~s."
        (empty-stack-error-offended-stack condition))))
  (:documentation
    "The ``Empty-Stack-Error'' condition serves in the signaling of an
     anomalous situation elicited by the attempt to pop from or peek
     into a stack destitute of any elements."))

;;; -------------------------------------------------------

(defun signal-empty-stack-error (offended-stack)
  "Signals an ``Empty-Stack-Error'' apprizing about the OFFENDED-STACK."
  (declare (type (stack-of integer) offended-stack))
  (error 'Empty-Stack-Error :offended-stack offended-stack))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-goto-table (program)
  "Calculates and returns for the Inputer PROGRAM the goto table, a jump
   table which maps the locations of the return point commands (\"!\")
   to those of the jump instructions (\"?\"), and vice versa."
  (declare (type inputer-program program))
  (let ((goto-table    (make-hash-table :test #'eql))
        (return-points NIL))
    (declare (type jump-table        goto-table))
    (declare (type (stack-of fixnum) return-points))
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0       by 1
      if (eq command :define-return-point) do
        (push position return-points)
      else if (eq command :jump-to-return-point) do
        (if return-points
          (let ((return-point (pop return-points))
                (jump-point   position))
            (declare (type fixnum return-point))
            (declare (type fixnum jump-point))
            (setf (gethash return-point goto-table) jump-point)
            (setf (gethash jump-point   goto-table) return-point))
          (error "Unmatched \"?\" at position ~d." position))
      end
      finally
        (when return-points
          (error "Unmatched \"!\" tokens at positions ~{~d~^, ~}."
            return-points)))
    (the jump-table goto-table)))

;;; -------------------------------------------------------

(defun build-loop-table (program)
  "Calculates and returns for the Input PROGRAM the loop table, a jump
   table which maps the locations of the loop start commands (\"(\") to
   those of the matching end commands (\")\"), and vice versa."
  (declare (type inputer-program))
  (let ((loop-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table        loop-table))
    (declare (type (stack-of fixnum) start-points))
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0       by 1
      if (eq command :start-loop) do
        (push position start-points)
      else if (eq command :end-loop) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point loop-table) end-point)
            (setf (gethash end-point   loop-table) start-point))
          (error "Unmatched \")\" at position ~d." position))
      end
      finally
        (when start-points
          (error "Unmatched \"(\" tokens at positions ~{~d~^, ~}."
            start-points)))
    (the jump-table loop-table)))

;;; -------------------------------------------------------

(defun interpret-program (program)
  "Interprets the Input PROGRAM and returns no value."
  (declare (type inputer-program program))
  (let ((ip         0)
        (goto-table (build-goto-table program))
        (loop-table (build-loop-table program))
        (stack      NIL))
    (declare (type fixnum             ip))
    (declare (type jump-table         goto-table))
    (declare (type jump-table         loop-table))
    (declare (type (stack-of integer) stack))
    (labels
        ((advance-ip ()
          "Moves the instruction pointer (IP) to the next position in
           the PROGRAM, if possible, and returns no value."
          (when (array-in-bounds-p program ip)
            (incf ip))
         (values))
         
         (move-to (new-position)
          "Relocates the instruction pointer (IP) to the NEW-POSITION
           and returns no value."
          (declare (type fixnum new-position))
          (setf ip new-position)
          (values))
         
         (peek-stack ()
          "Returns without removing the STACK's top element, or signals
           an error of the type ``Empty-Stack-Error'' upon its vacancy."
          (the integer
            (or (first stack)
                (signal-empty-stack-error stack))))
         
         (pop-from-stack ()
          "Removes and returns the STACK's top element, or signals an
           error of the type ``Empty-Stack-Error'' upon its vacancy."
          (the integer
            (or (pop stack)
                (signal-empty-stack-error stack))))
         
         (push-to-stack (new-element)
          "Pushes the NEW-ELEMENT unto the STACK and returns no value."
          (declare (type integer new-element))
          (push new-element stack)
          (values))
         
         (apply-to-stack (operator)
          "Pops the two top STACK elements, \"a\" and \"b\", applies the
           the OPERATOR to \"b\" and \"a\" in this exact order, pushes
           the result unto the STACK, and returns no value.
           ---
           An error of the type ``Empty-Stack-Error'' is signaled if the
           STACK cannot accommodate two elements because of its
           quantitative insufficiency."
          (declare (type (function (integer integer) integer) operator))
          (let ((a (pop-from-stack))
                (b (pop-from-stack)))
            (declare (type integer a))
            (declare (type integer b))
            (push-to-stack
              (funcall operator b a)))
          (values)))
      
      (symbol-macrolet
          ((program-exhausted-p
            (the boolean
              (not (null
                (>= ip (length program))))))
           
           (current-command
            (the command
              (aref program ip))))
        
        (loop until program-exhausted-p do
          (case current-command
            (:input
              (format T "~&>> ")
              (finish-output)
              (push-to-stack
                (parse-integer
                  (read-line)))
              (clear-input)
              (advance-ip))
            
            (:add
              (apply-to-stack #'+)
              (advance-ip))
            
            (:subtract
              (apply-to-stack #'-)
              (advance-ip))
            
            (:multiply
              (apply-to-stack #'*)
              (advance-ip))
            
            (:divide
              (apply-to-stack #'round)
              (advance-ip))
            
            (:define-return-point
              (advance-ip))
            
            (:jump-to-return-point
              (let ((top-element (pop-from-stack)))
                (declare (type integer top-element))
                (if (/= top-element (peek-stack))
                  (move-to (gethash ip goto-table))
                  (advance-ip))))
            
            (:start-loop
              (cond
                ((zerop (peek-stack))
                  (move-to (gethash ip loop-table))
                  (advance-ip))
                (T
                  (advance-ip))))
            
            (:end-loop
              (move-to (gethash ip loop-table)))
            
            (:output-stack
              (format T "~&[top> ~{~d~^, ~} <bottom]" stack)
              (advance-ip))
            
            (otherwise
              (error "Invalid command ~s at position ~d."
                current-command ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Inputer (code)
  "Interprets the piece of Inputer source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (extract-commands code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time "cat program".
(interpret-Inputer "'#")

;;; -------------------------------------------------------

;; Repeating "cat program" which terminates on an input equal to
;; zero (0).
(interpret-Inputer "'#('#)")

;;; -------------------------------------------------------

;; Truth-machine which utilizes the debug facility for printing the
;; stack content.
(interpret-Inputer "'(#)#")

;;; -------------------------------------------------------

;; Number guessing game mimicry.
;; 
;; Query the user for two numbers, the first norned "guard", the second
;; "test", and repeatedly pop, match, and substitute the "test", while
;; concomitantly printing the two stack items, until the two top
;; elements are equal --- akin to a number guessing game.
;; 
;; Formulated in pseudocode, the following holds:
;; 
;;   let guard <- input number
;;   let test  <- nil
;;   
;;   set label
;;   print stack
;;   test <- input number
;;   if test != guard then
;;     goto label
;;   end if
;; 
(interpret-Inputer "'!'#? ")

;;; -------------------------------------------------------

;; Adder.
;; 
;; Adds two numbers and print the stack.
(interpret-Inputer "''+#")
