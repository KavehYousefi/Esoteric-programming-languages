;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "$", invented by the Esolang user "Iamcalledbob" and
;; presented on June 28th, 2018, the foundational concept of which wones
;; in a Procrustean forbisen applied that, applied in certain scant
;; variations, produces a quadruple potency, to whom the capacitation is
;; vouchsafed to modify an infinite tape of non-negative integer cells,
;; as well as jump back to a previous instruction in the code.
;; 
;; 
;; Concept
;; =======
;; The $ programming language, invented as a simplified derivation from
;; its entheus, "`", by the same author, impounded to unbounded
;; non-negative integers, by expulsion of the stock-father's tolerance
;; adhibited to the negative axis, is founded upon the expression of its
;; operational triad upon a stringent homogeneity in the syntaxis'
;; weftage.
;; 
;; == ONE PATTERN, FOUR CAUSATA ==
;; An aefauld general pattern's governance begets the four competences
;; commorant in $.  This weftage, expressed in the Extended Backus-Naur
;; Form (EBNF), ostends the following design:
;; 
;;   [ "+" ] , nonNegativeInteger , "$" , [ "+" ] , nonNegativeInteger
;; 
;; Any non-command token or invalid command construction effort is
;; neglected without the program's intrusion by an error.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF NON-NEGATIVE INTEGER CELLS ==
;; $'s program memory consigned itself to the liberty of an infinitely
;; large tape, a componency of non-negative integers cells unbridled
;; along the upper bourne.
;; 
;; Each cell answers to a non-negative integer subscript, its address;
;; the particular location of zero (0) is attributed with a twifaced
;; proprium: its assignment produces as an epiphenomenon an output of
;; the character whose ASCII code equals the newly acquired cell value.
;; 
;; An additional concomitant engages with any assignment, as the most
;; recent transpiration of such is embalmed for a future application.
;; 
;; == THE MOST RECENT MEMORY ASSIGNMENT: A CONDITIONAL ENTITY ==
;; A dioristic agency's dation is exercised upon every assignment,
;; actuating the the most recently transferred value's mandation, while
;; superseding any contingent equivalent from the preterite, in order to
;; define the guard value for a contingent following jump operation.
;; 
;; The twissel of goto facilities relies on the equality of the probed
;; memory cell and the previous assignment's moved object; the
;; instruction pointer's relocation is homologated exclusively upon this
;; antecedant's conformance. 
;; 
;; 
;; Instructions
;; ============
;; $'s instruction set tallies a quadruple membership, a moeity among
;; whom dedicates itself to the program memory's manipulation, its
;; remnants appertains to the conditional control flow facility.
;; 
;; Any tokens or compositions endowed with a similitude of an
;; instruction, yet invalid in their ultimate form or content, receive
;; an administration of patience tantamount to their neglect.
;; 
;; == OVERVIEW ==
;; The following apercu shall in a cursory nortelry's adhibition anenst
;; the language's operational competences.
;; 
;; Please heed that placeholder segments are emphasized by an underline
;; compact of asterisk symbols ("*") and intended to be substituted by
;; actual $ code.
;; 
;;   ------------------------------------------------------------------
;;   Command             | Effect
;;   --------------------+---------------------------------------------
;;   destAddr$+sourceVal | Sets the memory cell at the address
;;   ********  ********* | {destAddr} to the value {sourceVal}.
;;                       | If the {destAddr} equals zero (0), the
;;                       | character whose ASCII code equals the value
;;                       | {sourceVal} is concomitantly printed to the
;;                       | standard output.
;;                       |---------------------------------------------
;;                       | The assigned value {sourceVal}, as an
;;                       | epiphenomenon and a parasceve for contingent
;;                       | future jump operations, must be memorized in
;;                       | lieu of any precedent assignment object.
;;   ..................................................................
;;   destAddr$sourceAddr | Sets the memory cell at the address
;;   ******** ********** | {destAddr} to the value of the memory
;;                       | cell at the address {sourceAddr}.
;;                       | If the {destAddr} equals zero (0), the
;;                       | character whose ASCII code equals the value
;;                       | of the memory cell at the address
;;                       | {sourceAddr}.
;;                       |---------------------------------------------
;;                       | The assigned value obtained from the memory
;;                       | at the address {sourceAddr}, as an
;;                       | epiphenomenon and a parasceve for contingent
;;                       | future jump operations, must be memorized in
;;                       | lieu of any precedent assignment object.
;;   ..................................................................
;;   +guard$+stepsValue  | If the most recently assigned value equals
;;    *****  **********  | the {guard} value, moves the instruction
;;                       | pointer (IP) back by a tally of steps equal
;;                       | to the value {stepsValue}.
;;   ..................................................................
;;   +guard$stepsAddress | If the most recently assigned value equals
;;    ***** ************ | the {guard} value, moves the instruction
;;                       | pointer (IP) back by a tally of steps equal
;;                       | to the value of the memory cell at the
;;                       | address {stepsAddress}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This program has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-25
;; 
;; Sources:
;;   [esolang2023$]
;;   The Esolang contributors, "$", May 16th, 2023
;;   URL: "https://esolangs.org/wiki/$"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype token ()
  "The ``token'' type defines a token as a cons, the sinistral
   compartment of which comprehends the token type keyword symbol, while
   the dextral moiety, admitting any object, ensconces the value."
  '(cons keyword *))

;;; -------------------------------------------------------

(deftype tokenizer-function ()
  "The ``tokenizer'' type defines a function acting as a delegate for
   the ``tokenizer'' type in cases where a type specifier is
   interdicted, such as in the ``the'' special form's context."
  'function)

;;; -------------------------------------------------------

(deftype tokenizer ()
  "The ``tokenizer'' type defines a niladic function which upon each
   inquest returns the next token from its source."
  '(function () token))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list compact of zero or more elements
   of the ELEMENT-TYPE, defaulting to the generic ``*'' sentinel."
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

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero (0), but unimpounded along the upper bourne."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized variation on
   instruction classes."
  '(member
    :set-to-value
    :set-to-address
    :jump-by-value
    :jump-by-address))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type defines a $ instruction as a list composed
   of three elements, the first of which contributes the instruction
   type as an ``instruction-type'', the second the primary operand in a
   ``non-negative-integer'' object, and the third its secondary operand
   conformant to the same species."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (= (length (the list candidate)) 3)
            (typep (first  candidate) 'instruction-type)
            (typep (second candidate) 'non-negative-integer)
            (typep (third  candidate) 'non-negative-integer))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable $ program as a vector of
   zero or more ``instruction'' objects."
  '(vector instruction *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the $ program memory as a sparse vector
   of non-negative integer-valued cells, infinite in their tally and
   enumerated commencing with the address zero (0), realized as a hash
   table whose ``non-negative-integer'' keys accommodate the addresses,
   while the values, obeying to the same species, maintain the mapped
   cell values."
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
                (and (typep key   'non-negative-integer)
                     (typep value 'non-negative-integer))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a new ``token'' specified by its TYPE and the
   associated VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the token (cons type value)))

;;; -------------------------------------------------------

(defun get-token-type (token)
  "Returns the TOKEN's type information."
  (declare (type token token))
  (the keyword (car token)))

;;; -------------------------------------------------------

(defun get-token-value (token)
  "Returns the value associated with the TOKEN."
  (declare (type token token))
  (the T (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean (not (null (eq (car token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tokenizer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position in the
   SOURCE of the first non-space character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (space-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun read-integer (source start)
  "Proceeding from the START position into the SOURCE< reads an unsigned
   integer number and returns two values:
     (1) The extracted and parsed unsigned integer value.
     (2) The position in the SOURCE immediately succeeding the desinent
         digit from the extracted number."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values non-negative-integer fixnum)
    (loop
      for   position of-type fixnum from start below (length source)
      while (digit-char-p (char source position))
      finally
        (return
          (values
            (parse-integer source :start start :end position)
            position)))))

;;; -------------------------------------------------------

(defun make-tokenizer (source)
  "Returns a tokenizer operating on the SOURCE as a niladic function
   which upon each invocation returns the next token from the same.
   ---
   Upon the SOURCE's exhaustion, any request is responded to by a fresh
   end-of-file (EOF) token."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (symbol-macrolet ((current-character
                        (the character
                          (char source position))))
      (declare (type character current-character))
      (the function
        #'(lambda ()
            (setf position
              (skip-spaces source position))
            (the token
              (cond
                ((>= position (length source))
                  (make-token :eof NIL))
                
                ((char= current-character #\$)
                  (prog1
                    (make-token :$ current-character)
                    (incf position)))
                
                ((char= current-character #\+)
                  (prog1
                    (make-token :+ current-character)
                    (incf position)))
                
                ((digit-char-p current-character)
                  (multiple-value-bind (number new-position)
                      (read-integer source position)
                    (declare (type non-negative-integer number))
                    (declare (type fixnum               new-position))
                    (setf position new-position)
                    (make-token :number number)))
                
                (T
                  (prog1
                    (make-token :orrels current-character)
                    (incf position))))))))))

;;; -------------------------------------------------------

(defun get-next-token (tokenizer)
  "Returns the next token from the TOKENIZER."
  (declare (type tokenizer tokenizer))
  (the token (funcall tokenizer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- xxx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Token-Buffer fixnum) *)
                ensure-token-buffer-size))
(declaim (ftype (function (Token-Buffer)        *)
                purge-token-buffer))

;;; -------------------------------------------------------

(defclass Token-Buffer ()
  ((tokenizer
    :initarg       :tokenizer
    :initform      (error "Missing tokenizer.")
    :type          tokenizer
    :documentation "The token purveyance source.")
   (tokens
    :initform      (make-array 0
                     :element-type    'token
                     :initial-element (make-token :eof NIL)
                     :adjustable      T
                     :fill-pointer    0)
    :reader        get-tokens
    :type          (vector token *)
    :documentation "The currently buffered tokens.")
   (exhausted-p
    :initform      NIL
    :reader        token-buffer-exhausted-p
    :type          boolean
    :documentation "Determines whether the TOKENIZER is exhausted,
                    resorting to end-of-file tokens' delivery only."))
  (:documentation
    "The ``Token-Buffer'' class furnishes a temporary storage for
     tokens, founded upon a dynamic vector."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokens Token-Buffer) &key)
  "Aspires to ensure a minimum of three elements in the token buffer
   TOKENS and returns no value."
  (declare (type Token-Buffer tokens))
  (ensure-token-buffer-size tokens 3)
  (values))

;;; -------------------------------------------------------

(defun make-token-buffer (tokenizer)
  "Creates and returns a ``Token-Buffer'' which depends on the TOKENIZER
   for its tokens' provision."
  (the Token-Buffer
    (make-instance 'Token-Buffer :tokenizer tokenizer)))

;;; -------------------------------------------------------

(defun get-token-at (tokens index)
  "Returns the element of the token buffer TOKENS at the zero-based
   INDEX."
  (declare (type Token-Buffer tokens))
  (declare (type fixnum       index))
  (the Token (aref (slot-value tokens 'tokens) index)))

;;; -------------------------------------------------------

(defun get-token-buffer-size (tokens)
  "Returns the number of elements currently residing in the token buffer
   TOKENS."
  (the fixnum (fill-pointer (slot-value tokens 'tokens))))

;;; -------------------------------------------------------

(defun ensure-token-buffer-size (tokens minimum-size)
  "Ensures that the token buffer TOKENS exhibits the MINIMUM-SIZE in
   elements by contingent loading of such and returns no value."
  (declare (type Token-Buffer tokens))
  (declare (type fixnum       minimum-size))
  (loop
    while
      (< (get-token-buffer-size tokens) minimum-size)
    for next-token
      of-type token
      =       (get-next-token (slot-value tokens 'tokenizer))
    ;; No more tokens?
    if (token-type-p next-token :eof) do
      (setf (slot-value tokens 'exhausted-p) T)
      (loop-finish)
    ;; No-operation token?
    else if (token-type-p next-token :orrels) do
      (purge-token-buffer tokens)
    ;; Valid isntruction token?
    else do
      (vector-push-extend next-token
        (slot-value tokens 'tokens)))
  (values))

;;; -------------------------------------------------------

(defun purge-token-buffer (tokens)
  "Clears the token buffer TOKENS and returns no value."
  (declare (type Token-Buffer tokens))
  (setf (fill-pointer (slot-value tokens 'tokens)) 0)
  (values))

;;; -------------------------------------------------------

(defun drop-tokens (tokens number-of-ejecta)
  "Removes the first NUMBER-OF-EJECTA elements from the token buffer
   TOKENS and returns no value."
  (declare (type Token-Buffer tokens))
  (declare (type fixnum       number-of-ejecta))
  (setf (slot-value tokens 'tokens)
    (delete-if (constantly T)
      (slot-value tokens 'tokens)
      :start 0
      :end   (min number-of-ejecta
               (get-token-buffer-size tokens))))
  (values))

;;; -------------------------------------------------------

(defun tokens-match-p (tokens expected-token-types)
  "Determines whether the token buffer TOKENS' elements match the
   EXPECTED-TOKEN-TYPES, if reded by necessity, appending further tokens
   in order to accommodate the requisite size, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Buffer      tokens))
  (declare (type (list-of keyword) expected-token-types))
  (ensure-token-buffer-size tokens
    (length expected-token-types))
  (the boolean
    (not (null
      (and
        (>= (get-token-buffer-size tokens)
            (length expected-token-types))
        (every
          #'(lambda (probed-token expected-type)
              (declare (type token   probed-token))
              (declare (type keyword expected-type))
              (token-type-p probed-token expected-type))
          (slot-value tokens 'tokens)
          expected-token-types))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction (type first-operand second-operand)
  "Creates and returns a new ``instruction'' subsuming into the
   specified TYPE and complemented by the FIRST-OPERAND and
   SECOND-OPERAND."
  (the instruction (list type first-operand second-operand)))

;;; -------------------------------------------------------

(defun get-instruction-type (instruction)
  "Returns the INSTRUCTION's categorizing type."
  (declare (type instruction instruction))
  (the instruction-type (first instruction)))

;;; -------------------------------------------------------

(defun get-first-instruction-operand (instruction)
  "Returns the INSTRUCTION's first operand."
  (declare (type instruction instruction))
  (the non-negative-integer (second instruction)))

;;; -------------------------------------------------------

(defun get-second-instruction-operand (instruction)
  "Returns the INSTRUCTION's second operand."
  (declare (type instruction instruction))
  (the non-negative-integer (third instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-program (tokens)
  "Parses the tokens conveyed by the token buffer TOKENS and returns the
   assembled $ program."
  (declare (type Token-Buffer tokens))
  (let ((instructions NIL))
    (declare (type (list-of instruction) instructions))
    (flet ((collect-instruction (type
                                 &optional (first-argument  0)
                                           (second-argument 0))
            "Creates a new instruction specified by its TYPE and the
             optional FIRST-ARGUMENT and SECOND-ARGUMENT jumelle, which
             acquire a default value of zero (0), prepends this freshly
             produced operation to the INSTRUCTIONS list, and returns no
             value."
            (declare (type instruction-type     type))
            (declare (type non-negative-integer first-argument))
            (declare (type non-negative-integer second-argument))
            (push (make-instruction type first-argument second-argument)
                  instructions)
            (values)))
      
      (loop do
        (ensure-token-buffer-size tokens 3)
        
        (cond
          ;; Insufficient tally of tokens remaining for an instruction?
          ;; => Cease the compilation process.
          ((and (< (get-token-buffer-size tokens) 3)
                (token-buffer-exhausted-p tokens))
            (loop-finish))
          
          ;; A$B
          ((tokens-match-p tokens '(:number :$ :number))
            (collect-instruction :set-to-address
              (get-token-value (get-token-at tokens 0))
              (get-token-value (get-token-at tokens 2)))
            (drop-tokens tokens 3))
          
          ;; A$+B
          ((tokens-match-p tokens '(:number :$ :+ :number))
            (collect-instruction :set-to-value
              (get-token-value (get-token-at tokens 0))
              (get-token-value (get-token-at tokens 3)))
            (drop-tokens tokens 4))
          
          ;; +A$B
          ((tokens-match-p tokens '(:+ :number :$ :number))
            (collect-instruction :jump-by-address
              (get-token-value (get-token-at tokens 1))
              (get-token-value (get-token-at tokens 3)))
            (drop-tokens tokens 4))
          
          ;; +A$+B
          ((tokens-match-p tokens '(:+ :number :$ :+ :number))
            (collect-instruction :jump-by-value
              (get-token-value (get-token-at tokens 1))
              (get-token-value (get-token-at tokens 4)))
            (drop-tokens tokens 5))
          
          ;; No instruction could be matched?
          ;; => Proceed with the TOKENS buffer curtailed of its front
          ;;    token.
          (T
            (drop-tokens tokens 1)
            (loop-finish))))
      
      (the program
        (coerce (nreverse instructions)
          '(simple-array * (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns an empty ``memory'' object."
  (the memory (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun get-memory-cell-at (memory address)
  "Returns the value of the cell commorant in the MEMORY at the
   ADDRESS."
  (declare (type memory               memory))
  (declare (type non-negative-integer address))
  (the non-negative-integer (gethash address memory 0)))

;;; -------------------------------------------------------

(defun set-memory-cell-at (memory address new-value)
  "Sets the value of the cell commorant in the MEMORY at the ADDRESS to
   the NEW-VALUE and returns no value."
  (declare (type memory               memory))
  (declare (type non-negative-integer address))
  (declare (type non-negative-integer new-value))
  (setf (gethash address memory 0) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-program (program
                          &key (initial-memory (make-memory)))
  "Interprets the $ PROGRAM, optionally operating upon the
   INITIAL-MEMORY, and returns no value."
  (declare (type program program))
  (declare (type memory  initial-memory))
  (let ((ip                  0)
        (memory              initial-memory)
        (last-assigned-value 0))
    (declare (type non-negative-integer ip))
    (declare (type memory               memory))
    (declare (type non-negative-integer last-assigned-value))
    
    (symbol-macrolet ((current-instruction
                        (the instruction
                          (aref program ip))))
      (declare (type instruction current-instruction))
      
      (flet ((print-cell-if-necessary (destination-address)
              "If the DESTINATION-ADDRESS equals zero (0), prints the
               character whose ASCII code matches the cell value,
               otherwise abstains from any action, in any case returning
               no value."
              (declare (type non-negative-integer destination-address))
              (when (zerop destination-address)
                (write-char
                  (code-char
                    (mod
                      (get-memory-cell-at memory destination-address)
                      256))))
              (values)))
        
        (loop while (array-in-bounds-p program ip) do
          (destructuring-bind
              (instruction-type first-operand second-operand)
              current-instruction
            (declare (type instruction-type     instruction-type))
            (declare (type non-negative-integer first-operand))
            (declare (ignorable                 first-operand))
            (declare (type non-negative-integer second-operand))
            (declare (ignorable                 second-operand))
            
            (case instruction-type
              ;; A$+B
              (:set-to-value
                (set-memory-cell-at memory first-operand second-operand)
                (print-cell-if-necessary first-operand)
                (setf last-assigned-value second-operand)
                (incf ip))
              
              ;; A$B
              (:set-to-address
                (let ((new-value (get-memory-cell-at memory
                                                     second-operand)))
                  (declare (type non-negative-integer new-value))
                  (set-memory-cell-at memory first-operand new-value)
                  (setf last-assigned-value new-value)
                  (print-cell-if-necessary first-operand))
                (incf ip))
              
              ;; +A$+B
              (:jump-by-value
                (if (= last-assigned-value first-operand)
                  (decf ip second-operand)
                  (incf ip)))
              
              ;; +A$B
              (:jump-by-address
                (if (= last-assigned-value first-operand)
                  (decf ip (get-memory-cell-at memory second-operand))
                  (incf ip)))
              
              (otherwise
                (error "Unrecognized instruction at position ~d: ~s."
                  ip current-instruction))))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-$ (code &key (initial-memory (make-memory)))
  "Interprets the piece of $ source CODE, optionally operating on the
   INITIAL-MEMORY, and returns no value."
  (declare (type string code))
  (declare (type memory initial-memory))
  (interpret-program
    (parse-program
      (make-token-buffer
        (make-tokenizer code)))
    :initial-memory initial-memory)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-$
 "0$+72
  0$+101
  0$+108
  0$+108
  0$+111
  0$+44
  0$+32
  0$+87
  0$+111
  0$+114
  0$+108
  0$+100
  0$+33")

;;; -------------------------------------------------------

;; Infinite repeating cat program which expects the ASCII code of the
;; character to print in the memory cell at the address one (1).
(interpret-$
  "This program relies upon the cell at the address one to contain
   the user input.
   0$1
   2$+0
   +0$+2"
  :initial-memory
    (let ((memory (make-memory)))
      (declare (type memory memory))
      (set-memory-cell-at memory 1 65)
      (the memory memory)))
