;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "¢%", invented by the Esolang user "AmNow" and presented on
;; June 28th, 2021, its existency's indicium wones in the representation
;; of its hexadecimal accompt of competences by combinations of the
;; character twissel "¢" and "%", their coefficient entelechy's
;; phenotype such facilities as to seize the homologation of the
;; memory's integer-valued stack's content, the peracting of basic
;; arithmetics, input and output communications, as well as the
;; navigation athwart the code per procurationem of a jump-based
;; conditional mechanism.
;; 
;; 
;; Concept
;; =======
;; The ¢% programming language's firmament is edified upon its
;; instructions' representation by four-letter identifiers of such
;; componency as to merely admit the symbols "¢" and "%", their telos
;; the manipulation of a stack dedicated to the castaldy of signed
;; integer numbers.
;; 
;; == "¢%": TWO CHARACTERS, SIXTEEN INSTRUCTIONS ==
;; The "¢%" agnominations engages in a bewrayment of the syntaxis'
;; foundational conformation, the operative expressions of which derive
;; from catenae, of uniform quadruple dispansion, of the "¢" and "%"
;; symbols, their tally's coalescence into 16 specimens.
;; 
;; == THE MEMORY: A STACK OF SIGNED INTEGER NUMBERS ==
;; The language's memory model is realized in a stack, the currency of
;; which relates of signed integers to whom neither a mear anent the
;; polarity nor the mickleness exercises its purview.
;; 
;; 
;; Instructions
;; ============
;; The ¢% programming language's instruction set enumerates a
;; hexadecimal accompt of members, its attrectation's bailiwicks the
;; manipulation of the memory stack, basic arithmetics, input and output
;; communications, as well as an aefauld jump-based control flow
;; construct.
;; 
;; == OVERVIEW ==
;; A requisite nortelry's adhibition concerning the language's operative
;; warklumes shall establish the following apercu's entelechy:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   STACK INSERTION AND REMOVAL
;;   ------------------------------------------------------------------
;;   %%¢¢    | Pushes the number one (1) onto the stack.
;;   ..................................................................
;;   ¢¢¢¢    | Pops the top stack element and discard the same.
;;   ==================================================================
;;   STACK ORDER MANIPULATION
;;   ------------------------------------------------------------------
;;   ¢¢¢%    | Swaps the positions of the two top stack elements.
;;   ..................................................................
;;   ¢¢%¢    | Duplicates the top stack element.
;;   ..................................................................
;;   ¢¢%%    | Moves the element at the stack's bottom to the top
;;           | position.
;;   ..................................................................
;;   ¢%¢%    | Moves the element at the stack top to the bottom
;;           | position.
;;   ==================================================================
;;   ARITHMETICS
;;   ------------------------------------------------------------------
;;   ¢%%¢    | Pops the top stack element, here nevened "a", removes
;;           | the new top element, "b", supputates the sum of (a + b),
;;           | and pushes the same onto the stack.
;;   ..................................................................
;;   ¢%%%    | Pops the top stack element, here nevened "a", removes
;;           | the new top element, "b", supputates the difference of
;;           | (a - b), and pushes the same onto the stack.
;;   ..................................................................
;;   %¢¢¢    | Pops the top stack element, here nevened "a", removes
;;           | the new top element, "b", supputates the product of
;;           | (a * b), and pushes the same onto the stack.
;;   ..................................................................
;;   %¢¢%    | Pops the top stack element, here nevened "a", removes
;;           | the new top element, "b", supputates the quotient of
;;           | (a / b), rounded to the nearest integer, and pushes the
;;           | same onto the stack.
;;   ==================================================================
;;   INPUT AND OUTPUT
;;   ------------------------------------------------------------------
;;   %¢%¢    | Queries the standard input conduit for a signed or
;;           | unsigned integer number and pushes the same onto the
;;           | stack.
;;   ..................................................................
;;   %¢%%    | Queries the standard input conduit for a Unicode
;;           | character and pushes its code point onto the stack.
;;   ..................................................................
;;   %%¢%    | Pops the top stack element and prints the same in its
;;           | verbatim numeric form to the standard output.
;;   ..................................................................
;;   %%%¢    | Pops the top stack element and prints the Unicode
;;           | character whose code point concurs with the same to the
;;           | standard output conduit.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   ¢%¢¢    | If the top stack element equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "%%%%" instruction;
;;           | otherwise, proceeds as usual.
;;   ..................................................................
;;   %%%%    | If the top stack element does not equal zero (0), moves
;;           | the instruction pointer (IP) backwards to the position
;;           | immediately succeeding the matching "¢%¢¢" instruction;
;;           | otherwise, proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes a chevisance peracted
;; in the programming language Common Lisp, its operation as twyfold
;; gestion; ensuing, imprimis, from an extraction of the ensconced
;; instructions in a more covenable format, ere their actual execution's
;; ultimity in the desinent stage.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-02-25
;; 
;; Sources:
;;   [esolang:2021:¢%]
;;   The Esolang contributors, "¢%", June 28th, 2021
;;   URL: "https://esolangs.org/wiki/%C2%A2%25"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE."
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

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack of zero or more
   elements, each of which conforms to the ELEMENT-TYPE."
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
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE."
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

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   encompassing, among others, ``format'', ``write'', and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized ¢% instructions."
  '(member
    :pop
    :swap
    :duplicate
    :move-up
    :start-loop
    :move-down
    :add
    :subtract
    :multiply
    :divide
    :input-number
    :input-character
    :push-1
    :output-number
    :output-character
    :end-loop))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string instruction)
               +INSTRUCTION-IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates with an ¢% instruction name the instruction type.")

;;; -------------------------------------------------------

;; Build the instruction table.
(setf (gethash "¢¢¢¢" +INSTRUCTION-IDENTIFIERS+) :pop)
(setf (gethash "¢¢¢%" +INSTRUCTION-IDENTIFIERS+) :swap)
(setf (gethash "¢¢%¢" +INSTRUCTION-IDENTIFIERS+) :duplicate)
(setf (gethash "¢¢%%" +INSTRUCTION-IDENTIFIERS+) :move-up)
(setf (gethash "¢%¢¢" +INSTRUCTION-IDENTIFIERS+) :start-loop)
(setf (gethash "¢%¢%" +INSTRUCTION-IDENTIFIERS+) :move-down)
(setf (gethash "¢%%¢" +INSTRUCTION-IDENTIFIERS+) :add)
(setf (gethash "¢%%%" +INSTRUCTION-IDENTIFIERS+) :subtract)
(setf (gethash "%¢¢¢" +INSTRUCTION-IDENTIFIERS+) :multiply)
(setf (gethash "%¢¢%" +INSTRUCTION-IDENTIFIERS+) :divide)
(setf (gethash "%¢%¢" +INSTRUCTION-IDENTIFIERS+) :input-number)
(setf (gethash "%¢%%" +INSTRUCTION-IDENTIFIERS+) :input-character)
(setf (gethash "%%¢¢" +INSTRUCTION-IDENTIFIERS+) :push-1)
(setf (gethash "%%¢%" +INSTRUCTION-IDENTIFIERS+) :output-number)
(setf (gethash "%%%¢" +INSTRUCTION-IDENTIFIERS+) :output-character)
(setf (gethash "%%%%" +INSTRUCTION-IDENTIFIERS+) :end-loop)

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (not (null
    (member character '(#\Space #\Newline #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun instruction-character-p (character)
  "Checks whether the CHARACTER represents a constituent of an
   instruction name, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character character))
  (not (null (member character '(#\¢ #\%) :test #'char=))))

;;; -------------------------------------------------------

(defun parse-instruction (identifier)
  "Returns the ``instruction'' associated with the IDENTIFIER, signaling
   an error on failure to locate a correspondence."
  (declare (type string identifier))
  (multiple-value-bind (instruction contains-identifier-p)
      (gethash identifier +INSTRUCTION-IDENTIFIERS+)
    (declare (type (or null instruction) instruction))
    (declare (type T                     contains-identifier-p))
    (the instruction
      (if contains-identifier-p
        instruction
        (error "Invalid instruction identifier: ~s."
          identifier)))))

;;; -------------------------------------------------------

(defun parse-¢% (code)
  "Extracts and returns from the piece of ¢% CODE a vector of
   instructions."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of instruction) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character, if
               possible, updates the current CHARACTER, and returns no
               value."
              (setf character
                (when (< position (1- (length code)))
                  (char code (incf position))))
              (values))
             
             (skip-whitespaces ()
              "Starting at the current POSITION, skips zero or more
               whitespaces, relocates the POSITION cursor to the first
               non-whitespace character, and returns no value."
              (loop
                while (and character (whitespace-character-p character))
                do    (advance))
              (values))
             
             (read-instruction ()
              "Starting at the current POSITION, reads an instruction
               name and returns the corresponding ``instruction''
               object."
              (the instruction
                (parse-instruction
                  (with-output-to-string (identifier)
                    (declare (type string-stream identifier))
                    (loop
                      while
                        (and character
                             (instruction-character-p character))
                      do
                        (write-char character identifier)
                        (advance)))))))
          
          (loop do
            (cond
              ((null character)
                (loop-finish))
              
              ((whitespace-character-p character)
                (skip-whitespaces))
              
              ((instruction-character-p character)
                (push (read-instruction) instructions))
              
              (T
                (error "Invalid character ~s at position ~d."
                  character position)))))))
    
    (the (simple-array instruction (*))
      (coerce (nreverse instructions)
        '(simple-array instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Interprets the ¢% INSTRUCTIONS and returns no value."
  (declare (type (simple-array instruction (*)) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)                     ;; Instruction pointer.
          (instruction (aref instructions 0)) ;; Current instruction.
          (stack       NIL))                  ;; Data stack.
      (declare (type fixnum                ip))
      (declare (type (or null instruction) instruction))
      (declare (type (stack-of integer)    stack))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION, and returns
             no value."
            (setf instruction
              (when (< ip (1- (length instructions)))
                (aref instructions (incf ip))))
            (values))
           
           (recede ()
            "Moves the instruction pointer IP to the previous
             instruction, if possible, updates the current INSTRUCTION,
             and returns no value."
            (setf instruction
              (when (plusp ip)
                (aref instructions (decf ip))))
            (values)))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:pop
              (pop stack)
              (advance))
            
            (:swap
              (rotatef (first stack) (second stack))
              (advance))
            
            (:duplicate
              (push (first stack) stack)
              (advance))
            
            (:move-up
              (setf stack (nreverse stack))
              (let ((bottom-element (pop stack)))
                (declare (type integer bottom-element))
                (setf stack (nreverse stack))
                (push bottom-element stack))
              (advance))
            
            (:start-loop
              (cond
                ((zerop (first stack))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case instruction
                      ((NIL)
                        (error "Unmatched loop start."))
                      (:start-loop
                        (incf level)
                        (advance))
                      (:end-loop
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            (:move-down
              (let ((top-element (pop stack)))
                (declare (type integer top-element))
                (setf stack (append stack (list top-element))))
              (advance))
            
            (:add
              (let ((augend (pop stack))
                    (addend (pop stack)))
                (declare (type integer augend))
                (declare (type integer addend))
                (push (+ augend addend) stack))
              (advance))
            
            (:subtract
              (let ((minuend    (pop stack))
                    (subtrahend (pop stack)))
                (declare (type integer minuend))
                (declare (type integer subtrahend))
                (push (- minuend subtrahend) stack))
              (advance))
            
            (:multiply
              (let ((multiplicand (pop stack))
                    (multiplier   (pop stack)))
                (declare (type integer multiplicand))
                (declare (type integer multiplier))
                (push (* multiplicand multiplier) stack))
              (advance))
            
            (:divide
              (let ((dividend (pop stack))
                    (divisor  (pop stack)))
                (declare (type integer dividend))
                (declare (type integer divisor))
                (push (round dividend divisor) stack))
              (advance))
            
            (:input-number
              (format T "~&Please input an integer: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (push input stack))
              (advance))
            
            (:input-character
              (format T "~&Please input a Unicode character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (push (char-code input) stack))
              (advance))
            
            (:push-1
              (push 1 stack)
              (advance))
            
            (:output-number
              (write (pop stack))
              (advance))
            
            (:output-character
              (write-char (code-char (pop stack)))
              (advance))
            
            (:end-loop
              (cond
                ((not (zerop (first stack)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case instruction
                      ((NIL)
                        (error "Unmatched loop end."))
                      (:start-loop
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (:end-loop
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            (otherwise
              (error "Unrecognized instruction: ~s." instruction)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-¢% (code)
  "Interprets the piece of ¢% CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (parse-¢% code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-¢% converter.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-¢% (brainfuck-code
                                &key (destination T))
  "Generates for the piece of BRAINFUCK-CODE an equivalent ¢% program
   and writes it to the DESTINATION, returning for a non-nil sink the
   ``NIL'' value and for a DESTINATION of ``NIL'' a fresh string
   containing the thus produced code.
   ---
   Characters not associated with a brainfuck command are simply
   ignored, and thus do not contribute to the output code."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  
  (if destination
    (let ((first-instruction-p T))
      (declare (type boolean first-instruction-p))
      (flet ((write-¢%-instruction (instruction)
              (declare (type string instruction))
              (if first-instruction-p
                (setf first-instruction-p NIL)
                (write-string " " destination))
              (write-string instruction destination)
              (values)))
        
        (loop for bf-token of-type character across brainfuck-code do
          (case bf-token
            ((NIL)
              (loop-finish))
            
            ;; Increment current cell.
            (#\+
              (write-¢%-instruction "%%¢¢ ¢%%¢"))
            
            ;; Decrement current cell.
            (#\-
              (write-¢%-instruction "%%¢¢ ¢¢¢% ¢%%%"))
            
            ;; Move memory pointer left.
            (#\<
              (write-¢%-instruction "¢¢%%"))
            
            ;; Move memory pointer right.
            (#\>
              (write-¢%-instruction "¢%¢%"))
            
            ;; Output current cell as a character.
            (#\.
              (write-¢%-instruction "¢¢%¢ %%%¢"))
            
            ;; Input character and store character code in current cell.
            (#\,
              (write-¢%-instruction "¢¢¢¢ %¢%%"))
            
            ;; Jump past matching "]" if current cell is zero.
            (#\[
              (write-¢%-instruction "¢%¢¢"))
            
            ;; Jump back past matching "[" if current cell is not zero.
            (#\]
              (write-¢%-instruction "%%%%"))
            
            ;; Ignore comment characters.
            (otherwise
              NIL)))))
    
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-¢% brainfuck-code :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-¢% "%¢%¢ ¢¢%¢ %%¢% ¢%¢¢ ¢¢%¢ %%¢% %%%%")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-¢% "%¢%% ¢¢%¢ %%%¢ ¢%¢¢ %¢%% ¢¢%¢ %%%¢ %%%%")

;;; -------------------------------------------------------

;; Convert the infinitely repeating brainfuck cat program
;;   ,.[,.]
;; to ¢% and interpret it.
(interpret-¢%
  (convert-brainfuck-to-¢% ",.[,.]" :destination NIL))
