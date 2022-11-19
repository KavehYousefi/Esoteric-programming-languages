;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Real Brainfuck", invented by the Esolang user "Delta23" in
;; the year 2020, and intended as a variant of Urban Mueller's original
;; "brainfuck" extended to real-valued, or, in a latter rendition,
;; complex-valued, tape cells, while concomitantly introducing a stack
;; of the same element type as a further adminiculum.
;; 
;; Instructions
;; ============
;; The Real Brainfuck instruction set is compact of a total of 29
;; members, subsumed into two variants: an original whose perimeter
;; embraces one less than the sum's moeity, and an augmented supplement
;; exhausting the 15 remaining participants.
;; 
;; Whereas the first iteration comprehended merely real-valued cells,
;; the enhancement incorporates complex numbers, in conjunction with
;; both basic and advanced mathematical concepts.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer some cursory nortelry anenst
;; the language's operational faculties.
;; 
;; The original Real Brainfuck instruction set tallies 14 members:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   <       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   >       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   0       | Sets the current cell to the half of its value c:
;;           |   c = (c + 0) / 2
;;   ..................................................................
;;   1       | Increments the current cell and sets it to the half of
;;           | its value c:
;;           |   c = (c + 1) / 2
;;   ..................................................................
;;   N       | Sets the current cell to one minus its value c:
;;           |   c = 1 - c
;;   ..................................................................
;;   [       | Utilizing the current cell value c as a probability,
;;           | randomly selects an integer from {0, 1}. If the result
;;           | equals zero (0), moves the instruction pointer forward
;;           | to the position immediately following the matching "]".
;;           | Otherwise, proceeds as usual.
;;   ..................................................................
;;   ]       | Utilizing the current cell value c as a probability,
;;           | randomly selects an integer from {0, 1}. If the result
;;           | equals one (1), moves the instruction pointer back to
;;           | the position immediately following the matching "[".
;;           | Otherwise, proceeds as usual.
;;   ..................................................................
;;   ^       | Pushes the current cell value unto the top of the stack.
;;   ..................................................................
;;   V       | Pops the top element from the stack and stores it into
;;           | the current cell.
;;   ..................................................................
;;   &       | Pops the two top items a and b from the stack,
;;           | mulitplies them, and stores the product into the current
;;           | cell c:
;;           |   c = a * b
;;   ..................................................................
;;   X       | Pops the two top items a and b from the stack, computes
;;           | their average, and stores the result into the current
;;           | cell c:
;;           |   c = (a + b) / 2
;;   ..................................................................
;;   ,       | Queries a source for a real number or a binary value and
;;           | stores the input into the current cell.
;;           | The source as well as the input format constitute an
;;           | implementation-dependent topic.
;;   ..................................................................
;;   .       | Outputs the current cell value as a real number.
;;   ..................................................................
;;   q       | Utilizing the current cell value c as a probability,
;;           | outputs an integer from {0, 1} with the probability c.
;;   ------------------------------------------------------------------
;; 
;; The Real Brainfuck 2 Variant adhibits an augmentation of the native
;; instruction set in order to admit complex real number and more
;; sophisticated operations, extending the roster by 15 new items.
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value c by one:
;;           |   c = c + 1
;;   ..................................................................
;;   -       | Decrements the current cell value c by one:
;;           |   c = c - 1
;;   ..................................................................
;;   ;       | Sets the current cell value c to the c-th root of c:
;;           |   c = c^(1/c)
;;   ..................................................................
;;   \       | Pops the two top items a and b from the stack, computes
;;           | the logarithm of b to the base a, and stores the result
;;           | into the current cell c:
;;           |   c = log_{a}(b)
;;   ..................................................................
;;   /       | Sets the current cell value c to its sine:
;;           |   c = sin(c)
;;   ..................................................................
;;   z       | Sets the current cell value c to itself raised by the
;;           | mathematical constant pi as its power:
;;           |   c = c^{pi}
;;   ..................................................................
;;   :       | Pops the two top items a and b from the stack, computes
;;           | the result of a raised to the power of b, and stores the
;;           | result into the current cell c:
;;           |   c = a^b
;;   ..................................................................
;;   f       | Sets the current cell value c to the tetration of
;;           | itself as the base, raised to the power of two (2):
;;           |   c = c^^2
;;   ..................................................................
;;   e       | Stores the transcendental number e into the current
;;           | cell:
;;           |   c = e
;;   ..................................................................
;;   p       | Stores the transcendatal number pi into the current
;;           | cell:
;;           |   c = pi
;;   ..................................................................
;;   !       | Sets the current cell value c to its multiplication
;;           | factorial, also known simply as the factorial:
;;           |   c = c!
;;           |     = 1 * 2 * ... * c
;;   ..................................................................
;;   ?       | Sets the current cell value c to its addition factorial,
;;           | also known as its triangular number:
;;           |   c = 1 + 2 + ... + c
;;   ..................................................................
;;   $       | Sets the current cell value c to its exponential
;;           | factorial
;;           |   c = n^{(n-1)^{(n-2)^...^{1}}}
;;   ..................................................................
;;   {       | Starts an infinite loop.
;;   ..................................................................
;;   }       | Terminates an infinite loop.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Real Brainfuck language's standardizing preterotype, despite its
;; thorough treatment, germinates a few undefined or underspecified
;; topics, a subset of which shall be the coming explications' cynosure.
;; 
;; == COMPLEX INPUT AND OUTPUT ==
;; The Real Brainfuck standard's extension by its second variant,
;; targeted at the instalment of complex numbers and the introduction of
;; both basic and advanced features, does not adhibit the same mete of
;; novelty to the input and output facilities.
;; 
;; Both the obtention and display still relate to real value; still, a
;; statement regarding the response to complex-valued input and output
;; is lacking.
;; 
;; It has thus been chosen to retain the real-valued input verbatim, but
;; extend the output functionality to permit complex printing, assuming
;; the format
;; 
;;   (realPart, complexPart).
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is designed in the Common Lisp programming
;; language.
;; 
;; Its perimeter's amplectation of complex numbers capacitates this
;; particular Lisp dialect to operate partially on the prepotence of the
;; newer Real Brainfuck rendition. Maugre this, particular complexities
;; in some operations have been avoided by resorting to the more common
;; integer or real-valued variants, in particular with reference to the
;; factorial functions.
;; 
;; A ramification of this compromise, this implementation does not obey
;; with veridical patration to the stringencies imposed by the Real
;; Brainfuck standard.
;; 
;; 
;; Future Extensions
;; =================
;; Its explicitly declaimed state as a simple implementation does not
;; exonerate the project from the facts of its frailties. A consectary
;; thereof, a selected set of potentials for future enhancements shall
;; follow.
;; 
;; == EXTENSIONS TO COMPLEX NUMBERS ==
;; Certain mathematical operations incorporated into Real Brainfuck
;; exist in common apprehension as defined either on the integer or real
;; plane --- maugre the, usually less acquainted, definition for the
;; prepotent complex realm. Specimens of this sort include in particular
;; the multifarious factorial functions.
;; 
;; While hitherto realized for integers, an implementation more fidel to
;; the language standard could supplement the complex counterpart. The
;; multiplication factorial, as a concrete instance, can be extended
;; to cater for the complex range by harnessing the integral-based
;; gamma function.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Real_Brainfuck"
;;   -> "https://en.wikipedia.org/wiki/Tetration"
;;       o Describes tetration.
;;   -> "https://mathvault.ca/derivative-tetration-hyperexponentiation/#Tetration_and_Related_Concepts"
;;       o Describes tetration.
;;   -> "https://en.wikipedia.org/wiki/Gamma_function"
;;       o Describes the gamma function as an extension of the factorial
;;         function to complex numbers.
;;   -> "https://en.wikipedia.org/wiki/Triangular_number"
;;       o Describes the addition factorial or triangular number.
;;   -> "https://oeis.org/A049384"
;;       o Describes the exponential factorial.
;;   -> "https://stackoverflow.com/questions/8183840/probability-in-java"
;;       o Random and probability implemented in the Java programming
;;         language.
;;   -> "https://stackoverflow.com/questions/26953460/75-probability-in-java"
;;       o Random and probability implemented in the Java programming
;;         language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized Real Brainfuck
   instructions."
  '(member
    
    ;; Real Brainfuck instructions:
    :move-left            ;; <
    :move-right           ;; >
    :halve                ;; 0 | c = (c + 0) / 2
    :increment-and-halve  ;; 1 | c = (c + 1) / 2
    :one-minus            ;; N | c = 1 - c
    :jump-forward         ;; [
    :jump-back            ;; ]
    :push-to-stack        ;; ^
    :pop-from-stack       ;; V
    :multiply             ;; &
    :average              ;; X
    :input                ;; ,
    :output-cell          ;; .
    :output-random-bit    ;; q
    
    ;; Real Brainfuck 2 instructions:
    :increment                  ;; +
    :decrement                  ;; -
    :root                       ;; ;
    :log                        ;; \
    :sin                        ;; /
    :raise-to-pi                ;; z
    :power                      ;; : | c = a^b, with a and b from stack
    :tetrate                    ;; f | c = c^^2
    :set-to-e                   ;; e | c = e, with e being Euler's number
    :set-to-pi                  ;; p | c = pi
    :multiplication-factorial   ;; ! | c = c!
    :addition-factorial         ;; ? | c = c?
    :exponential-factorial      ;; $ | c = c$
    :begin-infinite-loop        ;; {
    :end-infinite-loop))        ;; }

;;; -------------------------------------------------------

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

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a vector of zero or more
   commands."
  '(vector command *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping from forward jump to back
   jump positions, and vice versa, as well as from infinite loop start
   to iteration terminator locations, again in a symmetrical fashion,
   represented by a hash table that associates the respective
   instruction indices in a given instruction vector context."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a potentially infinite collection of
   cells, associating the cell index as the key of a hash table to the
   cell value."
  '(hash-table-of integer number))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list-based stack of zero or more
   numbers."
  '(list-of number))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character command) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+ (make-hash-table :test #'eql)
  "Associates the instruction identifier tokens with the respective
   command objects.")

;;; -------------------------------------------------------

(flet ((add-identifier (token command)
        "Associates the TOKEN with the COMMAND and returns no value."
        (declare (type character token))
        (declare (type command   command))
        (setf (gethash token +IDENTIFIERS+) command)
        (values)))
  
  ;; Real Brainfuck instructions:
  (add-identifier #\< :move-left)
  (add-identifier #\> :move-right)
  (add-identifier #\0 :halve)
  (add-identifier #\1 :increment-and-halve)
  (add-identifier #\N :one-minus)
  (add-identifier #\[ :jump-forward)
  (add-identifier #\] :jump-back)
  (add-identifier #\^ :push-to-stack)
  (add-identifier #\V :pop-from-stack)
  (add-identifier #\& :multiply)
  (add-identifier #\X :average)
  (add-identifier #\, :input)
  (add-identifier #\. :output-cell)
  (add-identifier #\q :output-random-bit)
  
  ;; Real Brainfuck 2 instructions:
  (add-identifier #\+ :increment)
  (add-identifier #\- :decrement)
  (add-identifier #\; :root)
  (add-identifier #\\ :log)
  (add-identifier #\/ :sin)
  (add-identifier #\z :raise-to-pi)
  (add-identifier #\: :power)
  (add-identifier #\f :tetrate)
  (add-identifier #\e :set-to-e)
  (add-identifier #\p :set-to-pi)
  (add-identifier #\! :multiplication-factorial)
  (add-identifier #\? :addition-factorial)
  (add-identifier #\$ :exponential-factorial)
  (add-identifier #\{ :begin-infinite-loop)
  (add-identifier #\} :end-infinite-loop)
  
  (values))

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Checks whether the TOKEN associates with a Real Brainfuck command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash token +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun get-command-for-token (token)
  "Returns the Real Brainfuck command associated with the TOKEN, or
   signals an error of an unspecified type in the case of a missing
   correspondence."
  (declare (type character token))
  (the command
    (or (gethash token +IDENTIFIERS+)
        (error "No command token: ~s." token))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of Real Brainfuck CODE a
   one-dimensional simple array of instructions.
   ---
   Non-command characters in the CODE are ignored and as such do not
   contribute to the result."
  (declare (type string code))
  (the (simple-array command (*))
    (coerce
      (loop
        for     token of-type character across code
        when    (command-token-p token)
        collect (get-command-for-token token))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of random number operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-random-boolean (probability)
  "Returns with the PROBABILITY either a ``boolean'' value of ``T'' or
   ``NIL''."
  (declare (type real probability))
  (the boolean
    (not (null
      (< (random 1.0) probability)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tetrate (x n)
  "Calculates and returns the tetration of the base X raised N times to
   itself as its exponent.
   ---
   In common nomenclature, X is yclept the \"base\", whereas N provides
   the \"height\"."
  (declare (type number        x))
  (declare (type (integer 0 *) n))
  (let ((result 1))
    (declare (type number result))
    (loop repeat n do
      (setf result
        (expt x result)))
    (the number result)))

;;; -------------------------------------------------------

(defun multiplication-factorial (n)
  "Calculates and returns the multiplication factorial of the
   non-negative integer N."
  (declare (type (integer 0 *) n))
  (the (integer 1 *)
    (if (zerop n)
      1
      (loop
        for     result of-type (integer 1 *) =    1 then (* result i)
        for     i      of-type (integer 1 *) from 1 to n
        finally (return result)))))

;;; -------------------------------------------------------

(defun addition-factorial (n)
  "Calculates and returns the additional factorial, or triangular
   number, of the non-negative integer N."
  (declare (type (integer 0 *) n))
  (the (integer 0 *)
    (loop for i of-type (integer 0 *) from 0 to n sum i)))

;;; -------------------------------------------------------

(defun exponential-factorial (n)
  "Calculates and returns the exponential factorial of the positive
   integer N.
   ---
   The exponential factorial can be defined recursively in the following
   manner:
     a[0] = 1
     a[n] = i^a[n-1], for 1 <= i <= n."
  (declare (type (integer 1 *) n))
  (let ((result 1))
    (declare (type (integer 1 *) result))
    (loop for i of-type (integer 1 *) from 1 to n do
      (setf result (expt i result)))
    (the (integer 1 *) result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type real +DEFAULT-CELL-VALUE+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CELL-VALUE+ 0.5
  "The initial value assumed by each cell on the tape.")

;;; -------------------------------------------------------

(defstruct (Tape
  (:constructor make-tape ()))
  "Represents an bilaterally infinite tape composed of real- or
   complex-valued cells, one among the same being designated as the
   active or current member by the cell or data pointer."
  (cells   (make-hash-table :test #'eql) :type memory)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun tape-current-cell (tape)
  "Returns the value stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the number
    (gethash (tape-pointer tape) (tape-cells tape)
      +DEFAULT-CELL-VALUE+)))

;;; -------------------------------------------------------

(defun (setf tape-current-cell) (new-value tape)
  "Stores the NEW-VALUE into the TAPE's current cell and returns the
   value."
  (declare (type number new-value))
  (declare (type Tape   tape))
  (setf (gethash (tape-pointer tape) (tape-cells tape)
          +DEFAULT-CELL-VALUE+)
        new-value)
  (the number
    (gethash (tape-pointer tape) (tape-cells tape))))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Moves the TAPE's cell pointer one step to the left and returns the
   modified TAPE."
  (declare (type Tape tape))
  (decf (tape-pointer tape))
  (the Tape tape))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Moves the TAPE's cell pointer one step to the right and returns the
   modified TAPE."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (the Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Generates and returns the jump table for the INSTRUCTIONS,
   affiliating with each forward jump index in the same the matching
   back jump position, and vice versa, and also associating similiter
   infinite loop start and end indices."
  (declare (type instruction-vector instructions))
  
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL)
        (loop-start-points   NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (declare (type (list-of fixnum) loop-start-points))
    
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0
      do
        (case instruction
          (:jump-forward
            (push position forward-jump-points))
          
          (:jump-back
            (if forward-jump-points
              (let ((jump-start (pop forward-jump-points))
                    (jump-end   position))
                (declare (type fixnum jump-start))
                (declare (type fixnum jump-end))
                (setf (gethash jump-start jump-table) jump-end)
                (setf (gethash jump-end   jump-table) jump-start))
              (error "Unmatched jump end instruction \"]\" at ~
                      position ~d."
                position)))
          
          (:start-infinite-loop
            (push position loop-start-points))
          
          (:end-infinite-loop
            (if loop-start-points
              (let ((loop-start (pop loop-start-points))
                    (loop-end   position))
                (declare (type fixnum loop-start))
                (declare (type fixnum loop-end))
                (setf (gethash loop-start jump-table) loop-end)
                (setf (gethash loop-end   jump-table) loop-start))
              (error "Unmatched loop end instruction \"}\" at ~
                      position ~d."
                position)))
          
          (otherwise
            NIL)))
    
    (when forward-jump-points
      (error "Unmatched forward jump instruction(s) \"[\" at ~
              positions ~{~d~^, ~}"
        forward-jump-points))
    
    (when loop-start-points
      (error "Unmatched loop start instruction(s) \"{\" at positions ~
              ~{~d~^, ~}."
        loop-start-points))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the Real Brainfuck INSTRUCTIONS and returns no value."
  (declare (type instruction-vector instructions))
  
  (when instructions
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (tape                (make-tape))
          (stack               NIL))
      (declare (type fixnum            ip))        
      (declare (type (or null command) current-instruction))
      (declare (type jump-table        jump-table))
      (declare (type Tape              tape))
      (declare (type stack             stack))
      
      (setf *random-state*              (make-random-state T))
      (setf *read-default-float-format* 'long-float)
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Moves the instruction pointer to the jump endpoint or
             infinite loop demarcation associated with the instruction
             pointer (IP) location, updates the CURRENT-INSTRUCTION, and
             returns no value."
            (setf ip (gethash ip jump-table))
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (pop-from-stack ()
            "Pops the top element from the STACK and returns it; or
             signals an error of an unspecified type if the STACK is
             empty."
            (the number
              (or (pop stack)
                  (error "Cannot pop from an empty stack."))))
           
           (push-to-stack (new-element)
            "Pushes the NEW-ELEMENT unto the STACK's top and returns no
             value."
            (declare (type number new-element))
            (push new-element stack)
            (values))
           
           (random-zero-chosen-p ()
            "Based upon the current cell value as the probablity,
             randomly selects either a value of zero (0) or one (1) and
             returns a ``boolean'' value of ``T'' in the former case,
             and ``NIL'' in the latter."
            (the boolean
              (not
                (get-random-boolean
                  (realpart
                    (tape-current-cell tape)))))))
        
        (symbol-macrolet
            ((current-cell
              (the number
                (tape-current-cell tape))))
          
          (loop while current-instruction do
            (case current-instruction
              ;; End of program.
              ((NIL)
                (loop-finish))
              
              ;; <
              (:move-left
                (tape-move-left tape))
              
              ;; >
              (:move-right
                (tape-move-right tape))
              
              ;; 0
              (:halve
                (setf current-cell
                      (/ current-cell 2)))
              
              ;; 1
              (:one-plus-halve
                (setf current-cell
                      (/ (1+ current-cell) 2)))
              
              ;; N
              (:one-minus
                (setf current-cell (- 1 current-cell)))
              
              ;; [
              (:jump-forward
                (when (random-zero-chosen-p)
                  (jump-to-opposite-boundary)))
              
              ;; ]
              (:jump-back
                (unless (random-zero-chosen-p)
                  (jump-to-opposite-boundary)))
              
              ;; ^
              (:push-to-stack
                (push-to-stack current-cell))
              
              ;; V
              (:pop-from-stack
                (setf current-cell (pop-from-stack)))
              
              ;; &
              (:multiply
                (push-to-stack
                  (* (pop-from-stack)
                     (pop-from-stack))))
              
              ;; X
              (:average
                (push-to-stack
                  (/ (+ (pop-from-stack)
                        (pop-from-stack))
                     2)))
              
              ;; ,
              (:input
                (format T "~&Please input a real number: ")
                (setf current-cell (read-from-string (read-line)))
                (clear-input))
              
              ;; .
              (:output-cell
                (format T "~a " current-cell))
              
              ;; q
              (:output-random-bit
                (format T "~a "
                  (if (random-zero-chosen-p) 0 1)))
              
              
              ;; +
              (:increment
                (incf current-cell))
              
              ;; -
              (:decrement
                (decf current-cell))
              
              ;; ;
              (:root
                (setf current-cell
                  (expt current-cell (/ 1 current-cell))))
              
              ;; \
              (:log
                (let ((base   (pop-from-stack))
                      (number (pop-from-stack)))
                  (declare (type number base))
                  (declare (type number number))
                  (setf current-cell (log base number))))
              
              ;; /
              (:sin
                (setf current-cell (sin current-cell)))
              
              ;; z
              (:raise-to-pi
                (setf current-cell (expt current-cell PI)))
              
              ;; :
              (:power
                (let ((base     (pop-from-stack))
                      (exponent (pop-from-stack)))
                  (declare (type number base))
                  (declare (type number exponent))
                  (setf current-cell (expt base exponent))))
              
              ;; f
              (:tetrate
                (setf current-cell (tetrate current-cell 2)))
              
              ;; e
              (:set-to-e
                (setf current-cell (exp 1)))
              
              ;; p
              (:set-to-pi
                (setf current-cell PI))
              
              ;; !
              (:multiplication-factorial
                (setf current-cell
                  (multiplication-factorial
                    (round current-cell))))
              
              ;; ?
              (:addition-factorial
                (setf current-cell
                  (addition-factorial
                    (round current-cell))))
              
              ;; $
              (:exponential-factorial
                (setf current-cell
                  (exponential-factorial
                    (round current-cell))))
              
              ;; {
              (:start-infinite-loop
                NIL)
              
              ;; }
              (:end-infinite-loop
                (jump-to-opposite-boundary))
              
              (otherwise
                (error "Invalid instruction ~s at position ~d."
                  current-instruction ip)))
            
            (advance))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Real-Brainfuck (code)
  "Interprets the piece of Real Brainfuck CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating numeric cat program, which may randomly terminate depending
;; on the input.
(interpret-Real-Brainfuck ",.[,.]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Real-Brainfuck ",.[.]")

;;; -------------------------------------------------------

;; Print the first five powers of pi, that is:
;; 
;;   3.5744317230367653
;;   4.574431723036765
;;   5.574431723036765
;;   6.574431723036765
;;   7.574431723036765
;;   8.574431723036765
(interpret-Real-Brainfuck
  "+++++
   >+z
   <[>.+<-]")
