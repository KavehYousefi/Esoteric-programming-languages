;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Numobin", invented by the Esolang user "Zzo38" and
;; presented on January 5th, 2007, the commorancy of its diorism the
;; tallying of the hitherto encountered hash signs ("#") and their
;; subsequent insertion on the memory stack, the same operates in
;; champarty with a registry of variables, and a Boolean flag.
;; 
;; 
;; Concept
;; =======
;; The Numobin programming language ostends a proprium woning in the
;; kenspeckle definition of its numeric literals via hash signs, the
;; "#" symbols, whose hitherto tallied quantity upon each encounter
;; is pushed onto the memory's stack, thilk, as a compernage which
;; completed a triad in componency, lays its amplection around a
;; variable registry and an appurtenant data castaldy object, and a
;; Boolean flag to whom the wike of the sole looping construct's
;; perpetuation or abortion is assigned.
;; 
;; == DATA IS REPRESENTED BY "#" INSTANCES ==
;; The statement of data literals in a Numobin ensues from the insertion
;; on hash signs, designated via "#" instances, acting in a concomitant
;; agency as an instruction that pushes the currently encountered tally
;; of this onto the memory's stack.
;; 
;; A hash counter, by default initialized to the inchoate value of zero
;; (0), but amenable in its state's configuration to a random selection
;; via the "?" operation, is consigned to the castaldy of the hitherto
;; visited "#" tokens. Upon such a symbol's consumption, the
;; contemporaneous counter state is pushed onto the stack, succeeded by
;; a consequent incrementation of the counter by a quantity of one (1).
;; Please heed that the counter state is *not* reset after communicating
;; its value to this salvatory.
;; 
;; == THE MEMORY: STACK + VARIABLES + BOOLEAN FLAG ==
;; The Numobin memory enumerates a treble of partages, identified as a
;; stack of integers and characters, a variable registry whose names and
;; values both resolve to the aforementioned twissel of species, and a
;; Boolean flag whose state serves as the antecedent to the looping
;; mechanism's entelechy.
;; 
;; == THE STACK: A SALVATORY FOR NON-NEGATIVE INTEGER AND CHARACTERS ==
;; Entalented with the most elevated mete of potentials among the
;; salvatories' triad, the stack component accoutres a theoretically
;; infinite storage for both non-negative integer numbers and ASCII
;; characters.
;; 
;; == THE VARIABLE REGISTRY: NAMED INTEGERS AND CHARACTERS ==
;; The variable registry's establishment constitutes such as to
;; associate with a non-negative integer number or ASCII character,
;; construed in a unique identifier's agency, a value desumed from the
;; selfsame realm.
;; 
;; == THE BOOLEAN FLAG: AN ITERANCE GUARD ==
;; Least in its componency's perimeter, a Boolean flag partakes of the
;; execution in a role whose exclusivity appertains to the enabling or
;; disabling of the sole control flow construct, signified by the
;; jumelle of "[" and "]" tokens.
;; 
;; 
;; Instructions
;; ============
;; Numobin's instruction set enumerates an undecimal cardinality, the
;; compass assigned to this competences amplects rudimentary
;; arithmetics, stack, variables and Boolean flag manipulations, input
;; and output communications, as well as an aefauld conditional control
;; flow mechanism.
;; 
;; Please heed that succedaneous tmemata are underlined by a catena of
;; carets ("^"), intended to be replaced by actual Numobin code in the
;; ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   #       | Pushes the current hash sign counter value onto the
;;           | stack and subsequently increments the counter.
;;   ..................................................................
;;   [ stm ] | While the Boolean flag is true, repeats the instructions
;;     ^^^   | {stm}.
;;           |---------------------------------------------------------
;;           | {stm} must be a sequence comprising zero or more
;;           | instructions.
;;   ..................................................................
;;   -       | Pops the top element, yclept "a" here, and expected to
;;           | represent a number, from the stack, pops the new top
;;           | element, "b", also required to be numeric in type,
;;           | supputates the absolute value of the difference (a - b),
;;           | and pushes this new number onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following holds:
;;           |   let left          <- pop from stack
;;           |   let right         <- pop from stack
;;           |   let absDifference <- abs(left - right)
;;           |   push difference onto stack
;;   ..................................................................
;;   {       | Pops the top stack element, here yclept "a", from the
;;           | stack, pops the new top element, "b", and associates
;;           | the variable with the name signified by the content of
;;           | "a" with the value of "b" in the variables registry.
;;   ..................................................................
;;   }       | Pops the top stack element, employs thilk as a variable
;;           | name, and pushes the value associated with this name
;;           | onto the stack.
;;   ..................................................................
;;   ~       | Swap the two top stack elements.
;;   ..................................................................
;;   *       | Toggles the Boolean flag's state.
;;   ..................................................................
;;   =       | Pops the two top numbers from the stack; if these are
;;           | equal, toggles the Boolean flag; otherwise accompasses
;;           | no causatum.
;;   ..................................................................
;;   (       | Pops the top element from the stack and discards thilk.
;;   ..................................................................
;;   )       | Queries the standard input conduit for either an
;;           | unsigned integer number greater than or equal to zero
;;           | (0), or a single ASCII character and pushes the received
;;           | object onto the stack.
;;   ..................................................................
;;   ?       | Sets the hash sign counter to a random integer number
;;           | greater than or equal to zero (0), with no natural
;;           | bourne concerning its mickleness.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, its operations' entelechia being
;; adhibited immediately on the provided Numobin source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-07-09
;; 
;; Sources:
;;   [esolang2022Numobin]
;;   The Esolang contributors, "Numobin", July 10th, 2022
;;   URL: "https://esolangs.org/wiki/Numobin"
;;   Notes:
;;     - Original specification of the Numobin programming language.
;;   
;;   [esolang2022Truth-machine]
;;   The Esolang contributors, "Truth-machine", July 10th, 2022
;;   URL: "https://esolangs.org/wiki/Truth-machine#Numobin"
;;   Notes:
;;     - Implementation of a truth-machine in Numobin.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   entries, each key of which conforms to the KEY-TYPE and is associated
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

(deftype jump-table ()
  "The ``jump-table'' type defines a hash table of zero or more entries,
   mapping fixnum keys to values of the same set, with the keys
   interpreted as the position of either a loop start or end position in
   an appertaining instruction sequence, associated with the position
   immediately following the opposite loop boundary."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an unsigned integer number
   occupying the range with zero (0) as its lower extremum, but wisting
   of no upper bourne."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype numobin-object ()
  "The ``numobin-object'' type embraces the homologated data types in
   currency during a Numobin program's execution."
  '(or character non-negative-integer))

;;; -------------------------------------------------------

(deftype variable-set ()
  "The ``variable-set'' type defines a mapping betwixt variable names
   and values, manifesting in a hash table which associates a
   ``numobin-object'' key with exactly one value of the same type."
  '(hash-table-of numobin-object numobin-object))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a list in the agency of a last-in
   first-out (LIFO) data structure, intended to maintain instances of
   the ``numobin-object'' type."
  '(list-of numobin-object))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Creates and returns a jump table based upon the iteration facilities
   present in the piece of Numobin CODE.
   ---
   The thus generated hash table associates with each loop start token,
   represented by its index into the CODE, the position in the CODE
   immediately following the matching loop end token. Conversely, a loop
   end token, again defined by its index alone, maps to the exact
   position of its loop start. A loop inciting index thus skips over its
   terminating brace, when moved to the corresponding position, while an
   iteration terminator returns to the respective starter for a new
   continuation predicate check and potential repetition."
  (declare (type string code))
  (let ((jump-table  (make-hash-table :test #'eql))
        (loop-starts NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) loop-starts))
    
    (loop
      for character of-type character across code
      and position  of-type fixnum    from   0
      do
        (case character
          (#\[
            (push position loop-starts))
          
          (#\]
            (if loop-starts
              (let ((start-position (pop loop-starts)))
                (declare (type fixnum start-position))
                ;; Connect the loop start with the position immediately
                ;; following its matching end.
                (setf (gethash start-position jump-table)
                      (1+ position))
                
                ;; Connect the loop end with the position of its
                ;; matching start.
                (setf (gethash position jump-table)
                      start-position))
              (error "Unmatched loop end \"]\" at position ~d."
                position)))
          
          (otherwise
            NIL)))
    
    (when loop-starts
      (error "Unmatched loop start~p \"[\" at position~:p ~{~d~^, ~}."
        (length loop-starts)
        loop-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun validate-as-non-negative-integer (candidate)
  "Determines whether the CANDIDATE represents a non-negative integer
   number greater than or equal to zero (0), with no upper march to its
   mickleness, returning on confirmation the CANDIDATE itself; otherwise
   an error of an unspecified type is signaled."
  (declare (type integer candidate))
  (the non-negative-integer
    (if (minusp candidate)
      (error "Expected a non-negative integer, but encountered ~d."
        candidate)
      candidate)))

;;; -------------------------------------------------------

(defun integer-string-p (string)
  "Checks whether the STRING represents a valid signed integer number,
   returning on confirmation the parsed numeric value, otherwise
   ``NIL''."
  (declare (type string string))
  (let ((parsed-integer
          (ignore-errors
            (parse-integer string))))
    (declare (type (or null non-negative-integer) parsed-integer))
    (the (or null non-negative-integer)
      (and parsed-integer
           (validate-as-non-negative-integer parsed-integer)))))

;;; -------------------------------------------------------

(defun empty-string-p (string)
  "Checks whether the STRING is empty, that is, exhibits a length of
   zero character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string string))
  (the boolean
    (zerop
      (length string))))

;;; -------------------------------------------------------

(defun blank-string-p (string)
  "Checks whether the STRING is blank, that is, composed of zero or more
   spaces only without any other content, returning on confirmation a
   single space character, otherwise ``NIL''."
  (declare (type string string))
  (the (or null character)
    (and
      (zerop (length (string-trim " " string)))
      #\Space)))

;;; -------------------------------------------------------

(defun singleton-string-p (string)
  "Checks whether the STRING is composed of exactly one character,
   returning on confirmation this single constituent as a ``character''
   object, otherwise responding with ``NIL''."
  (declare (type string string))
  (let ((trimmed-string (string-trim " " string)))
    (declare (type string trimmed-string))
    (the (or null character)
      (when (= (length trimmed-string) 1)
        (char trimmed-string 0)))))

;;; -------------------------------------------------------

(defun parse-input (input)
  "Evaluates the INPUT string and returns the object most appropriate
   for its form.
   ---
   If the INPUT cannot be matched against any recognized pattern, an
   error of an unspecified type is signaled."
  (declare (type string input))
  (or
    (and (empty-string-p input) #\Newline)
    (blank-string-p     input)
    (integer-string-p   input)
    (singleton-string-p input)
    (error "Invalid input: ~s. Neither character nor number." input)))

;;; -------------------------------------------------------

(defun interpret-Numobin (code
                          &key (random-upper-bound 256))
  "Interprets the piece of Numobin CODE and returns no value.
   ---
   The RANDOM-UPPER-BOUND parameter may be adjusted to defines an
   exclusive upper bound for the random hash sign offset calculator
   involved in the interpretation of the \"?\" command. The thus
   occupied integer range amounts to [0, RANDOM-UPPER-BOUND)."
  (declare (type string        code))
  (declare (type (integer 1 *) random-upper-bound))
  
  (when (plusp (length code))
    (let ((ip           0)
          (token        (char code 0))
          (jump-table   (build-jump-table code))
          (stack        NIL)
          (boolean-flag NIL)
          (variables    (make-hash-table :test #'eql))
          (hash-counter 0))
      (declare (type fixnum              ip))
      (declare (type (or null character) token))
      (declare (type jump-table          jump-table))
      (declare (type stack               stack))
      (declare (type boolean             boolean-flag))
      (declare (type variable-set        variables))
      (declare (type (integer 0 *)       hash-counter))
      
      (setf *random-state* (make-random-state T))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the CODE, if possible, updates the current TOKEN, and
             returns no value."
            (setf token
              (when (array-in-bounds-p code (1+ ip))
                (char code (incf ip))))
            (values))
           
           (move-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION,
             updates the current TOKEN, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf token
              (when (array-in-bounds-p code ip)
                (char code ip)))
            (values))
           
           (jump-to-label ()
            "Moves the instruction pointer IP to the jump destination in
             the CODE opposite to the current location, updates the
             current TOKEN, and returns no value."
            (move-to (gethash ip jump-table))
            (values))
           
           
           (get-variable-value (variable-name)
            "Returns the value of the variable identified by the
             VARIABLE-NAME, or signals an error of an unspecified type
             if the same cannot be detected."
            (declare (type numobin-object variable-name))
            (the numobin-object
              (or
                (gethash variable-name variables)
                (error "No variable with the name ~s could be found."
                  variable-name))))
           
           (set-variable (variable-name variable-value)
            "Associates the VARIABLE-NAME with the VARIABLE-VALUE,
             either creating a new variable with these attributes or
             updating an extant instance, and returns no value."
            (declare (type numobin-object variable-name))
            (declare (type numobin-object variable-value))
            (setf (gethash variable-name variables) variable-value)
            (values))
           
           
           (check-if-stack-is-empty ()
            "Determines whether the stack is empty, on confirmation
             signaling an error of an unspecified type; otherwise
             accompasses no causatum and returns no value."
            (unless stack
              (error "Cannot pop from an empty stack."))
            (values))
           
           (pop-number ()
            "Pops the topmost value from the STACK, expecting it to be
             an integer number, either returning the same upon
             confirmation or signaling an error of unspecified type if
             the removed element does not constitute a number."
            (check-if-stack-is-empty)
            (let ((top-element (pop stack)))
              (declare (type numobin-object top-element))
              (unless (integerp top-element)
                (error "The top stack element is not a number."))
              (the non-negative-integer top-element)))
           
           (pop-value ()
            "Pops the topmost value from the STACK, expecting it to be
             either a character or an integer number, and returns the
             same"
            (check-if-stack-is-empty)
            (the numobin-object
              (pop stack))))
        
        (loop while token do
          (case token
            ;; End of code.
            ((NIL)
              (loop-finish))
            
            ;; Push hash count.
            (#\#
              (push hash-counter stack)
              (incf hash-counter)
              (advance))
            
            ;; Jump forward.
            (#\[
              (if boolean-flag
                (advance)
                (jump-to-label)))
            
            ;; Jump back.
            (#\]
              (jump-to-label))
            
            ;; Push absolute difference.
            (#\-
              (let ((left-operand  (pop-number))
                    (right-operand (pop-number)))
                (declare (type non-negative-integer left-operand))
                (declare (type non-negative-integer right-operand))
                (push (abs (- left-operand right-operand)) stack))
              (advance))
            
            ;; Define variable.
            (#\{
              (let ((variable-value (pop-value))
                    (variable-name  (pop-value)))
                (declare (type numobin-object variable-value))
                (declare (type numobin-object variable-name))
                (set-variable variable-name variable-value))
              (advance))
            
            ;; Push variable value.
            (#\}
              (let ((variable-name (pop-value)))
                (declare (type numobin-object variable-name))
                (push (get-variable-value variable-name) stack))
              (advance))
            
            ;; Swap top stack elements.
            (#\~
              (rotatef (first stack) (second stack))
              (advance))
            
            ;; Toggle boolean value.
            (#\*
              (setf boolean-flag (not boolean-flag))
              (advance))
            
            ;; Toggle boolean value if equal.
            (#\=
              (let ((first-number  (pop-number))
                    (second-number (pop-number)))
                (declare (type non-negative-integer first-number))
                (declare (type non-negative-integer second-number))
                (when (= first-number second-number)
                  (setf boolean-flag (not boolean-flag))))
              (advance))
            
            ;; Pop and output.
            (#\(
              (let ((top-element (pop-value)))
                (declare (type numobin-object top-element))
                (format T "~a" top-element))
              (advance))
            
            ;; Input and push.
            (#\)
              (format T "~&Please enter either an ASCII character ~
                           or a non-negative integer: ")
              (finish-output)
              (let ((input (read-line)))
                (declare (type string input))
                (clear-input)
                (push (parse-input input) stack))
              (advance))
            
            ;; Set random hash counter offset.
            (#\?
              (setf hash-counter
                (random random-upper-bound))
              (advance))
            
            ;; All other characters are prohibited.
            (otherwise
              (error "Invalid character \"~c\" at position ~d."
                token ip)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate and print the number 0.
(interpret-Numobin "##-##--(")

;;; -------------------------------------------------------

;; Generate and print the number 1.
(interpret-Numobin "##-(")

;;; -------------------------------------------------------

;; Generate and print the number 2.
(interpret-Numobin "###~#=-(")

;;; -------------------------------------------------------

;; Generate and print the number 3.
(interpret-Numobin "###=#-(")

;;; -------------------------------------------------------

;; Generate and print the number 4.
(interpret-Numobin "###-#=#-(")

;;; -------------------------------------------------------

;; Generate and print the number 5.
(interpret-Numobin "##-##=##=#-(")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Numobin ")#=*[##-(]##-##--(")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Numobin "*[)(]")

;;; -------------------------------------------------------

;; Prompt for a user input and store it in the variable named with the
;; integer identifier "1", query the variable's value, and print it to
;; the standard output.
(interpret-Numobin "##-){##-}(")
