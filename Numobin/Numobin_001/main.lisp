;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Numobin", invented by the Esolang user "Zzo38".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-07-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Numobin"
;;   -> "https://esolangs.org/wiki/Truth-machine#Numobin"
;;       o Implementation of a truth-machine in Numobin.
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

(deftype numobin-object ()
  "The ``numobin-object'' type embraces the homologated data types in
   currency during a Numobin program's execution."
  '(or character integer))

;;; -------------------------------------------------------

(deftype variable-set ()
  "The ``variable-set'' type defines a hash table which associates a
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
    (declare (type jump-table jump-table))
    (declare (type list       loop-starts))
    
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
      (error "Unmatched loop starts \"[\" at positions ~{~d~^, ~}."
        loop-starts))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun integer-string-p (string)
  "Checks whether the STRING represents a valid signed integer number,
   returning on confirmation the parsed numeric value, otherwise
   ``NIL''."
  (declare (type string string))
  (the (or null integer)
    (handler-case
      (parse-integer string)
      (error ()
        NIL))))

;;; -------------------------------------------------------

(defun empty-string-p (string)
  "Checks whether the STRING is empty, that is, exhibits a length of
   zero character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string string))
  (the boolean (zerop (length string))))

;;; -------------------------------------------------------

(defun blank-string-p (string)
  "Checks whether the STRING is blank, that is, composed of zero or more
   spaces only without any other content, returning on confirmation a
   single space character, otherwise ``NIL''."
  (declare (type string string))
  (the (or null character)
    (when (zerop (length (string-trim " " string)))
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
      
      (setf *random-state* (make-random-state))
      
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
              (unless (typep top-element 'integer)
                (error "The top stack element is not a number."))
              (the integer top-element)))
           
           (pop-value ()
            "Pops the topmost value from the STACK, expecting it to be
             either a character or an integer number, and returns the
             same"
            (check-if-stack-is-empty)
            (the numobin-object (pop stack))))
        
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
                (declare (type integer left-operand))
                (declare (type integer right-operand))
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
                (declare (type integer first-number))
                (declare (type integer second-number))
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
                           or an integer: ")
              (let ((input (read-line)))
                (declare (type T input))
                (clear-input)
                (push (parse-input input) stack))
              (advance))
            
            ;; Set random hash counter offset.
            (#\?
              (setf hash-counter (random random-upper-bound))
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
