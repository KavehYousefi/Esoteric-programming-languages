;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "2.+-", presented by the Esolang user "Iamcalledbob" in the
;; year 2018, and designed as a variation of Jonathan Todd Skinner's
;; language "Deadfish", albeit distinguished by the designation of its
;; command names, its character-based in lieu of numeric output, and the
;; potential of defining aliases for tokens, including instructions.
;; 
;; 
;; Architecture
;; ============
;; As an exact duplication of its cleronomy's property, 2.+- relies on a
;; single signed integer register, commonly referred to as the
;; "accumulator", for its data castaldy.
;; 
;; In its act of mimicry, the accumulator exhibits the same diorism as
;; incorporated into the provenance: While theoretically unbounded, thus
;; commorant in the gamut [-infinity, +infinity], the value is reset to
;; zero (0) in any of these two cases:
;; 
;;   (a) If the accumulator equals -1.
;;   (b) If the accumulator equals 256.
;; 
;; Any other value is retained without manipulations. Maugre this vouch,
;; the lack of a more potent deduction operation does not permit a
;; negative accumulator state to survive the normalization principle
;; explicated aboon --- as counterdistinguished from the square command
;; ("2" in this language, and "s" in Deadfish), by adminiculum of which
;; the positive axis can be perambulated infinitely.
;; 
;; 
;; Instructions
;; ============
;; The 2.+- language employs an instruction set prepotent to its
;; Deadfish inspiration, while yet admissive of the similarities'
;; recognition.
;; 
;; The relationship betwixt the two specimens may be subsumed into three
;; categories:
;; 
;;   (a) Paregal commands: Such are appropriated verbatim from Deadfish
;;       by 2.+- and comprehend
;;         + (increment accumulator)
;;         - (decrement accumulator)
;;   (b) Renamed commands: This ilk administers the exact same effect
;;       as some equivalent in Deadfish, albeit deploying a
;;       distinguished token for its purpose:
;;         2 (square accumulator; equivalent to Deadfish's "s")
;;   (c) New commands: Members of this tier introduce new capabilities
;;       into the language, their kind lacking in Deadfish:
;;         = (define alias for a token)
;; 
;; == OVERVIEW ==
;; The following apercu shall adhibit some cursory nortelry considering
;; the instruction set and its effects.
;; 
;; Please note that commands depending upon arguments are designed in a
;; particular fashion, with the variable parts underlined by asterisks.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Effect
;;   ------------+-----------------------------------------------------
;;   2           | Squares the accumulator.
;;               | This constitutes an original Deadfish operation,
;;               | however, differing in its designation from the
;;               | stock-father's "s".
;;   ..................................................................
;;   .           | Outputs the character whose code corresponds to the
;;               | accumulator value.
;;               | This constitutes a variant of the Deafish "o"
;;               | operation, however, printing characters instead of
;;               | numbers.
;;   ..................................................................
;;   +           | Increments the accumulator by one.
;;               | This constitutes an original Deadfish operation.
;;   ..................................................................
;;   -           | Decrements the accumulator by one.
;;               | This constitutes an original Deadfish operation.
;;   ..................................................................
;;   alias=token | Defines the {alias} as a synonym for the {token},
;;   ***** ***** | both expected to be exactly one character in length.
;;               | This constitutes a new operation not found in
;;               | Deadfish.
;;               | Please note that upon the {token}'s modification,
;;               | the {alias} is expected to assimilate its behavior.
;;   ------------------------------------------------------------------
;; 
;; == DEADFISH COMMANDS JUXTAPOSED TO 2.+- ==
;; The following table serves to juxtapose Deadfish's commands with that
;; of its derivation.
;; 
;;   ------------------------------------------------------------------
;;   Deadfish | 2.+- | Notes
;;   ---------+------+-------------------------------------------------
;;   +        | +    | 
;;   ..................................................................
;;   -        | -    | 
;;   ..................................................................
;;   s        | 2    | Only a nominal discrepancy differentiates the
;;            |      | two commands.
;;   ..................................................................
;;   o        | .    | Deadfish prints the accumulator verbatim as an
;;            |      | integer number, whereas 2.+- displays the
;;            |      | character responding to the accumulator value in
;;            |      | a perspective as a character code.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-24
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/2.%2B-"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' defines a hash table of zero or more entries,
   each key of which conforms to the KEY-TYPE, associated with a value
   of the VALUE-TYPE, both defaulting to the comprehensive ``T''."
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

(deftype alias-table ()
  "The ``alias-table'' type defines a mapping of command tokens to other
   such, thus defining aliases, this being represented by a hash table
   whose keys resolve to the alias characters, associated with the
   actual characters."
  '(hash-table-of character character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of alias table.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-default-alias-table ()
  "Creates and returns the alias table comprehending the default 2.+-
   command entries."
  (let ((aliases (make-hash-table :test #'eql)))
    (declare (type alias-table aliases))
    (setf (gethash #\2 aliases) #\2)
    (setf (gethash #\. aliases) #\.)
    (setf (gethash #\+ aliases) #\+)
    (setf (gethash #\- aliases) #\-)
    (setf (gethash #\= aliases) #\=)
    (the alias-table aliases)))

;;; -------------------------------------------------------

(defun alias-table-find-root (alias-table alias)
  "Retrieves and returns for the ALIAS in the ALIAS-TABLE the actually
   represented token, or ``NIL'' if no such exists."
  (declare (type alias-table alias-table))
  (declare (type character   alias))
  (let ((parent (gethash alias alias-table)))
    (declare (type (or null character) parent))
    (loop while parent do
      (if (char= parent alias)
        (loop-finish)
        (let ((grandparent (gethash parent alias-table)))
          (declare (type (or null character) grandparent))
          (cond
            ;; PARENT represents the last member of the alias path.
            ((null grandparent)
              (loop-finish))
            
            ;; Cycle in entry detected.
            ((char= parent grandparent)
              (loop-finish))
            
            ;; Cycle to the ALIAS detected.
            ((char= grandparent alias)
              (setf parent NIL)
              (loop-finish))
            
            ;; Follow the alias path in the TABLE.
            (T
              (setf parent grandparent))))))
    (the (or null character) parent)))

;;; -------------------------------------------------------

(defun alias-table-define (alias-table alias actual-token)
  "Registers in the alias ALIAS-TABLE a new entry which associates the
   ALIAS with the ACTUAL-TOKEN, and returns the modified ALIAS-TABLE."
  (declare (type alias-table alias-table))
  (declare (type character   alias))
  (declare (type character   actual-token))
  (setf (gethash alias alias-table) actual-token)
  (the alias-table alias-table))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-string-iterator ((string
                                 token-variable
                                 position-variable)
                                &body body)
  "Evaluates the STRING, defines a local binding of the designation
   TOKEN-VARIABLE to hold its current character, and another yclept
   POSITION-VARIABLE for the current location, evaluates the BODY forms,
   and returns the last processed form's results.
   ---
   As an accompaniment to the bindings, two local functions of fixed
   nomenclature will be defined in order to drive the iteration
   procedure and accommodate convenience utilies:
   
     ------------------------------------------------------------------
     Local function | Effect
     ---------------+--------------------------------------------------
     advance (n)    | Moves the position cursor n steps forward or
                    | back, updates the current character, and returns
                    | no value.
     ..................................................................
     peek (n)       | Returns the character n steps relating to the
                    | return position cursor, or ``NIL'' if outside of
                    | the STRING's boundaries, without modifying the
                    | iterator state.
     ------------------------------------------------------------------"
  (let ((evaluated-string (gensym)))
    (declare (type symbol evaluated-string))
    `(let ((,evaluated-string ,string))
       (declare (type string ,evaluated-string))
       
       (let ((,position-variable 0)
             (,token-variable
               (when (array-in-bounds-p ,evaluated-string 0)
                 (char ,evaluated-string 0))))
         (declare (type fixnum              ,position-variable))
         (declare (type (or null character) ,token-variable))
         (flet
             ((advance (&optional (offset 1))
               "Translates the position cursor by OFFSET steps, updates
                the iterator state, and returns no value."
               (declare (type fixnum offset))
               (setf ,token-variable
                 (when (array-in-bounds-p ,evaluated-string
                         (+ ,position-variable offset))
                   (char ,evaluated-string
                     (incf ,position-variable offset))))
               (values))
              
              (peek (offset)
               "Returns the character OFFSET steps from the position
                cursor, or ``NIL'' if the location would transgress the
                STRING's boundaries."
               (declare (type fixnum offset))
               (the (or null character)
                 (when (array-in-bounds-p ,evaluated-string
                         (+ ,position-variable offset))
                   (char ,evaluated-string
                     (+ ,position-variable offset))))))
           
           ,@body)))))

;;; -------------------------------------------------------

(defun interpret-2.+- (&optional (initial-input NIL))
  "Launches the 2.+- interpreter, optionally provided with the
   INITIAL-INPUT as the incipient commands, and returns no value."
  (declare (type (or null string) initial-input))
  
  (let ((alias-table (build-default-alias-table))
        (accumulator 0))
    (declare (type alias-table alias-table))
    (declare (type integer     accumulator))
    
    (loop do
      (format T "~&>> ")
      
      (let ((input (or initial-input
                       (read-line))))
        (declare (type string input))
        
        (setf initial-input NIL)
        (clear-input)
        
        (with-string-iterator (input token position)
          (labels
              ((get-command (alias)
                "Returns the command token associated with the ALIAS."
                (the (or null character)
                  (alias-table-find-root alias-table alias)))
               
               (define-alias (new-alias command-token)
                "Defines the NEW-ALIAS as a synonym of the
                 COMMAND-TOKEN and returns no value."
                (declare (type character new-alias))
                (declare (type character command-token))
                (alias-table-define alias-table new-alias command-token)
                (values))
               
               (assignment-follows-p ()
                "Checks whether the character immediately succeeding the
                 current POSITION represents an equals sign (\"=\"),
                 thus designating an alias command, returning on
                 confirmation a ``boolean'' value of ``T'', otherwise
                 ``NIL''."
                (the boolean
                  (and (peek 1)
                       (get-command (peek 1))
                       (char= (get-command (peek 1)) #\=))))
               
               (normalize-accumulator ()
                "Normalizes the ACCUMULATOR by resetting it to zero (0)
                 if its value equals -1 or 256 and returns no value."
                (when (or (= accumulator -1)
                          (= accumulator 256))
                  (setf accumulator 0))
                (values)))
            
            (loop while token do
              (normalize-accumulator)
              
              (cond
                ((and (assignment-follows-p)
                      (peek 2))
                  (let ((new-command token))
                    (declare (type character new-command))
                    ;; Skip current TOKEN and "=".
                    (advance 2)
                    (define-alias new-command token)
                    (advance)))
                
                ((and (assignment-follows-p)
                      (not (peek 2)))
                  (error "Expected to find a character to assign in ~
                          \"~c=\", but encountered end of file at the
                          position ~d."
                    token position))
                
                (T
                  (case (get-command token)
                    (#\2
                      (setf accumulator
                            (* accumulator accumulator)))
                    
                    (#\.
                      (write-char (code-char accumulator)))
                    
                    (#\+
                      (incf accumulator))
                    
                    (#\-
                      (decf accumulator))
                    
                    (otherwise
                      (terpri)))
                  
                  (advance)))))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;; Print the letter "B".
(interpret-2.+-
  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.")
|#

;;; -------------------------------------------------------

;; Print "Hello world".
(interpret-2.+-
  "s=2o=.i=+d=-iisiiiisiiiiiiiioiiiiiiiiiiiiiiiiiiiiiiiiiiiiioiiiiiiiooiiiodddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddodddddddddddddddddddddsddoddddddddoiiioddddddoddddddddo")
