;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ABG" and its extended kin "ABGAD", both designed by the
;; Esolang user "ChuckEsoteric08" in the year 2022, its foundment
;; ostending a semblance to Urban Mueller's brainfuck, while reiterating
;; their distinctive instruction names in the respective language's
;; designation, and thus incorporating cell movements and manipulations,
;; as well a goto facility.
;; 
;; 
;; Concept
;; =======
;; A rudimentary warklume of programming efforts, ABG and its extension
;; ABGAD demonstrate a forbisen of the multum in parvo principle, the
;; single character instructions attending to the manipulation of an
;; infinite cell-based memory, as well as the contingency for
;; iterations. The latter specimen of this family also incorporates
;; sufficient potence as to define input and output conduits.
;; 
;; Its operative elements' conspectus bewrays a derivation from
;; brainfuck, several actually distinct effects having been ligated into
;; adunations. The capability to recur to its inspiration constitutes
;; the endeavors applied to the correct choice of ABG/ABGAD commands in
;; succession.
;; 
;; 
;; Architecture
;; ============
;; A consectary of its brainfuck inspiration, ABG/ABGAD accommodates a
;; tape-like memory, infinite in its extent across both directions, with
;; integer scalars stored in each cell, the active member referenced by
;; a dedicated, movable pointer.
;; 
;; 
;; Data Types
;; ==========
;; ABG/ABGAD employs two distinct data types with tight interrelations:
;; signed integer values, a paravaunt object of the memory cells, and
;; characters, their inroads exhausted by input/output operations.
;; 
;; 
;; Instructions
;; ============
;; A tantamount of its bifurcation into a basic and an extended variant,
;; the ABG/ABGAD programming language partakes of two distinct operative
;; accoutrements, with the latter specimen being a strict superset of
;; the former.
;; 
;; == OVERVIEW ==
;; The four operations furnished by ABG shall be listed in summary
;; below:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   a       | Increments the current cell value by one and
;;           | subsequently moves the cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   b       | Decrements the current cell value by one.
;;   ..................................................................
;;   g       | Moves the cell pointer one step to the left.
;;           | If the current cell value equals zero, relocates the
;;           | instruction pointer to the position immediately
;;           | succeeding the matching "c" command. Otherwise, proceeds
;;           | as usual.
;;   ..................................................................
;;   c       | If the current cell value does not equal zero, relocates
;;           | the instruction pointer to the position immediately
;;           | succeeding the matching "g" command. Otherwise, proceeds
;;           | as usual.
;;   ------------------------------------------------------------------
;; 
;; The augmented instruction set of the ABGAD variant, valorized by
;; three new members' participation --- namely, "d", "#", and "e" --- as
;; opposed to its basic companion, shall be exposed in the following:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   a       | Increments the current cell value by one and
;;           | subsequently moves the cell pointer one step to the
;;           | right.
;;   ..................................................................
;;   b       | Decrements the current cell value by one.
;;   ..................................................................
;;   g       | Moves the cell pointer one step to the left.
;;           | If the current cell value equals zero, relocates the
;;           | instruction pointer to the position immediately
;;           | succeeding the matching "c" command. Otherwise, proceeds
;;           | as usual.
;;   ..................................................................
;;   c       | If the current cell value does not equal zero, relocates
;;           | the instruction pointer to the position immediately
;;           | succeeding the matching "g" command. Otherwise, proceeds
;;           | as usual.
;;   ..................................................................
;;   d       | Queries the user for an ASCII character and stores its
;;           | character code in the current cell.
;;   ..................................................................
;;   #       | Prints to the standard output the character associated
;;           | with the current cell value when construed as an ASCII
;;           | code.
;;   ..................................................................
;;   e       | Immediately halts the program.
;;   ------------------------------------------------------------------
;; 
;; == JUMPING VIA "g" and "c" ==
;; Conforming with a precise ilk of treatise, the following holds for a
;; jump start marker "g", in consequence, of course, of the precedent
;; sinistral cell pointer translation:
;; 
;;   (1) If the loop body does not comprehend any commands,
;;       unconditionally move past its end marker.
;;       In this case, the loop denegerates into a simple left motion
;;       of the cell pointer, deprived of any veridical iteration power.
;;   (2) If the loop body ensconces at least one command, and the
;;       current cell value equals zero, move past its end marker.
;;   (3) If the loop body ensconces at least one command, and the
;;       current cell value does not equal zero, move to the first
;;       body command.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-04
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/ABG"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (loop
              for    element of-type T in (the list object)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype command ()
  "The ``command'' type enumerates the recognized ABGAD commands types."
  '(member
    :increment
    :decrement
    :jump-forward
    :jump-back
    
    :input
    :output
    :halt
    
    :no-operation))

;;; -------------------------------------------------------

(deftype argument-entry ()
  "The ``argument-entry'' type defines an entry, or key-value pair,
   as deployed in an ``argument-list'', represented as a cons whose
   left compartment maintains the arugment name, while the dextral
   moeity refers to the argument value."
  '(cons keyword T))

;;; -------------------------------------------------------

(deftype argument-list ()
  "The ``argument-list'' type defines a mapping of argument names to
   their respective values, manifested as an association list (alist),
   whose cons conform to the ``argument-entry'' type, storing in the
   left part the name, while the right side maintains the value."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (loop
              for    element of-type T in (the list object)
              always (typep element 'argument-entry)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype ABG-program ()
  "The ``ABG-program'' type defines a vector of instructions."
  '(vector Instruction *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the ABG/ABGAD program memory as a
   theoretically infinite linear arrangement of cells, manifesting in a
   hash table of signed integer cell indices associated with signed
   integer cell values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type location)))
  "The ``Instruction'' class models an AGB or AGBAD instruction, storing
   with in conjunction with its command type the location of its
   beginning in the analyzed source code, as well as zero or more
   arguments."
  (type      (error "Missing instruction type.") :type command)
  (location  0                                   :type fixnum)
  (arguments NIL                                 :type argument-list))

;;; -------------------------------------------------------

(defun instruction-argument-entry (instruction argument-name)
  "Returns a reference to the cons associated in the INSTRUCTION's
   argument association list with the ARGUMENT-NAME, or ``NIL'' in the
   case of its absence."
  (declare (type Instruction instruction))
  (declare (type keyword     argument-name))
  (the (or null argument-entry)
    (assoc argument-name
      (instruction-arguments instruction)
      :test #'eq)))

;;; -------------------------------------------------------

(defun instruction-argument (instruction argument-name)
  "Returns the argument value associated with ARGUMENT-NAME in the
   INSTRUCTION, or signals an error upon the identifier's absence."
  (declare (type Instruction instruction))
  (declare (type keyword     argument-name))
  (let ((argument-entry
          (instruction-argument-entry instruction argument-name)))
    (declare (type (or null argument-entry) argument-entry))
    (the T
      (if argument-entry
        (cdr argument-entry)
        (error "There exists no argument with the name ~s for the ~
                instruction ~s."
          argument-name instruction)))))

;;; -------------------------------------------------------

(defun (setf instruction-argument) (new-value instruction argument-name)
  "Adds or updates the INSTRUCTION argument identified by the
   ARGUMENT-NAME to affiliate with the NEW-VALUE and returns the
   modified INSTRUCTION."
  (declare (type T           new-value))
  (declare (type Instruction instruction))
  (declare (type keyword     argument-name))
  (let ((argument-entry
          (instruction-argument-entry instruction argument-name)))
    (declare (type (or null argument-entry) argument-entry))
    (if argument-entry
      (setf (cdr argument-entry) new-value)
      (push (cons argument-name new-value)
            (instruction-arguments instruction))))
  (the Instruction instruction))

;;; -------------------------------------------------------

(defmethod print-object ((instruction Instruction) stream)
  (declare (type Instruction instruction))
  (declare (type destination stream))
  (format stream "(Instruction :type ~s)"
    (instruction-type instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extract and returns from the piece of ABG or ABGAD source CODE a
   one-dimensional simple array of instructions."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of Instruction) instructions))
    (flet
        ((collect-instruction (type location)
          "Creates a new instruction of the TYPE, memorizing its
           LOCATION in the CODE, prepends the fresh object to the
           INSTRUCTIONS list, and returns no value."
          (declare (type command type))
          (declare (type fixnum  location))
          (push (make-instruction type location) instructions)
          (values)))
      (loop
        for token    of-type character across code
        and location of-type fixnum    from   0
        do
          (case token
            (#\a       (collect-instruction :increment    location))
            (#\b       (collect-instruction :decrement    location))
            (#\g       (collect-instruction :jump-forward location))
            (#\c       (collect-instruction :jump-back    location))
            
            (#\d       (collect-instruction :input        location))
            (#\#       (collect-instruction :output       location))
            (#\e       (collect-instruction :halt         location))
            
            (#\Space   (collect-instruction :no-operation location))
            (#\Tab     (collect-instruction :no-operation location))
            (#\Newline (collect-instruction :no-operation location))
            
            (otherwise (error "Invalid token at position ~d: ~s."
                         location token)))))
    (the (simple-array Instruction (*))
      (coerce (nreverse instructions)
        '(simple-array Instruction (*))))))

;;; -------------------------------------------------------

(defun count-instructions-between (first-instruction second-instruction)
  "Returns the number of instructions in the intermede betwixt the
   FIRST-INSTRUCTION and the SECOND-INSTRUCTION."
  (declare (type Instruction first-instruction))
  (declare (type Instruction second-instruction))
  (let ((first-location  (instruction-location first-instruction))
        (second-location (instruction-location second-instruction)))
    (declare (type fixnum first-location))
    (declare (type fixnum second-location))
    (the (integer 0 *)
      (cond
        ((= first-location second-location)
          0)
        ((< first-location second-location)
          (- second-location first-location 1))
        (T
          (- first-location second-location 1))))))

;;; -------------------------------------------------------

(defun connect-loop-endpoints (loop-start loop-end)
  "Stores in the LOOP-START instruction a reference to the LOOP-END and
   vice versa, returning no value."
  (declare (type Instruction loop-start))
  (declare (type Instruction loop-end))
  ;; Connect the forward jump to its end post.
  (setf (instruction-argument loop-start  :end) loop-end)
  ;; Connect the back jump to its start post.
  (setf (instruction-argument loop-end :start) loop-start)
  ;; Check whether the loop body is empty or not.
  (setf (instruction-argument loop-start :empty-p)
        (not (null
          (<= (count-instructions-between loop-start loop-end)
              0))))
  ;; Copy the LOOP-START emptiness check result to the LOOP-END.
  (setf (instruction-argument loop-end   :empty-p)
        (instruction-argument loop-start :empty-p))
  (values))

;;; -------------------------------------------------------

(defun build-loop-connections (instructions)
  "Connects all jump start and end points in the INSTRUCTIONS and
   returns the modified INSTRUCTIONS."
  (declare (type ABG-program instructions))
  (let ((jump-stack NIL))
    (declare (type (list-of Instruction) jump-stack))
    (loop for instruction of-type Instruction across instructions do
      (case (instruction-type instruction)
        (:jump-forward
          (push instruction jump-stack))
        (:jump-back
          (if jump-stack
            (connect-loop-endpoints (pop jump-stack) instruction)
            (error "Unmatched loop end instruction: ~s."
              instruction)))
        (otherwise
          NIL)))
    
    (when jump-stack
      (error "Unmatched loop start instructions: ~{~a~^, ~}."
        jump-stack)))
  
  (the ABG-program instructions))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loop-instruction-p (instruction)
  "Checks whether the INSTRUCTION represents a forward or back jump,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (or (eq (instruction-type instruction) :jump-forward)
          (eq (instruction-type instruction) :jump-back))))))

;;; -------------------------------------------------------

(defun empty-loop-p (instruction)
  "Checks whether the INSTRUCTION represents a loop whose forward and
   back jump instructions ensconce no tokens at all, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (the boolean
    (not (null
      (and
        (loop-instruction-p   instruction)
        (instruction-argument instruction :empty-p))))))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Evaluates the ABG/ABGAD INSTRUCTIONS and returns no value."
  (declare (type ABG-program instructions))
  
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (memory              (make-hash-table :test #'eql))
          (pointer             0))
      (declare (type fixnum                ip))
      (declare (type (or null Instruction) current-instruction))
      (declare (type memory                memory))
      (declare (type integer               pointer))
      
      (labels
          ((advance (&optional (number-of-steps 1))
            "Moves the instruction pointer IP a NUMBER-OF-STEPS forward,
             updates the CURRENT-INSTRUCTION, and returns no value."
            (incf ip number-of-steps)
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (move-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION,
             updates the CURRENT-INSTRUCTION, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (move-to-instruction (instruction)
            "Moves the instruction pointer IP to the position of the
             INSTRUCTION in the INSTRUCTIONS vector, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (declare (type Instruction instruction))
            (move-to (instruction-location instruction))
            (values))
           
           (current-cell ()
            "Returns the current cell's value."
            (the integer
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell and returns no
             value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while current-instruction do
          (case (instruction-type current-instruction)
            (:increment
              (incf (current-cell))
              (incf pointer)
              (advance))
            
            (:decrement
              (decf (current-cell))
              (advance))
            
            (:jump-forward
              (decf pointer)
              (cond
                ;; Start of empty loop?
                ;; => Move pointer and skip both start and end.
                ((empty-loop-p current-instruction)
                  (advance 2))
                ;; Non-empty loop and skipping required?
                ;; => Move to loop end.
                ((zerop (current-cell))
                  (move-to-instruction
                    (instruction-argument current-instruction :end)))
                ;; Non-empty loop which shall not be skipped?
                ;; => Enter body.
                (T
                  (advance))))
            
            (:jump-back
              (cond
                ;; End of empty loop?
                ;; => Advance.
                ((empty-loop-p current-instruction)
                  (advance))
                ;; Non-empty loop and repetition required?
                ;; => Jump to loop start.
                ((not (zerop (current-cell)))
                  (move-to-instruction
                    (instruction-argument current-instruction :start)))
                ;; Non-empty loop which shall be skipped?
                ;; => Advance.
                (T
                  (advance))))
            
            (:input
              (format T "~&Please input an ASCII character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input)
              (advance))
            
            (:output
              (write-char (code-char (current-cell)))
              (advance))
            
            (:halt
              (setf ip                  (length instructions))
              (setf current-instruction NIL)
              (loop-finish))
            
            (:no-operation
              (advance))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                current-instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-ABGAD (code)
  "Interprets the piece of ABG or ABGAD CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (build-loop-connections
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An infinitely repeating cat program.
(interpret-ABGAD "d#agd#aagcc")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-ABGAD
  "d
   #
   bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
   bag
     agcagcagcagcagcagcagcagcagcagc
     agcagcagcagcagcagcagcagcagcagc
     agcagcagcagcagcagcagcagcagcagc
     agcagcagcagcagcagcagcagcagcagc
     agcagcagcagcagcagcagcagc
     #
     bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
     aagc
   c")
