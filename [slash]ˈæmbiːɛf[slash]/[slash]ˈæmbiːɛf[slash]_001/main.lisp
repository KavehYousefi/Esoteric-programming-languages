;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "/ˈæmbiːɛf/", created by the Esolang user "ais523" in the
;; year 2012, and based upon Urban Mueller's "brainfuck", however,
;; substituting the deterministic increment/decrement and left/right
;; cell pointer translation operations by random executions, while
;; discarding the input and output facilities.
;; 
;; Instructions
;; ============
;; While inheriting the preponderance of brainfuck's instruction set,
;; /ˈæmbiːɛf/ applies two kinds of modifications unto the same: excising
;; the input and output facilities and coalescing the
;; increment/decrement operation into a single unit, in the same manner
;; as the cell pointer left/right translations are united. Merely the
;; jump instructions retain their verbatim interpretation.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Randomly either increments or decrements the current
;;           | cell by one.
;;           | Is paregal to the command "-".
;;           | Constitutes a modified variant of the brainfuck command.
;;   ..................................................................
;;   -       | Randomly either increments or decrements the current
;;           | cell by one.
;;           | Is paregal to the command "+".
;;           | Constitutes a modified variant of the brainfuck command.
;;   ..................................................................
;;   <       | Randomly either moves the cell pointer one step to the
;;           | left or to the right.
;;           | Is paregal to the command ">".
;;           | Constitutes a modified variant of the brainfuck command.
;;   ..................................................................
;;   >       | Randomly either moves the cell pointer one step to the
;;           | left or to the right.
;;           | Is paregal to the command "<".
;;           | Constitutes a modified variant of the brainfuck command.
;;   ..................................................................
;;   [       | If the current cell contains the value zero (0), moves
;;           | the instruction pointer forward to the position
;;           | immediately succeeding the matching "]".
;;           | Otherwise proceeds as usual.
;;           | Has been appropriated verbatim from brainfuck.
;;   ..................................................................
;;   ]       | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer back to the position
;;           | immediately succeeding the matching "[".
;;           | Otherwise proceeds as usual.
;;           | Has been appropriated verbatim from brainfuck.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-10-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki//ˈæmbiːɛf/"
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

(deftype jump-table ()
  "The ``jump-table'' type defines an association of forward jump
   positions to matching back jump locations, and vice versa, in a piece
   of /ˈæmbiːɛf/ code, modeled as a hash table, mapping fixnums to the
   same type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse mapping
   from integer cell indices to integer cell values, modeled as a hash
   table with key-value pairs of the respective type."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Calculates and returns for the piece of /ˈæmbiːɛf/ CODE a jump table,
   which associates with each jump forward position in the CODE the
   matching back jump location, and vice versa."
  (declare (type string code))
  
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    
    (loop
      for token    of-type character across code
      for position of-type fixnum    from   0
      do
        (case token
          (#\[
            (push position forward-jump-points))
          
          (#\]
            (if forward-jump-points
              (let ((forward-jump-position (pop forward-jump-points)))
                (declare (type fixnum forward-jump-position))
                (setf (gethash forward-jump-position jump-table)
                      position)
                (setf (gethash position jump-table)
                      forward-jump-position))
              (error "Unmatched \"]\" at position ~d." position)))
          
          (otherwise
            NIL)))
    
    (when forward-jump-points
      (error "Unmatched \"[\"s at positions ~{~d~^, ~}."
        forward-jump-points))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun print-memory (memory)
  "Prints the content of the MEMORY to the standard output and returns
   no value."
  (declare (type memory memory))
  (let ((sorted-indices
          (sort
            (loop
              for cell-index
                of-type integer
                being the hash-keys in memory
              collect cell-index)
            #'<)))
    (declare (type (list-of integer) sorted-indices))
    (loop for cell-index of-type integer in sorted-indices do
      (format T "~&memory[~d] = ~d" cell-index
        (gethash cell-index memory))))
  (values))

;;; -------------------------------------------------------

(defun interpret-/ˈæmbiːɛf/ (code)
  "Interprets the piece of /ˈæmbiːɛf/ CODE, prints at the end of the
   program the cell content to the standard output, and returns no
   value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((ip         0)
          (token      (char code 0))
          (jump-table (build-jump-table code))
          (memory     (make-hash-table :test #'eql))
          (pointer    0))
      (declare (type fixnum              ip))
      (declare (type (or null character) token))
      (declare (type jump-table          jump-table))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      
      ;; Populate the first cell.
      (setf (gethash 0 memory) 0)
      
      ;; Initialize the random number generator.
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
            "Relocates the instruction pointer IP to the NEW-POSITION in
             the CODE, updates the current TOKEN, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf token
              (when (array-in-bounds-p code ip)
                (char code ip)))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expected to reside at a forward or back jump TOKEN,
             relocates the instruction pointer IP to the matching
             opposite jump point, updates the current TOKEN, and returns
             no value."
            (move-to (gethash ip jump-table))
            (values))
           
           (current-cell ()
            "Returns the value stored in the cell under the POINTER."
            (the integer
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the cell under the POINTER and
             returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while token do
          (case token
            ((NIL)
              (loop-finish))
            
            (#\[
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            (#\]
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            ((#\+ #\-)
              (if (zerop (random 2))
                (incf (current-cell))
                (decf (current-cell))))
            
            ((#\< #\>)
              (if (zerop (random 2))
                (decf pointer)
                (incf pointer)))
            
            (otherwise
              NIL))
          
          (advance)))
      
      (print-memory memory)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first cell to a value of 0 with a probability of one (1).
(interpret-/ˈæmbiːɛf/ "[+]")

;;; -------------------------------------------------------

;; Set the first cell to a value of +1 or -1 with a probability of one
;; (1).
(interpret-/ˈæmbiːɛf/ "[+]+")
