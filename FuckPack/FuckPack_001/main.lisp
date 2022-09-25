;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "FuckPack", invented by the Esolang user "A" in the year
;; 2018, and intended as an extension of Urban Mueller's original
;; "brainfuck" language, the augmentation of which enumerates such
;; warklumes as an absolute memory cell indexing, as well as arithmetic
;; and logical operations.
;; 
;; Instructions
;; ============
;; FuckPack's appropriation of its brainfuck cleronomy is concluded by a
;; set encompassing four additional implements, covering the aspects of
;; absolute memory cell indexing, arithmetics, and logical operations.
;; 
;; == OVERVIEW ==
;; The following table shall impart an apercu regarding the language's
;; tool set:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell pointer one position to the right.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   <       | Moves the cell pointer one position to the left.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   +       | Increments the current cell's value by one.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   -       | Decrements the current cell's value by one.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   .       | Outputs the character associated with current cell's
;;           | value as its ASCII code.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   ,       | Queries for a character input and stores its ASCII code
;;           | in the current cell.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), relocates the
;;           | instruction pointer (IP) forward to the position
;;           | immediately following the matching "]" instruction.
;;           | Otherwise, proceeds as usual.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0),
;;           | relocates the instruction pointer (IP) back to the
;;           | position immediately following the matching "["
;;           | instruction.
;;           | Otherwise, proceeds as follows.
;;           | Constitutes a verbatim appropriation from brainfuck.
;;   ..................................................................
;;   J       | Relocates the cell pointer to the cell whose index
;;           | equals the current cell's value.
;;           | Constitutes a newly introduced instruction.
;;   ..................................................................
;;   *       | Multiplies the current cell's value by the value of the
;;           | immediately succeeding neighbor and stores the product
;;           | back into the current cell.
;;           | The participating neighbor cell is not modified.
;;           | Constitutes a newly introduced instruction.
;;   ..................................................................
;;   }       | Performs a single left bit shift on the current cell's
;;           | value.
;;           | Constitutes a newly introduced instruction.
;;   ..................................................................
;;   X       | Performs a logical bitwise NOT operation on the current
;;           | cell's value, that is, inverts all of its bits.
;;           | Constitutes a newly introduced instruction.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-24
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/FuckPack"
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of jump sections in the
   form of bidirectional associations betwixt start and end positions in
   a piece of code, represented by a hash table of fixnum objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a representation of the program memory in
   the form of a hash table which associates with integer cell indices
   as keys the cell values of the same type."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Builds and returns a jump table for the CODE which associates with
   each forward jump position the matching back jump index and vice
   versa."
  (declare (type string code))
  
  (let ((jump-table             (make-hash-table :test #'eql))
        (forward-jump-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-positions))
    
    (loop
      for character of-type character across code
      and position  of-type fixnum    from   0
      do
        (case character
          (#\[
            (push position forward-jump-positions))
          (#\]
            (cond
              (forward-jump-positions
                (let ((start-position (pop forward-jump-positions)))
                  (declare (type fixnum start-position))
                  (setf (gethash start-position jump-table) position)
                  (setf (gethash position jump-table) start-position)))
              (T
                (error "Unterminated \"]\"."))))
          (otherwise
            NIL)))
    
    (when forward-jump-positions
      (error "Unmatched \"[\" instances at positions ~{~d~^, ~}."
        forward-jump-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun invert-bits (bits
                    &optional
                      (number-of-bits (max 8 (integer-length bits))))
  "Returns a new integer containing the inverted BITS, that is, the
   input after having been applied to a bitwise NOT operation, tallying
   the first NUMBER-OF-BITS, which defaults to highest position of a
   one-bit in the BITS, with the minimum being eight (8)."
  (declare (type integer       bits))
  (declare (type (integer 0 *) number-of-bits))
  (let ((inverted-bits 0))
    (declare (type integer inverted-bits))
    (dotimes (bit-position number-of-bits)
      (declare (type (integer 0 *) bit-position))
      (setf (ldb (byte 1 bit-position) inverted-bits)
            (if (logbitp bit-position bits)
              0
              1)))
    (the integer inverted-bits)))

;;; -------------------------------------------------------

(defun interpret-FuckPack (code)
  "Interprets the piece of FuckPack CODE and returns no value."
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
      
      (flet
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
            (declare (type integer new-position))
            (setf ip new-position)
            (setf token
              (when (array-in-bounds-p code ip)
                (char code ip)))
            (values))
           
           (current-cell ()
            "Returns the current cell's value."
            (the integer (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE into the current cell and returns no
             value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0) new-value)
            (values))
           
           (next-cell ()
            "Returns the next cell's value."
            (the integer (gethash (1+ pointer) memory 0))))
        
        (loop while token do
          (case token
            ;; End of CODE.
            ((NIL)
              (loop-finish))
            
            ;; Move the cell pointer right by one cell.
            (#\>
              (incf pointer))
            
            ;; Move the cell pointer left by one cell.
            (#\<
              (decf pointer))
            
            ;; Increment current cell value.
            (#\+
              (incf (current-cell)))
            
            ;; Decrement current cell value.
            (#\-
              (decf (current-cell)))
            
            ;; Output ASCII character corresponding to current cell
            ;; value.
            (#\.
              (format T "~c" (code-char (current-cell))))
            
            ;; Prompt for user input character and store its ASCII code
            ;; into the current cell.
            (#\,
              (format T "~&Please enter an ASCII character: ")
              (setf (current-cell) (char-code (read-char)))
              (clear-input))
            
            ;; If current cell contains zero: jump forward past
            ;; matching "]".
            (#\[
              (when (zerop (current-cell))
                (move-to (gethash ip jump-table))))
            
            ;; If current cell does not contain zero: jump back past
            ;; matching "[".
            (#\]
              (unless (zerop (current-cell))
                (move-to (gethash ip jump-table))))
            
            ;; Move cell pointer to index equal to current cell value.
            (#\J
              (setf pointer (current-cell)))
            
            ;; Set current cell value to itself multiplied by the next
            ;; cell value.
            (#\*
              (setf (current-cell)
                    (* (current-cell)
                       (next-cell))))
            
            ;; Shift current cell value bitwise left by one position.
            (#\}
              (setf (current-cell)
                    (ash (current-cell) 1)))
            
            ;; Apply logical NOT to current cell value, that is, invert
            ;; its bits.
            (#\X
              (setf (current-cell)
                    (invert-bits (current-cell))))
            
            ;; Any other character is commentary.
            (otherwise
              NIL))
          (advance)))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite cat program.
(interpret-FuckPack ",.[,.]")

;;; -------------------------------------------------------

;; One-time cat program which, operating on 256 cells with prepopulated
;; ASCII codes equivalent to the cell indices, jumps to the cell equal
;; to the user input in order to print the same, instead of printing
;; the current cell containing the input directly.
;; 
;; Concept:
;;   move to cell[255]
;;   set cell[255] to the value 255
;;   
;;   { Set each cell[i], with 0 <= i < 255, to cell[i] = i. }
;;   repeat 255 times
;;     move cell pointer left
;;     set the current cell to 1
;;     multiply the current cell by the next one
;;     decrement the current cell to obtain the value nextCell - 1
;;   end repeat
;;   
;;   let input         <- prompt character from user
;;   let characterCode <- ASCII code for character input
;;   jump to cell[characterCode]
;;   print cell[characterCode]
(interpret-FuckPack
  "
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*-
  
  ,J.
  ")

;;; -------------------------------------------------------

;; Initialize the first cells cell[i], with 0 <= i <= 255, with the
;; respective ASCII character codes:
;;   cell[i] <- i
;; and print them.
(interpret-FuckPack
  "
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*- <+*-
  <+*- <+*- <+*-
  
  .>[.>]
  ")

;;; -------------------------------------------------------

;; Produce the ASCII code 65, representing the letter "A", by
;; adminiculum of the increment and left bit shift operations, and print
;; it to the standard output.
(interpret-FuckPack "+}}}}}}+ .")

;;; -------------------------------------------------------

;; Produce the ASCII code 65, representing the letter "A", by first
;; generating its complement, 190, and then inverting its bits, ere its
;; value is printed.
(interpret-FuckPack "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ X .")
