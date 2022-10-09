;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "4DChess", designed by the Esolang user "Zemeckis" on 18th
;; November, 2019, and intended as a derivative of Urban Mueller's
;; "brainfuck", the linear memory of which is augmented into four
;; dimensions, with eight cells, each of a single unsigned byte
;; capacity, available across any dimension.
;; 
;; Instructions
;; ============
;; 4DChess does invest any further extensions, beside the additional
;; operations for navigating across four memory spaces in lieu of a
;; single one, when juxtaposed with the original brainfuck.
;; 
;; == OVERVIEW ==
;; The following apercu shall assign a cursory nortelry concerning the
;; language's functional aspects:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Increments the cell pointer's X-axis by one.
;;           | This constitutes an equivalent of the original brainfuck
;;           | instruction.
;;   ..................................................................
;;   <       | Decrements the cell pointer's X-axis by one.
;;           | This constitutes an equivalent of the original brainfuck
;;           | instruction.
;;   ..................................................................
;;   ^       | Increments the cell pointer's Y-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   v       | Decrements the cell pointer's Y-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   *       | Increments the cell pointer's Z-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   o       | Decrements the cell pointer's Z-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   @       | Increments the cell pointer's W-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   ?       | Decrements the cell pointer's W-axis by one.
;;           | This constitutes a new operation not found in brainfuck.
;;   ..................................................................
;;   +       | Increments the memory cell under the pointer by one.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   -       | Decrements the memory cell under the pointer by one.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   .       | Writes to the standard output the ASCII character
;;           | associated with the value of the memory cell under the
;;           | pointer.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   ,       | Queries the user for an ASCII character and stores its
;;           | character code into the memory cell under the pointer.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   [       | If the memory cell under the pointer contains zero,
;;           | moves the instruction pointer forward to the position
;;           | immediately following the matching "]".
;;           | Otherwise proceeds as usual.
;;           | This constitutes an original brainfuck operation.
;;   ..................................................................
;;   ]       | If the memory cell under the pointer does not contain
;;           | zero, moves the instruction pointer back to the position
;;           | immediately following the matching "[".
;;           | Otherwise proceeds as usual.
;;           | This constitutes an original brainfuck operation.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-10-08
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/4DChess"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T) (size NIL))
  "The ``list-of'' type defines a list whose elements all conform to the
   ELEMENT-TYPE, optionally restrained to the specified SIZE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (or (null size)
                (= size (length (the list object))))
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
   with a value of the VALUE-TYPE, both defaulting to ``T''."
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
  "The ``jump-table'' represents an association of forward jump
   positions to their matching back jump points, and vice versa,
   construed as indices into the respective piece of 4DChess code
   string."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   covering the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a four-dimensional
   simple array of octet, thus producing four axes, with each such
   capable of storing exactly eight cells."
  '(simple-array octet (8 8 8 8)))

;;; -------------------------------------------------------

(deftype pointer ()
  "The ``pointer'' type defines the memory cell pointer as a list of
   four 8-bit bytes, with the elements representing in this order the
   X-, Y-, Z-, and W-axis, each one of these amenable to one of the
   eight cells along the specified dimension."
  '(list-of octet 4))

;;; -------------------------------------------------------

(deftype axis ()
  "The ``axis'' type enumerates the recognized dimension identifiers."
  '(member :x :y :z :w))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (code)
  "Generates and returns the jump table for the piece of 4DChess CODE.
   ---
   The jump table associates with each forward jump command \"[\" the
   matching back jump command \"]\", and vice versa, in the form of
   their positions in the CODE.
   ---
   An error of an unspecified type is signaled if a jump command cannot
   be matched with a peer."
  (declare (type string code))
  
  (let ((jump-table             (make-hash-table :test #'eql))
        (forward-jump-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-positions))
    
    (loop
      for token    of-type character across code
      for position of-type fixnum    from   0
      do
        (case token
          (#\[
            (push position forward-jump-positions))
          
          (#\]
            (if forward-jump-positions
              (let ((start-position (pop forward-jump-positions)))
                (declare (type fixnum start-position))
                (setf
                  (gethash start-position jump-table) position
                  (gethash position       jump-table) start-position))
              (error "Unmatched \"]\" at position ~d." position)))
          
          (otherwise
            NIL)))
    
    (when forward-jump-positions
      (error "Unmatched \"[\"s at positions ~{~a~^, ~}."
        forward-jump-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun interpret-4DChess (code)
  "Interprets the piece of 4DChess CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position   0)
          (token      (char code 0))
          (jump-table (build-jump-table code))
          (memory     (make-array '(8 8 8 8)
                        :element-type    'octet
                        :initial-element 0
                        :adjustable      NIL
                        :fill-pointer    NIL))
          (pointer    (list 0 0 0 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) token))
      (declare (type jump-table          jump-table))
      (declare (type memory              memory))
      (declare (type pointer             pointer))
      
      (flet
          ((advance ()
            "Moves the POSITION cursor to the next location in the CODE,
             if possible, updates the current TOKEN, and returns no
             value."
            (setf token
              (when (array-in-bounds-p code (1+ position))
                (char code (incf position))))
            (values))
           
           (jump-to-opposite-bound ()
            "Expecting to reside at a forward or back jump token,
             relocates the POSITION cursor to the matching opposite
             march and returns no value.
             ---
             An error of an unspecified type is signaled if no jump
             target can be found for the current POSITION."
            (setf position
              (or (gethash position jump-table)
                  (error "No matching jump target for position ~d."
                    position)))
            (setf token
              (when (array-in-bounds-p code position)
                (char code position)))
            (values))
           
           (increment-axis (axis)
            "Increments the AXIS of the cell POINTER by one and returns
             no value."
            (declare (type axis axis))
            (case axis
              (:x (incf (first  pointer)))
              (:y (incf (second pointer)))
              (:z (incf (third  pointer)))
              (:w (incf (fourth pointer)))
              (otherwise (error "Invalid axis: ~s." axis)))
            (values))
           
           (decrement-axis (axis)
            "Decrements the AXIS of the cell POINTER by one and returns
             no value."
            (declare (type axis axis))
            (case axis
              (:x (decf (first  pointer)))
              (:y (decf (second pointer)))
              (:z (decf (third  pointer)))
              (:w (decf (fourth pointer)))
              (otherwise (error "Invalid axis: ~s." axis)))
            (values))
            
            (current-cell ()
              "Returns the value of the MEMORY cell under the POINTER."
              (the octet
                (apply #'aref memory pointer)))
            
            ((setf current-cell) (new-value)
              "Sets the value of the MEMORY cell under the POINTER to
               the NEW-VALUE and returns no value."
              (declare (type integer new-value))
              (if (<= 0 new-value 255)
                (setf (apply #'aref memory pointer) new-value)
                (error "The intended cell value ~d violates the valid ~
                        byte range [0, 255]."
                  new-value))
              (values)))
        
        (loop while token do
          (case token
            ((NIL)
              (loop-finish))
            
            (#\>
              (increment-axis :x))
            
            (#\<
              (decrement-axis :x))
            
            (#\^
              (increment-axis :y))
            
            (#\v
              (decrement-axis :y))
            
            (#\*
              (increment-axis :z))
            
            (#\o
              (decrement-axis :z))
            
            (#\@
              (increment-axis :w))
            
            (#\?
              (decrement-axis :w))
            
            (#\+
              (incf (current-cell)))
            
            (#\-
              (decf (current-cell)))
            
            (#\.
              (write-char (code-char (current-cell))))
            
            (#\,
              (format T "~&Please input an ASCII character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            (#\[
              (when (zerop (current-cell))
                (jump-to-opposite-bound)))
            
            (#\]
              (unless (zerop (current-cell))
                (jump-to-opposite-bound)))
            
            (otherwise
              NIL))
          
          (advance)))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "Hello World!" using the fourth dimension (W-axis):
(interpret-4DChess "++++++++[@++++[@++@+++@+++@+????-]@+@+@-@@+[?]?-]@@.@---.+++++++..+++.@@.?-.?.+++.------.--------.@@+.@++.")
