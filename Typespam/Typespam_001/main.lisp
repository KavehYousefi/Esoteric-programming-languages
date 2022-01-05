;; Date: 2022-01-04
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Typespam"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for writing operations,
   including, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE, both defaulting to a
   comprehensive ``T''."
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

(deftype instance-type ()
  "The ``instance-type'' defines the possibly data types that an
   instance is permitted to assume.
   ---
   Apart from the canonical elements, a special sentinel ``:unknown''
   accommodates a designator for the undecided initial instance state."
  '(member :unknown :character :integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Cell
  "The ``Cell'' class represents the concept of an 'instance' in the
   Typespam programming language
   ---
   A resorting to this name is implemented in order to obviate
   potential conflicts with Common Lisp's built-in symbols, which with
   crebritude involve the term 'instance'."
  (type           :unknown :type instance-type)
  (value          0        :type integer)
  (is-initialized NIL      :type boolean))

;;; -------------------------------------------------------

(defun cell-display (cell &optional (destination T))
  "Prints the CELL's value to the DESTINATION, with defaults to ``T'',
   and returns the CELL."
  (declare (type Cell        cell))
  (declare (type destination destination))
  (case (cell-type cell)
    (:character
      (write-char (code-char (cell-value cell)) destination))
    (:integer
      (format destination "~d" (cell-value cell)))
    (otherwise
      (error "Cell cannot display as a type of ~s." (cell-type cell))))
  (the Cell cell))

;;; -------------------------------------------------------

(defun cell-is-nonzero (cell)
  "Checks whether the CELL containas a non-zero value, returning a
   ``boolean'' result of ``T'' upon confirmation, and ``NIL'' otherwise."
  (declare (type Cell cell))
  (the boolean (not (zerop (cell-value cell)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Typespam (code)
  "Interprets the piece of Typespam CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          
          (chain     (make-hash-table :test #'eql))
          (pointer   0)
          (markers   (make-hash-table :test #'eql)))
      (declare (type fixnum                           position))
      (declare (type (or null character)              character))
      (declare (type (hash-table-of integer Cell)     chain))
      (declare (type integer                          pointer))
      (declare (type (hash-table-of character fixnum) markers))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character in the CODE,
             updates the current CHARACTER, and returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (move-to (new-position)
            "Moves the POSITION cursor to the NEW-POSITION, updates the
             current CHARACTER, and returns no value."
            (declare (type fixnum new-position))
            (setf position  new-position)
            (setf character (char code position))
            (values))
           
           
           (current-cell ()
            "Returns the cell (instance) under the memory POINTER."
            (the (or null Cell) (gethash pointer chain)))
           
           ((setf current-cell) (new-cell)
            "Sets the cell under the memory POINTER to the NEW-CELL, and
             returns the NEW-CELL."
            (declare (type Cell new-cell))
            (setf (gethash pointer chain) new-cell)
            (the Cell (gethash pointer chain)))
           
           (check-if-initialized ()
            "Checks whether the current instance is selected, throwing an
             error if not, otherwise returning no value."
            (unless (gethash pointer chain)
              (error "The instance at the index ~d is not initialized."
                pointer))
            (values))
           
           
           (set-marker (marker-name marker-position)
            "Associates the MARKER-NAME with the MARKER-POSITION in the
             CODE, overwriting any extant entry with such a name, and
             returning no value."
            (declare (type character marker-name))
            (declare (type fixnum    marker-position))
            (setf (gethash marker-name markers) marker-position)
            (values))
           
           (get-marker-position (marker-name)
            "Returns the position in the CODE designating the start of the
             body of the marker with the MARKER-NAME.
             ---
             If no MARKER-NAME association exists yet, the respective
             declaration is searched starting from the beginning of the
             CODE, and, if found, an association is established ere its
             returning. If no marker with this identifier can be detected,
             an error is signaled."
            (declare (type character marker-name))
            (multiple-value-bind (marker-position contains-marker)
                (gethash marker-name markers)
              (declare (type (or null fixnum) marker-position))
              (declare (type T                contains-marker))
              (the fixnum
                (if contains-marker
                  marker-position
                  (let ((return-position position))
                    (declare (type fixnum return-position))
                    (move-to 0)
                    (loop while character do
                      (case character
                        (#\|
                          (advance)
                          (when (char= character marker-name)
                            (advance)
                            (set-marker marker-name position)
                            (return
                              (prog1 position
                                (setf position return-position)))))
                        (otherwise
                          (advance)))
                      finally
                        (error "No marker with the name ~s could be found."
                          marker-name))))))))
        
        (loop do
          (cond
            ((null character)
              (loop-finish))
            
            ;; Initialize instance at the pointer.
            ((char= character #\_)
              (setf (current-cell) (make-cell))
              (advance))
            
            ;; Move the memory pointer up the chain.
            ((char= character #\^)
              (decf pointer)
              (advance))
            
            ;; Move the memory pointer down the chain.
            ((char= character #\v)
              (incf pointer)
              (advance))
            
            ;; Set the current instance type to character.
            ((char= character #\c)
              (check-if-initialized)
              (setf (cell-type (current-cell)) :character)
              (advance))
            
            ;; Set the current instance type to integer.
            ((char= character #\i)
              (check-if-initialized)
              (setf (cell-type (current-cell)) :integer)
              (advance))
            
            ;; Set the current instance value to the following character.
            ((char= character #\{)
              (check-if-initialized)
              (advance)
              (setf (cell-value (current-cell))
                    (char-code character))
              (advance))
            
            ;; Set a marker using the next character as its name.
            ((char= character #\|)
              (advance)
              (let ((marker-name character))
                (declare (type character marker-name))
                (advance)
                (set-marker marker-name position)))
            
            ;; Jump to the marker identified by next character.
            ((char= character #\})
              (advance)
              (let ((marker-name character))
                (declare (type character marker-name))
                (declare (ignorable      marker-name))
                (cond
                  ((cell-is-nonzero (current-cell))
                    (move-to (get-marker-position marker-name)))
                  (T
                    (advance)))))
            
            ;; Display the current instance value.
            ((char= character #\-)
              (check-if-initialized)
              (cell-display (current-cell) T)
              (advance))
            
            ;; Retrieve user input and store it in the current instance.
            ((char= character #\#)
              (check-if-initialized)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (cell-value (current-cell))
                      (char-code input)))
              (advance))
            
            ;; Ignore any other character as comment.
            (T
              (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rectified version of the "Hello World" program.
(interpret-Typespam "_c {H-{e-{l--{o-{ -{W-{o-{r-{l-{d-")

;;; -------------------------------------------------------

;; Infinite cat program.
(interpret-Typespam "_ c |A #- }A")
