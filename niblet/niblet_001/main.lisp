;; Date: 2021-12-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Niblet"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype nybble ()
  "The ``nybble'' type defines an unsigned byte composed of four
   adjacent bits."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   adjacent bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype put-state ()
  "The ``put-state'' type defines the possible interactions between
   a cell and the stack."
  '(member :put :merge))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each of which composed of a key that conforms to the
   KEY-TYPE and a value that conforms to the VALUE-TYPE, both defaulting
   to the comprehensive type ``T''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Cell
  "The ``Cell'' class describes a cell in the memory of a niblet
   program, defined by an octet-typed value and a state pertaining to
   its interaction with the stack."
  (value 0    :type octet)
  (state :put :type put-state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0 *) +NUMBER-OF-CELLS+))
(declaim (type (integer 0 *) +MAXIMUM-CELL-INDEX+))

;;; -------------------------------------------------------

(defparameter +NUMBER-OF-CELLS+ 30000
  "The maximum number of cells in the program memory.")

(defparameter +MAXIMUM-CELL-INDEX+ (1- +NUMBER-OF-CELLS+)
  "The index of the last cell in the program memory.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-niblet (code)
  "Interprets the niblet CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((source-pointer 0)
          (token          (char code 0))
          (cells          (make-hash-table :test #'eql))
          (cell-pointer   0)
          (stack          0))
      (declare (type fixnum                              source-pointer))
      (declare (type (or null character)                 token))
      (declare (type (hash-table-of (integer 0 *) octet) cells))
      (declare (type integer                             cell-pointer))
      (declare (type nybble                              stack))
      
      (labels
          ((advance ()
            "Moves the SOURCE-POINTER to the next token, if possible,
             updating the TOKEN on success, and returning no value in any
             case."
            (cond
              ((< source-pointer (1- (length code)))
                (incf source-pointer)
                (setf token (char code source-pointer)))
              (T
                (setf token NIL)))
            (values))
           
           (coalesce-back-sources ()
            "Starting at the current SOURCE-POINTER, consumes a series of
             zero or more adjacent 'back source' tokens (']'), relocating
             the SOURCE-POINTER to the first non-']' position, and
             returning the tally of consumed 'back source' tokens."
            (the (integer 0 *)
              (loop
                while (and token (char= token #\[))
                do    (advance)
                count 1)))
           
           
           (advance-cell-pointer ()
            "Moves the CELL-POINTER to the next cell if possible,
             signaling an error if the CELL-POINTER would infringe the end
             of the CELLS, returning no value otherwise."
            (if (< (1+ cell-pointer) +MAXIMUM-CELL-INDEX+)
              (incf cell-pointer)
              (error "The program has attempted to move the cell ~
                      pointer to the illegal position ~d, which ~
                      infringes the maximum of ~d."
                (1+ cell-pointer) +MAXIMUM-CELL-INDEX+)))
           
           
           (ensure-cell ()
            "Ensures that a cell at the current CELL-POINTER exists,
             creating one if missing, and returns the cell at the
             CELL-POINTER."
            (unless (gethash cell-pointer cells)
              (setf (gethash cell-pointer cells) (make-cell)))
            (the Cell (gethash cell-pointer cells)))
           
           (current-cell ()
            "Returns the cell at the current CELL-POINTER, creating one
             if none such exists yet."
            (the Cell (ensure-cell)))
           
           ((setf current-cell) (new-value)
            "Sets the value of the cell at the current CELL-POINTER to
             the new VALUE and returns no value."
            (declare (type octet new-value))
            (setf (cell-value (ensure-cell)) new-value)
            (values))
           
           
           (set-stack-to-not ()
            "Sets the value of the STACK to its own logical 'NOT' and
             returns no value."
            (loop
              for bit-index
                of-type (integer 0 *)
                from    0
                below   4
              do
                (if (logbitp bit-index stack)
                  (setf (ldb (byte 1 bit-index) stack) 0)
                  (setf (ldb (byte 1 bit-index) stack) 1)))
            (values)))
        
        (loop do
          (cond
            ((null token)
              (loop-finish))
            
            ((char= token #\<)
              (advance)
              (decf cell-pointer 2)
              (advance-cell-pointer))
            
            ((char= token #\[)
              (let ((return-position source-pointer)
                    (back-steps      (coalesce-back-sources)))
                (declare (type fixnum        return-position))
                (declare (type (integer 1 *) back-steps))
                (setf source-pointer (- return-position back-steps))
                (setf token (char code source-pointer)))
              (advance-cell-pointer))
            
            ((char= token #\$)
              (cond
                ;; currentCell = 0? => Skip next command.
                ((zerop (cell-value (current-cell)))
                  (advance)
                  (case token
                    ((NIL)
                      (loop-finish))
                    (#\[
                      (coalesce-back-sources))
                    (otherwise
                      (advance)))
                  (advance-cell-pointer))
                ;; currentCell != 0? => Advance as usual.
                (T
                  (advance)
                  (advance-cell-pointer))))
            
            ((char= token #\,)
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (current-cell) (char-code input)))
              (advance)
              (advance-cell-pointer))
            
            ((char= token #\.)
              (write-char (code-char (cell-value (current-cell))))
              (advance)
              (advance-cell-pointer))
            
            ((char= token #\v)
              (let ((current-cell (current-cell)))
                (declare (type Cell current-cell))
                (case (cell-state current-cell)
                  (:put
                    (setf (cell-value current-cell) (ash stack 4))
                    (set-stack-to-not)
                    (setf (cell-state current-cell) :merge))
                  
                  (:merge
                    (setf (cell-value current-cell)
                          (logior (cell-value current-cell) stack))
                    (set-stack-to-not)
                    (setf (cell-state current-cell) :put))
                  
                  (otherwise
                    (error "Invalid cell state: ~s."
                      (cell-state current-cell)))))
              (advance)
              (advance-cell-pointer))
            
            ((char= token #\+)
              (incf stack)
              (advance)
              (advance-cell-pointer))
            
            ((char= token #\-)
              (decf stack)
              (advance)
              (advance-cell-pointer))
            
            (T
              (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-niblet ",<.<[[[[")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-niblet
"
++++<<<<v<
---<<<v<.
-<v<
----<<<<v<.
----<<<<<v<
+++<<<v<.
+++<<<v<
+++<<<v<.
+++<<<v<
++++++<<<<<<v<.
++<<v<
-<v<.
-<v<
-------------<<<<<<<<<<<<<v<.
----------<<<<<<<<<<v<
---<<<v<.
--<<v<
++++++<<<<<<v<.
+++++++<<<<<<<v<
------<<<<<<v<.
-------<<<<<<<v<
+++<<<v<.
+++<<<v<
-----<<<<<v<.
---------<<<<<<<<<v<
------------<<<<<<<<<<<<v<.
")
