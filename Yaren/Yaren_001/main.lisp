;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Remarks
;; =======
;; 
;; == ENSURE A VALUE OF ZERO ==
;; When proceeding from left to right, the following code fragment
;; ensures that the current cell contains the value zero (0):
;;   [+-]
;; 
;; == ENSURE A VALUE OF ONE ==
;; When proceeding from left to right, the following code fragment
;; ensures that the current cell contains the value one (1):
;;   [+-]+-
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-03-28
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Yaren"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE."
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

(deftype octet ()
  "The ``octet'' type defines an eight-bit unsigned integer value."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a hash table
   mapping to each signed integer cell index a single bit as the
   respective cell value."
  '(hash-table-of integer bit))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized directions into
   which the program counter may move."
  '(member :left :right))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Yaren (code)
  "Interprets the piece of Yaren CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((ip-location    0)
          (ip-direction   :right)
          (character      (char code 0))
          (memory         (make-hash-table :test #'eql))
          (cell-pointer   0))
      (declare (type fixnum              ip-location))
      (declare (type direction           ip-direction))
      (declare (type (or null character) character))
      (declare (type memory              memory))
      (declare (type integer             cell-pointer))
      
      (labels
          ((advance ()
            "Moves the IP-LOCATION cursor one character into the
             IP-DIRECTION, updates the current CHARACTER, and returns no
             value."
            (case ip-direction
              (:right
                (setf character
                  (when (array-in-bounds-p code (1+ ip-location))
                    (char code (incf ip-location)))))
              (:left
                (setf character
                  (when (array-in-bounds-p code (1- ip-location))
                    (char code (decf ip-location)))))
              (otherwise
                (error "Invalid IP direction: ~s." ip-direction)))
            (values))
           
           (recede ()
            "Moves the IP-LOCATION cusor one character widdershins the
             IP-DIRECTION, updates the current CHARACTER, and returns no
             value."
            (case ip-direction
              (:right
                (setf character
                  (when (array-in-bounds-p code (1- ip-location))
                    (char code (decf ip-location)))))
              (:left
                (setf character
                  (when (array-in-bounds-p code (1+ ip-location))
                    (char code (incf ip-location)))))
              (otherwise
                (error "Invalid IP direction: ~s." ip-direction)))
            (values))
           
           (jump-to-terminator (starter terminator)
            "Starting at the current IP-LOCATION, searches in the
             IP-DIRECTION for the TERMINATOR character matching the
             STARTER with respect to the correct nesting, finally moving
             the IP-LOCATION cursor one character past the detected
             TERMINATOR, and returns no value."
            (declare (type character starter))
            (declare (type character terminator))
            (loop with level of-type integer = 0 do
              (cond
                ((null character)
                  (error "Unmatched starter '~a'." starter))
                ((char= character terminator)
                  (cond
                    ((zerop level)
                      (advance)
                      (loop-finish))
                    (T
                      (decf level)
                      (advance))))
                ((char= character starter)
                  (incf level)
                  (advance))
                (T
                  (advance))))
            (values))
           
           (jump-to-starter (starter terminator)
            "Starting at the current IP-LOCATION, searches in the
             IP-DIRECTION for the STARTER character matching the
             TERMINATOR with respect to the correct nesting, finally
             moving the IP-LOCATION cursor to the detected position, and
             returns no value.
             ---
             Please note that the IP-LOCATION is settled on the STARTER
             itself, not translated past it."
            (declare (type character starter))
            (declare (type character terminator))
            (loop with level of-type integer = 0 do
              (cond
                ((null character)
                  (error "Unmatched terminator '~a'." terminator))
                ((char= character starter)
                  (cond
                    ((zerop level)
                      (loop-finish))
                    (T
                      (decf level)
                      (recede))))
                ((char= character terminator)
                  (incf level)
                  (recede))
                (T
                  (recede))))
            (values))
           
           (current-cell ()
            "Returns the bit stored in the current cell."
            (the bit (gethash cell-pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Sets the bit stored in the current cell to the NEW-VALUE
             and returns no value."
            (declare (type bit new-value))
            (setf (gethash cell-pointer memory) new-value)
            (values))
           
           (cell-at (index)
            "Returns the bit stored in the cell at the INDEX."
            (declare (type integer index))
            (the bit (gethash index memory 0)))
           
           ((setf cell-at) (new-value index)
            "Sets the bit stored in the cell at the INDEX to the
             NEW-VALUE and returns no value."
            (declare (type integer index))
            (declare (type bit     new-value))
            (setf (gethash index memory 0) new-value)
            (values))
           
           (read-integer ()
            "Reads from the standard input a line and attempts to parse
             the same as an integer object, returning on success the
             parsed integer, otherwise ``NIL''."
            (the (or null integer)
              (handler-case
                (parse-integer (read-line))
                (error ()
                  NIL))))
           
           (prompt-for-byte ()
            "Prompts the user for a byte until the same is specified,
             finally returning the value as an ``octet''."
            (the octet
              (loop do (format T "~&Please input a byte: ")
                (let ((input (read-integer)))
                  (declare (type (or null integer) input))
                  (clear-input)
                  (when (and input (typep input 'octet))
                    (return input)))))))
        
        (loop do
          (case character
            ;; End of CODE.
            ((NIL)
              (loop-finish))
            
            ;; Toggle the current cell value, and increment the cell
            ;; pointer.
            (#\+
              (setf (current-cell)
                    (- 1 (current-cell)))
              (incf cell-pointer)
              (advance))
            
            ;; Decrement the cell pointer.
            (#\-
              (decf cell-pointer)
              (advance))
            
            ;; Jump to the matching "]" if the current cell is zero.
            (#\[
              (case ip-direction
                (:right
                  ;; If current cell is zero: jump past matching "]".
                  (cond
                    ((zerop (current-cell))
                      (advance)
                      (jump-to-terminator #\[ #\]))
                    (T
                      (advance))))
                ;; This is a terminating bracket?
                (:left
                  (cond
                    ((zerop (current-cell))
                      (advance))
                    (T
                      (recede)
                      (jump-to-starter #\] #\[))))
                (otherwise
                  (error "Invalid direction: ~s." ip-direction))))
            
            ;; Jump back to the matching "[" is necessary.
            (#\]
              (case ip-direction
                ;; This is a terminating bracket?
                (:right
                  (cond
                    ((zerop (current-cell))
                      (advance))
                    (T
                      (recede)
                      (jump-to-starter #\[ #\]))))
                (:left
                  ;; If current cell is zero: jump past matching "[".
                  (cond
                    ((zerop (current-cell))
                      (advance)
                      (jump-to-terminator #\] #\[))
                    (T
                      (advance))))
                (otherwise
                  (error "Invalid direction: ~s." ip-direction))))
            
            ;; Direct the program counter to the left.
            (#\<
              (setf ip-direction :left)
              (advance))
            
            ;; Direct the program counter to the right.
            (#\>
              (setf ip-direction :right)
              (advance))
            
            ;; Input a byte.
            (#\,
              (let ((input (prompt-for-byte)))
                (declare (type octet input))
                (clear-input)
                (dotimes (bit-index 8)
                  (declare (type octet bit-index))
                  (setf (cell-at (+ cell-pointer bit-index))
                        (ldb (byte 1 bit-index) input))))
              (advance))
            
            ;; Output a byte.
            (#\.
              (let ((byte 0))
                (declare (type octet byte))
                (dotimes (bit-index 8)
                  (declare (type octet bit-index))
                  (setf (ldb (byte 1 bit-index) byte)
                        (cell-at (+ cell-pointer bit-index))))
                (write byte))
              (advance))
            
            ;; Ignore unknown characters.
            (otherwise
              (advance)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Yaren ",.")

;;; -------------------------------------------------------

;; Cat program which repeats until the current cell contains zero.
(interpret-Yaren ",.[,.]")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which does not cease when the
;; current cell contains zero.
(interpret-Yaren ",.[+-]+-[,.[+-]+-]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Yaren ",[>.<].")
