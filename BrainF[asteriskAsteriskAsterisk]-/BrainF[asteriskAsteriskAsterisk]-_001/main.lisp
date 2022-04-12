;; Author: Kaveh Yousefi
;; Date:   2022-04-11
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Brainf***-"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Cell".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Cell
  (:constructor make-cell ()))
  "The ``Cell'' class models a memory cell capable of storing an
   element to indagate and modify, as well as being able to memorize the
   last positive value for the case of inverting from the zero to a
   non-zero state."
  (current-value       0 :type integer)
  (last-positive-value 1 :type integer))

;;; -------------------------------------------------------

(defun cell-value (cell)
  "Returns the CELL value."
  (declare (type Cell cell))
  (the integer (cell-current-value cell)))

;;; -------------------------------------------------------

(defun (setf cell-value) (new-value cell)
  "Sets the CELL value to the NEW-VALUE and returns the modified CELL.
   ---
   If the NEW-VALUE is positive, it will be persisted for switching
   betwixt its quantity and zero during inversion processes."
  (declare (type integer new-value))
  (declare (type Cell    cell))
  (when (plusp new-value)
    (setf (cell-last-positive-value cell) new-value))
  (setf (cell-current-value cell) new-value)
  (the Cell cell))

;;; -------------------------------------------------------

(defun cell-invert (cell)
  "Inverts the CELL value, setting it to zero if positive, and to the
   last positive value if zero, and returns the modified CELL."
  (declare (type Cell cell))
  (setf (cell-current-value cell)
    (if (zerop (cell-current-value cell))
      (cell-last-positive-value cell)
      0))
  (the Cell cell))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BrainF***- (code)
  "Interprets the piece of BrainF***- CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (memory    (make-hash-table :test #'eql))
          (pointer   0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type hash-table          memory))
      (declare (type integer             pointer))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (array-in-bounds-p code (1+ position))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION cursor to the previous character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (array-in-bounds-p code (1- position))
                (char code (decf position))))
            (values))
           
           (ensure-cell (index)
            "Ensures that a ``Cell'' exists in the MEMORY at the INDEX,
             on necessity creating and inserting a fresh instance
             thereof, and either returns the extant or the created
             cell."
            (declare (type integer index))
            (multiple-value-bind (cell contains-cell-p)
                (gethash index memory)
              (declare (type (or null Cell) cell))
              (declare (type T              contains-cell-p))
              (unless contains-cell-p
                (setf cell                   (make-cell))
                (setf (gethash index memory) cell))
              (the Cell cell)))
           
           (cell-at (index)
            "Returns the value of the cell at the INDEX, creating a
             zero-valued entity if absent."
            (declare (type integer index))
            (let ((cell (ensure-cell index)))
              (declare (type Cell cell))
              (the integer (cell-value cell))))
           
           ((setf cell-at) (new-value index)
            "Sets the value of the cell at the INDEX to the NEW-VALUE
             and returns no value."
            (declare (type integer new-value))
            (declare (type integer index))
            (let ((cell (ensure-cell index)))
              (declare (type Cell cell))
              (setf (cell-value cell) new-value))
            (values))
           
           (current-cell ()
            "Returns the value of the current cell."
            (the integer (cell-at pointer)))
           
           ((setf current-cell) (new-value)
            "Sets the value of the current cell to the NEW-VALUE and
             returns no value."
            (declare (type integer new-value))
            (setf (cell-at pointer) new-value)
            (values))
           
           (next-cell ()
            "Returns the value of the cell following the current cell."
            (the integer (cell-at (1+ pointer))))
           
           ((setf next-cell) (new-value)
            "Sets the value of the cell following the current cell to
             the NEW-VALUE and returns no value."
            (declare (type integer new-value))
            (setf (cell-at (1+ pointer)) new-value)
            (values))
           
           (invert-current-cell ()
            "Inverts the current cell and returns no value."
            (cell-invert (ensure-cell pointer))
            (values)))
        
        (loop do
          (case character
            ;; End of CODE.
            ((NIL)
              (loop-finish))
            
            ;; Move the memory pointer right.
            (#\>
              (incf pointer)
              (advance))
            
            ;; Move the memory pointer left.
            (#\<
              (decf pointer)
              (advance))
            
            ;; Increase the current cell.
            (#\+
              (incf (current-cell))
              (advance))
            
            ;; Decrease the current cell.
            (#\-
              (decf (current-cell))
              (advance))
            
            ;; Output the current cell.
            (#\.
              (write-char (code-char (current-cell)))
              (advance))
            
            ;; Prompt input and store in the current cell.
            (#\,
              (format T "~&Please enter a character: ")
              (let ((input (read-char)))
                (declare (type character input))
                (clear-input)
                (setf (current-cell) (char-code input)))
              (advance))
            
            ;; Jump past the matching "]" if the current cell is zero.
            (#\[
              (cond
                ((zerop (current-cell))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated '['."))
                      (#\[
                        (incf level)
                        (advance))
                      (#\]
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back to matching "]" if the current cell is nonzero.
            (#\]
              (cond
                ((not (zerop (current-cell)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unmatched ']'."))
                      (#\]
                        (incf level)
                        (recede))
                      (#\[
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            ;; Decrement the next cell by the current cell.
            (#\_
              (decf (next-cell)
                    (current-cell))
              (advance))
            
            ;; Invert the current cell.
            (#\)
              (invert-current-cell)
              (advance))
            
            ;; Skip any other content as comment.
            (otherwise
              (advance)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-BrainF***- "+[>,.<_]")

;;; -------------------------------------------------------

;; Alternative implementation of an infinitely repeating cat program.
(interpret-BrainF***- ",.>+[,.<_>]")

;;; -------------------------------------------------------

;; Prompt an input character and print the character immediately
;; preceding it in the alphabet.
(interpret-BrainF***- ">,<+_>.")

;;; -------------------------------------------------------

;; Random program: Queries an input character, checks if it equals "A",
;; on parity printing infinitely "A", otherwise looping infinitely
;; without printing anything.
;; 
;; Memory layout:
;;   memory[0] <- 0
;;   memory[1] <- "A"
;;   memory[2] <- input
;; 
;; Algorithm (enumerated in the order of the program code lines):
;;   (1) Set memory[0] = 0, memory[1] = 65 (= "A")
;;   (2) Move to memory[1]
;;   (3) Move to memory[2]; set memory[2] = input
;;   (4) Move to memory[1];
;;       set memory[2] = memory[2] - memory[1] = input - "A"
;;   (5) Move to memory[2]
;;   (6) If memory[2] != 0 (=> input != "A"); loop infinitely
;;   (7) If memory[2]  = 0 (=> input  = "A"), invert memory[2] to
;;         previous positive value (= 65 = "A");
;;       infinitely print memory[2].
;; 
(interpret-BrainF***- "+++++++++++++[>+++++<-]
                       >
                       >,
                       <_
                       >
                       []
                       .
                       )[.]")
