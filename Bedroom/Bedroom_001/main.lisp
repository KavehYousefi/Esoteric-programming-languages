;; Author: Kaveh Yousefi
;; Date:   2022-03-02
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Bedroom"
;;   -> "https://stackoverflow.com/questions/38100329/what-does-u-ufe0f-in-an-emoji-mean-is-it-the-same-if-i-delete-it"
;;       o Describes Unicode variation selectors.



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

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized Bedroom commands."
  '(member
    :move-ip-up
    :move-ip-down
    :move-ip-left
    :move-ip-right
    :halt
    :increment-memory
    :decrement-memory
    :move-dp-left
    :move-dp-right
    :input
    :output
    :turn-90
    :nop))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized directions in which
   the program flow may proceed."
  '(member :down :left :right :up))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ancillary functions.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-instruction-for (character)
  "Returns the instruction associated with the CHARACTER, signaling an
   error if no correspondence can be established."
  (declare (type character character))
  (the instruction
    (case character
      (#\UPWARDS_BLACK_ARROW    :move-ip-up)
      (#\DOWNWARDS_BLACK_ARROW  :move-ip-down)
      (#\LEFTWARDS_BLACK_ARROW  :move-ip-left)
      (#\BLACK_RIGHTWARDS_ARROW :move-ip-right)
      (#\U0001F6CF              :halt)
      (#\📗                      :increment-memory)
      (#\🧸                     :decrement-memory)
      (#\🪄                      :move-dp-left)
      (#\🪆                      :move-dp-right)
      (#\🗄                      :input)
      (#\🗒                      :output)
      (#\📐                      :turn-90)
      (#\Space                  :nop)
      (otherwise
        (error "No instruction associated with the character ~s."
          character)))))

;;; -------------------------------------------------------

(defun turn-direction-by-90-degrees-ccw (direction)
  "Returns the ``direction'' obtained by turning from the specified
   DIRECTION by 90 degrees in a widdershins airt."
  (declare (type direction direction))
  (the direction
    (case direction
      (:down     :right)
      (:left     :down)
      (:right    :up)
      (:up       :left)
      (otherwise (error "Invalid direction: ~s." direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Location".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Location
  (:constructor make-location (x y)))
  "The ``Location'' class describes a Cartesian position in the integer
   space, compatible with a grid layout."
  (x 0 :type (integer 0 *))
  (y 0 :type (integer 0 *)))

;;; -------------------------------------------------------

(defun location-move (location direction)
  "Moves the LOCATION one step into the DIRECTION and returns the
   modified LOCATION."
  (declare (type Location  location))
  (declare (type direction direction))
  (case direction
    (:left     (decf (location-x location)))
    (:up       (decf (location-y location)))
    (:right    (incf (location-x location)))
    (:down     (incf (location-y location)))
    (otherwise (error "Invalid direction for location: ~s." direction)))
  (the Location location))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((width
    :initarg       :width
    :initform      0
    :type          (integer 0 *)
    :documentation "The number of columns.")
   (height
    :initarg       :height
    :initform      0
    :type          (integer 0 *)
    :documentation "The number of rows.")
   (cells
    :initarg       :cells
    :initform      (make-hash-table :test #'equalp)
    :type          (hash-table-of Location instruction)
    :documentation "A sparse "))
  (:documentation
    "The ``Grid'' class represents a two-dimensional Cartesian layout
     of Bedroom instructions, suitable to contain a program in the same
     language."))

;;; -------------------------------------------------------

(defun grid-at (grid location)
  "Returns the instruction at the GRID residing at the LOCATION."
  (declare (type Grid     grid))
  (declare (type Location location))
  (the instruction
    (gethash location (slot-value grid 'cells) :nop)))

;;; -------------------------------------------------------

(defun (setf grid-at) (new-instruction grid location)
  "Sets the GRID cell at the LOCATION to the NEW-INSTRUCTION and returns
   the modified GRID."
  (declare (type instruction new-instruction))
  (declare (type Grid        grid))
  (declare (type Location    location))
  (with-slots (cells) grid
    (declare (type (hash-table-of Location instruction) cells))
    (setf (gethash (copy-location location) cells) new-instruction))
  (the Grid grid))

;;; -------------------------------------------------------

(defun grid-empty-p (grid)
  "Checks whether the GRID is empty, that is, exhibits a width or height
   of zero, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Grid grid))
  (the boolean
    (not (null
      (or
        (zerop (slot-value grid 'width))
        (zerop (slot-value grid 'height)))))))

;;; -------------------------------------------------------

(defun grid-contains-point-p (grid point)
  "Checks whether the GRID contains the POINT, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type Location point))
  (with-slots (width height) grid
    (declare (type (integer 0 *) width))
    (declare (type (integer 0 *) height))
    (the boolean
      (not (null
        (and
          (<= 0 (location-x point) (1- width))
          (<= 0 (location-y point) (1- height))))))))

;;; -------------------------------------------------------

(defun make-grid-from-code (code)
  "Creates and returns a new ``Grid'' created from the CODE."
  (declare (type string code))
  (let ((grid     (make-instance 'Grid))
        (location (make-location 0 0)))
    (declare (type Grid      grid))
    (declare (type Location location))
    (with-slots (width height) grid
      (declare (type (integer 0 *) width))
      (declare (type (integer 0 *) height))
      (loop for character of-type character across code do
        (when (zerop height)
          (setf height 1))
        (case character
          (#\Newline
            (setf (location-x location) 0)
            (incf (location-y location) 1)
            (incf height                1))
          (#\VARIATION_SELECTOR-16
            NIL)
          (otherwise
            (setf (grid-at grid location)
                  (get-instruction-for character))
            (incf (location-x location) 1)
            (setf width (max width (location-x location)))))))
    (the Grid grid)))

;;; -------------------------------------------------------

(defun validate-grid (grid)
  "Checks whether the GRID satisfies the prerequisite of containing at
   least one halt instruction, on confirmation returning the GRID
   itself, otherwise signaling an error."
  (declare (type Grid grid))
  (loop
    for instruction
      of-type instruction
      being   the hash-values in (slot-value grid 'cells)
    thereis
      (eq instruction :halt)
    finally
      (error "No ``:halt'' instruction found in the grid."))
  (the Grid grid))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) stream)
  (declare (type Grid                            grid))
  (declare (type (or null (eql T) stream string) stream))
  (with-slots (width height cells) grid
    (declare (type (integer 0 *)                        width))
    (declare (type (integer 0 *)                        height))
    (declare (type (hash-table-of Location instruction) cells))
    (format stream "~&~d x ~d grid:" width height)
    (let ((location (make-location 0 0)))
      (declare (type Location location))
      (dotimes (y height)
        (declare (type (integer 0 *) y))
        (terpri stream)
        (setf (location-y location) y)
        (dotimes (x width)
          (declare (type (integer 0 *) x))
          (setf (location-x location) x)
          (format stream "~18a" (grid-at grid location)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Bedroom (code)
  "Interprets the piece of Bedroom CODE and returns no value.
   ---
   The processing of a Bedroom program relies on six constituents,
   partially involved in interrelations:
     grid         --- The Bedroom program converted from a CODE string
                      into a ``Grid'' for facilitated analyzation.
     ip           --- The instruction pointer, representing a cursor
                      into the current CODE grid cell.
     ip-direction --- The direction of the instruction pointer.
     instruction  --- The currently processed instruction.
     memory       --- The program memory.
     dp           --- The data pointer, a cursor into the current memory
                      cell."
  (declare (type string code))
  (let ((grid (make-grid-from-code code)))
    (declare (type Grid grid))
    
    ;; Check whether the GRID contains at least one "halt" instruction.
    (validate-grid grid)
    
    (unless (grid-empty-p grid)
      (let ((ip           (make-location 0 0)) ;; Instruction pointer.
            (ip-direction :right)
            (instruction  NIL)                 ;; Current instruction.
            (memory       (make-hash-table :test #'eql))
            (dp           0))                  ;; Data (memory) pointer.
        (declare (type (hash-table-of integer integer) memory))
        (declare (type Location                        ip))
        (declare (type direction                       ip-direction))
        (declare (type (or null instruction)           instruction))
        (declare (type integer                         dp))
        
        (setf instruction (grid-at grid ip))
        
        (flet
            ((advance ()
              "Moves the instruction pointer IP one step in the current
               IP-DIRECTION, if possible, updates the current
               INSTRUCTION, and returns no value."
              (location-move ip ip-direction)
              (setf instruction
                (when (grid-contains-point-p grid ip)
                  (grid-at grid ip)))
              (values)))
          
          (loop do
            (case instruction
              ((NIL)
                (error "Unexpected termination of program."))
              
              (:move-ip-up
                (setf ip-direction :up)
                (advance))
              
              (:move-ip-down
                (setf ip-direction :down)
                (advance))
              
              (:move-ip-left
                (setf ip-direction :left)
                (advance))
              
              (:move-ip-right
                (setf ip-direction :right)
                (advance))
              
              (:halt
                (loop-finish))
              
              (:increment-memory
                (incf (gethash dp memory 0))
                (advance))
              
              (:decrement-memory
                (decf (gethash dp memory 0))
                (advance))
              
              (:move-dp-left
                (decf dp)
                (advance))
              
              (:move-dp-right
                (incf dp)
                (advance))
              
              (:input
                (format T "~&Please input an ASCII character: ")
                (let ((input (read-char)))
                  (declare (type character input))
                  (clear-input)
                  (setf (gethash dp memory)
                        (char-code input)))
                (advance))
              
              (:output
                (write-char (code-char (gethash dp memory 0)))
                (advance))
              
              (:turn-90
                (when (zerop (gethash dp memory 0))
                  (setf ip-direction
                    (turn-direction-by-90-degrees-ccw ip-direction)))
                (advance))
              
              (:nop
                (advance))
              
              (otherwise
                (error "Invalid instruction: ~s." instruction))))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
(interpret-Bedroom
"➡️⬇️🛏
⬆️⬅️")

;;; -------------------------------------------------------

;; Concise ("golfed") infinite loop.
(interpret-Bedroom "➡️⬅️🛏")

;;; -------------------------------------------------------

;; Rectified truth-machine.
(interpret-Bedroom
"🗄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄⬇️🛏
⬇️🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸⬅️
➡️🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄⬇️⬆️⬇️
⬇️🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸⬅️⬆️🗒
➡️🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄⬇️⬆️⬆️
⬇️🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸⬅️🗒📗
➡️🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄🧸🪆📗🪄⬇️🪆🪆
⬇️🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸🪄📗🪆🧸⬅️⬆️📐
➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️➡️⬆️")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Bedroom
"⬇️  🛏
➡️🗄🗒📐⬇️
⬆️⬅️⬅️⬅️⬅️")

;;; -------------------------------------------------------

;; Print an input ASCII character and, in descending order of codes, all
;; preceding characters.
;; For example:
;;   Input  = M
;;   Output = MLKJIHGFEDCBA@?>=<;:9876543210/.-,+*)('&%$#" [...] ^B^A
(interpret-Bedroom
"⬇️  🛏
➡️🗄➡️📐🗒🧸⬇️
  ⬆️⬅️⬅️⬅️⬅️")
