;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Typespam", invented by the Esolang user "TheZipCreator" and
;; presented on August 4th, 2017, the concept of which loosely derives
;; from Urban Mueller's language "brainfuck', distinguished, however,
;; in the visualization of its memory as a vertically aligned "chain" in
;; lieu of a horizontal tape, the same admits integers as well as
;; characters as "instances", and the kenspeckle control flow mechanims
;; based upon markers.
;; 
;; 
;; Concept
;; =======
;; Typespam programs operate on a vertical arrangement of cells, the
;; "chain", each component of which may store an "instance", that is,
;; either a character or an integer.
;; 
;; == CONTROL FLOW IS MARKER-BASED ==
;; A dioristic element of its haecceity, Typespam permits the duction of
;; the program flow via marker-based gotos.
;; 
;; The command "|" interprets the subsequent character as a new marker
;; name, the same is registered with the instruction pointer (IP)
;; location. Transpiring from an encounter with the instruction "}",
;; itself expecting a succeeding character as such an identifier, the
;; marker answering to this character is retrieved, and the instruction
;; pointe relocated to the memorized position.
;; 
;; 
;; Architecture
;; ============
;; An aspect of its indicium, Typespam supplants the infinite tape of
;; brainfuck, usually conceived as a horizontal extent of byte-valued
;; cells, by the vertically aligned innumerate chain, an entity of
;; enhanced potence, as it embraces any integer or character.
;; 
;; == THE CHAIN: A VERTICAL SERIES OF CELLS ==
;; The chain's ordonnance registers an infinite account of cells,
;; designed in a vertical arrangement.
;; 
;; == THE CELL: A CHARACTER OR INTEGER CONTAINER ==
;; The proprium of each of chain's components is limned by its ability
;; to amplect an aefauld instance, that is, a character or integer of
;; any sign and magnitude. A warklume of this diversity, associated with
;; any cell is its type.
;; 
;; It bears the similitude of a variable that a cell is not responsive
;; to actions in its inchoate state; prior to its employment, an
;; initialization ought to be applied to homologate examinations or
;; modifications.
;; 
;; == THE CELL POINTER: A CURSOR ==
;; At any instance, a cursor, known as the "cell pointer", selects the
;; currently active unit among the cells, its amenability extending to
;; all indagations and manipulations. Proeeding from the chain's
;; idopathic design, the pointer is illustrated as a mobile object that
;; may be translated upwards or downwards.
;; 
;; 
;; Data Types
;; ==========
;; Typespam's bivial type system distinguishes its entities, known as
;; instances, into characters and signed integers of any magnitude.
;; 
;; 
;; Instructions
;; ============
;; Typespam tallies a decimal of instructions, their wikes covering the
;; bailiwicks of chain management in topology and content, input,
;; output, as well as a marker-based control flow helming.
;; 
;; == OVERVIEW ==
;; A cursory apercu shall communicate the foundational aspects of the
;; language's competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   _       | Initializes the current cell.
;;   ..................................................................
;;   ^       | Moves the cell pointer one step up the chain.
;;   ..................................................................
;;   v       | Moves the cell pointer one step down the chain.
;;   ..................................................................
;;   c       | Designates the current cell as capable of holding a
;;           | character instance.
;;   ..................................................................
;;   i       | Designates the current cell as capable of holding an
;;           | integer instance.
;;   ..................................................................
;;   {       | Consumes the subsequent character or integer following
;;           | this "{" command and stores it in the current cell.
;;   ..................................................................
;;   |       | Consumes the next character in the program and creates a
;;           | new marker which associates this character as a label
;;           | with the position of its encounter for future jump
;;           | commands, for which please see the "}" operation.
;;           |---------------------------------------------------------
;;           | Any already extant marker with the same name is tacitly
;;           | superceded by a new definition.
;;   ..................................................................
;;   }       | Consumes the next character as a marker name. If the
;;           | current cell's value is not zero (0), moves the
;;           | instruction pointer (IP) to the position associated with
;;           | this marker. Otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | An error of an unspecified type is signaled if the
;;           | marker name does not correspond to any registered
;;           | entity.
;;   ..................................................................
;;   -       | Displays the value of the current cell in the standard
;;           | output.
;;   ..................................................................
;;   #       | Queries the standard input for a character and stores
;;           | it in the current cell.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-01-04
;; 
;; Sources:
;;   [esolang2020Typespam]
;;   The Esolang contributors, "Typespam", June 15th, 2020
;;   URL: "https://esolangs.org/wiki/Typespam"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  "The ``instance-type'' defines the possible data types that an
   instance is permitted to assume.
   ---
   Apart from the canonical elements, a special sentinel, ``:unknown'',
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
  "Prints the CELL's value to the DESTINATION, which defaults to ``T'',
   and returns the CELL."
  (declare (type Cell        cell))
  (declare (type destination destination))
  (case (cell-type cell)
    (:character
      (write-char (code-char (cell-value cell)) destination))
    (:integer
      (format destination " ~d" (cell-value cell)))
    (otherwise
      (error "Cell cannot display as a type of ~s." (cell-type cell))))
  (the Cell cell))

;;; -------------------------------------------------------

(defun cell-is-nonzero (cell)
  "Checks whether the CELL contains a non-zero value, returning a
   ``boolean'' result of ``T'' upon confirmation, and ``NIL''
   otherwise."
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
            "Checks whether the current instance is selected, throwing
             an error of an unspecified type if not, otherwise simply
             returning no value."
            (unless (gethash pointer chain)
              (error "The instance at the index ~d is not initialized."
                pointer))
            (values))
           
           
           (set-marker (marker-name marker-position)
            "Associates the MARKER-NAME with the MARKER-POSITION in the
             CODE, overwriting any extant entry with such a name, and
             returns no value."
            (declare (type character marker-name))
            (declare (type fixnum    marker-position))
            (setf (gethash marker-name markers) marker-position)
            (values))
           
           (get-marker-position (marker-name)
            "Returns the position in the CODE designating the start of
             the body of the marker with the MARKER-NAME.
             ---
             If no MARKER-NAME association exists yet, the respective
             declaration is searched starting from the beginning of the
             CODE, and, if found, an association is established ere its
             returning. If no marker with this identifier can be
             detected, an error is signaled."
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

;; Print "Hello World".
(interpret-Typespam "_c {H-{e-{l--{o-{ -{W-{o-{r-{l-{d-")

;;; -------------------------------------------------------

;; Infinite cat program.
(interpret-Typespam "_ c |A #- }A")
