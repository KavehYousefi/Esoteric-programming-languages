;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Third Party Contractor Accused Of A Robbery", or,
;; abbreviated "TPCAOAR", invented by the Esolang user "Mario" and
;; presented on March 2nd, 2021, its kenspeckle caract's expression
;; that of a two-dimensional Cartesian reticulation of symbols, to whom
;; the attrectation of an integer-valued stack constitutes the telos'
;; attendance, the operative warklumes themselves in a dioristic mode
;; derivations from the top stack element's state as integral
;; identifiers.
;; 
;; 
;; Concept
;; =======
;; The "Third Party Contractor Accused Of A Robbery", abbreviatd to
;; "TPCAOAR", constitutes a two-dimensional esoteric programm language
;; whose champarty of the stack in both the data castaldy and the
;; execution bailiwick serves as the indicial component.
;; 
;; == THE PROGRAM: A RETICULATION OF CHARACTERS ==
;; A TPCAOAR program's constitution is founded upon a two-dimensional
;; Cartesian rete accommodate commorancies to characters. Among these
;; arrangement's consectaries the most peisant sequela relates to
;; everichon among its cells' location by a twissel of integral
;; subscripts, commonly yclept the x- and y-coordinates.
;; 
;; Upon any of the grid marches' transgression, an abortive error is
;; inflicted upon the execution.
;; 
;; == THE PROGRAM MEMORY: A STACK OF INTEGER NUMBERS ==
;; Its data castaldy's architectural edification ensues from a stack
;; admitting non-negative integer numbers wisting of no imposition along
;; the upper march.
;; 
;; A parasceuastic exercise, the stack commences as a singleton, with
;; the default value of zero (0) as its inchoate element --- a requisite
;; that proceeds from the necessity to accommodate at least one item
;; for the instruction selection.
;; 
;; A further corollary begotten by the impositions, upon its vacancy,
;; the stack is automatically extended to this singleton state of zero
;; if an operation demands its collaboration.
;; 
;; == PROGRAM TERMINATION REQUIRES "911" ON THE STACK ==
;; A program's orderly cessation ostends itself as kenspeckle as the
;; ubiquitous conception of the language, by the requisition of the
;; number "911" on the stack's top.
;; 
;; 
;; Instructions
;; ============
;; The TPCAOAR programming language's cynosure, in perquisition of its
;; operative potential, constitutes an incolant of the stack dedicated
;; to the castaldy of signed integer numbers, the top element thereof
;; the agent of the current action's furnishment.
;; 
;; In order for this feelefold furcations' reification, a prevenience
;; in the aspect of the stack's attrectation constitutes a stern
;; requisitum; thilk is realized in the two-dimensional program grid,
;; its tolerance exhausted already by a twissel's admission, one moeity
;; among these concredited with the top element's incrementation, the
;; second with the associated element action's actuation.
;; 
;; In TPCAOAR the instruction communication is realized in an
;; intermediate form, encoded in the top stack element, which, when
;; taken modulo eight (8), yields a command code in the integral
;; interval [0, 7], each such member, except for the number five (5),
;; ligated into the affiliation with an operative causatum.
;; 
;; == OVERVIEW OF INSTRUCTIONS: STACK MANIPULATED BY THE GRID ==
;; The following symbols are expected to participate in the code grid,
;; their telos the stack's parasceuastic manipulation in order for the
;; consequent actions' engagement:
;; 
;;   ------------------------------------------------------------------
;;   Command   | Effect
;;   ----------+-------------------------------------------------------
;;     (space) | Increments the top stack element by one (1).
;;   ..................................................................
;;   &         | Executes the action associated with the code obtained
;;             | via the formula:
;;             | 
;;             |   topStackElement modulo 8
;;             |-------------------------------------------------------
;;             | For the recognized actions please consult the
;;             | treatise below.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW OF ACTIONS: OPERATIONS INSTIGATED BY THE STACK ==
;; The following apercu shall be vested with the capacity to adhibit a
;; cursory mete of nortelry anenst the action facilities, derived from
;; the memory stack's top element via the formula
;; 
;;   actionCode <- topStackElement modulo 8
;; 
;; The resulting action code will constitute an integral number desumed
;; from the closed interval [0, 7].
;; 
;;   ------------------------------------------------------------------
;;   Action code | Effect
;;   ------------+-----------------------------------------------------
;;   0           | Sets the top stack element to the value of the
;;               | stack item immediately below it.
;;               |-----------------------------------------------------
;;               | If no subordinate element to the top position
;;               | exists, the value zero (0) is assumed.
;;   ..................................................................
;;   1           | Rotates the instruction pointer (IP) direction
;;               | widdershins by 90 degrees.
;;               |-----------------------------------------------------
;;               | The following correspondences betwixt a
;;               | contemporaneous airt and its subsequent state
;;               | govern the principle:
;;               |   ----------------------------------
;;               |   Current direction | New direction
;;               |   ------------------+---------------
;;               |   North             | West
;;               |   ..................................
;;               |   West              | South
;;               |   ..................................
;;               |   South             | East
;;               |   ..................................
;;               |   East              | North
;;               |   ----------------------------------
;;   ..................................................................
;;   2           | Prints the character whose ASCII code conforms to
;;               | the value
;;               | 
;;               |   floor((topStackElement - 2) / 8)
;;               | 
;;               | to the standard output.
;;   ..................................................................
;;   3           | Multiplies the top stack element by the factor
;;               | 
;;               |   floor((topStackElement - 3) / 8)
;;               | 
;;               | and replaces the element by thus yielded product.
;;   ..................................................................
;;   4           | Pops the top stack element; if the new top element
;;               | does not equal zero (0), rotates the instruction
;;               | pointer (IP) direction deasil by 90 degrees, ere
;;               | popping this indagated element. Upon this new
;;               | top element's inequality with zero (0), either a
;;               | rotation nor a removal will be accompassed.
;;               |-----------------------------------------------------
;;               | In a pseudocode diction, it holds:
;;               | 
;;               |   stack.pop()
;;               |   
;;               |   let newTopStackElement <- stack.peek()
;;               |   
;;               |   if newTopStackElement != 0 then
;;               |     rotate the instruction pointer clockwise
;;               |     stack.pop()
;;               |   end if
;;   ..................................................................
;;   5           | Accompasses no causatum; thus designates a
;;               | no-operation (NOP).
;;   ..................................................................
;;   6           | Clears the stack.
;;   ..................................................................
;;   7           | Sets the top stack element to zero (0), and pushes
;;               | the value zero (0) unto the stack's top position,
;;               | thus defining a new top element.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been accomplished in the
;; programming language Common Lisp, wisting of a parasceve to its
;; symbols' procession by a rearrangement into a dedicated reticulation
;; representation, with a veridicous adit in a two-dimensional mode.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-12
;; 
;; Sources:
;;   [esolang2021TPCAOAR]
;;   The Esolang contributors,
;;     "Third Party Contractor Accused Of A Robbery",
;;     April 6th, 2021
;;   URL: "https://esolangs.org/wiki/
;;         Third_Party_Contractor_Accused_Of_A_Robbery"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an unsigned integer number
   greater than or equal to zero, but bourneless along the upper
   extremum, yielding the logical interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the same
   defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional Cartesian coordinates
   jumelle as either a complex number compact of two integer numbers,
   the first of which specifies the x-coordinate, while the latter
   contributes the y-part, or as a scalar integer representative of the
   abscissa, with the ordinate implicitly conveyed as zero (0)."
  '(or integer (complex integer)))

;;; -------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a two-dimensional arrangement
   of characcters following a parse spatial diorism, realized in the
   form of a hash table, the keys of which introduce the points as
   ``location'' instances, whereas the entries, or cell values, issue
   from ``character''s."
  '(hash-table-of location character))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines a stack composed of zero or more
   non-negative integer numbers, realized by a simple list."
  '(list-of non-negative-integer))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized choices homologated
   during the perambulation of the program's character grid."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, that
   diorism encompasses, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string construction operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arrange-the-strings-into-lines (&rest lines)
  "Concatenates the LINES, each twissel's intermede being segregated by
   an aefauld newline character, and returns a fresh string
   representation of the thus produced character sequence."
  (declare (type (list-of string) lines))
  (the string
    (format NIL "~{~a~^~%~}" lines)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of direction operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-clockwise-direction (current-direction)
  "Returns the direction assumed by rotating the CURRENT-DIRECTION in a
   clockwise fashion."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:left     :up)
      (:right    :down)
      (:up       :right)
      (:down     :left)
      (otherwise (error "Invalid direction: ~s." current-direction)))))

;;; -------------------------------------------------------

(defun get-counterclockwise-direction (current-direction)
  "Returns the direction assumed by rotating the CURRENT-DIRECTION in a
   counterclockwise fashion."
  (declare (type direction current-direction))
  (the direction
    (case current-direction
      (:left     :down)
      (:right    :up)
      (:up       :left)
      (:down     :right)
      (otherwise (error "Invalid direction: ~s." current-direction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of location operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-a-location (x y)
  "Creates and returns a new ``location'' designated by the X- and the
   Y-coordinate twissel."
  (declare (type integer x))
  (declare (type integer y))
  (the location
    (complex x y)))

;;; -------------------------------------------------------

(defun get-the-location-x (location)
  "Returns the LOCATION's x-coordinate."
  (declare (type location location))
  (the integer
    (realpart location)))

;;; -------------------------------------------------------

(defun get-the-location-y (location)
  "Returns the LOCATION's y-coordinate."
  (declare (type location location))
  (the integer
    (imagpart location)))

;;; -------------------------------------------------------

(defun translate-the-location-into (current-location direction)
  "Returns the location reached by translating the CURRENT-LOCATION one
   step into the DIRECTION."
  (declare (type location  current-location))
  (declare (type direction direction))
  (let ((current-x (get-the-location-x current-location))
        (current-y (get-the-location-y current-location)))
    (declare (type integer current-x))
    (declare (type integer current-y))
    (the location
      (case direction
        (:left     (make-a-location (1- current-x)     current-y))
        (:right    (make-a-location (1+ current-x)     current-y))
        (:up       (make-a-location     current-x  (1- current-y)))
        (:down     (make-a-location     current-x  (1+ current-y)))
        (otherwise (error "Invalid direction: ~s." direction))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Pointer".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Pointer ()
  ((location
    :initform      (make-a-location 0 0)
    :accessor      pointer-location
    :type          location
    :documentation "The position of the pointer in the character grid.")
   (direction
    :initform      :right
    :accessor      pointer-direction
    :type          direction
    :documentation "The direction along which the pointer proceeds."))
  (:documentation
    "The ``Pointer'' class serves in the encapsulation of a pointer to
     whom the commorancy in a two-dimensional character grid is
     vouchsafed, a status delineated by two pieces of information,
     namely the x-y location and the current airt."))

;;; -------------------------------------------------------

(defun prepare-a-pointer ()
  "Creates and returns a fresh ``Pointer'' instance endowed with the
   default properties."
  (the Pointer
    (make-instance 'Pointer)))

;;; -------------------------------------------------------

(defun rotate-the-pointer-deasil (pointer)
  "Rotates the POINTER in a clockwise direction by 90 degrees and
   returns no value."
  (declare (type Pointer pointer))
  (setf (pointer-direction pointer)
    (get-clockwise-direction
      (pointer-direction pointer)))
  (values))

;;; -------------------------------------------------------

(defun rotate-the-pointer-widdershins (pointer)
  "Rotates the POINTER in a counterclockwise direction by 90 degrees
   and returns no value."
  (declare (type Pointer pointer))
  (setf (pointer-direction pointer)
    (get-counterclockwise-direction
      (pointer-direction pointer)))
  (values))

;;; -------------------------------------------------------

(defun move-the-pointer (pointer)
  "Translates the POINTER one step into the current direction and
   returns no value."
  (declare (type Pointer pointer))
  (setf (pointer-location pointer)
    (translate-the-location-into
      (pointer-location  pointer)
      (pointer-direction pointer)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((pointer Pointer) (stream T))
  (declare (type Pointer     pointer))
  (declare (type destination stream))
  (format stream "(Pointer location=(~d, ~d) direction=~s)"
    (get-the-location-x
      (pointer-location pointer))
    (get-the-location-y
      (pointer-location pointer))
    (pointer-direction pointer))
  (the Pointer pointer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Grid".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Grid ()
  ((width
    :initform      0
    :accessor      grid-width
    :type          non-negative-integer
    :documentation "The number of columns comprising the grid.")
   (height
    :initform      0
    :accessor      grid-height
    :type          non-negative-integer
    :documentation "The number of rows comprising the grid.")
   (cells
    :initform      (make-hash-table :test #'eql)
    :type          character-matrix
    :documentation "A two-dimensional array of cells, conceived as a
                    parse array, and represented by a hash table, the
                    keys of which comprehend the column-row location
                    specifiers, affiliated with the cell commands."))
  (:documentation
    "The ``Grid'' class implements a two-dimensional Cartesian grid of
     characters, providing a convenient representation of a
     \"Third Party Contractor Accused Of A Robbery\" program."))

;;; -------------------------------------------------------

(defun set-the-cell-at (grid point new-entry)
  "Stores the NEW-ENTRY in the GRID cell at the POINT and returns no
   value."
  (declare (type Grid     grid))
  (declare (type location point))
  (declare (type T        new-entry))
  (with-slots (cells) grid
    (declare (type character-matrix cells))
    (setf (gethash point cells) new-entry))
  (values))

;;; -------------------------------------------------------

(defun construct-a-grid-for (code)
  "Creates and returns a new ``Grid'' as a two-dimensional
   representation of the piece of \"Third Party Contractor Accused Of A
   Robbery\" source CODE."
  (declare (type string code))
  (let ((grid   (make-instance 'Grid))
        (width  0)
        (height 0)
        (x      0)
        (y      0))
    (declare (type Grid                 grid))
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (declare (type non-negative-integer x))
    (declare (type non-negative-integer y))
    (loop for token of-type character across code do
      (case token
        (#\Newline
          (setf x 0)
          (incf y 1)
          (setf height (max height (1+ y))))
        (otherwise
          (set-the-cell-at grid (make-a-location x y) token)
          (incf x 1)
          (setf width  (max width  x))
          (setf height (max height 1)))))
    (psetf
      (grid-width  grid) width
      (grid-height grid) height)
    (the Grid grid)))

;;; -------------------------------------------------------

(defun valid-grid-point-p (grid point)
  "Determines whether the POINT specifies a valid location inside of the
   GRID's marches, returning on confirmation a ``boolan'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Grid     grid))
  (declare (type location point))
  (the boolean
    (not (null
      (and
        (< -1 (get-the-location-x point) (grid-width  grid))
        (< -1 (get-the-location-y point) (grid-height grid)))))))

;;; -------------------------------------------------------

(defun verify-the-grid-point (grid point)
  "Determines whether the POINT specifies a valid location inside of the
   GRID's marches, on confirmation returning no value, otherwise
   signaling an error of an unspecified type."
  (declare (type Grid     grid))
  (declare (type location point))
  (unless (valid-grid-point-p grid point)
    (error "The point (~d, ~d) violates the bournes of the grid ~
            with ~d columns and ~d rows."
      (get-the-location-x point)
      (get-the-location-y point)
      (grid-width     grid)
      (grid-height    grid)))
  (values))

;;; -------------------------------------------------------

(defun get-cell-at (grid point)
  "Returns the GRID cell located at the POINT, or signals an error of
   an unspecified type upon the location specifier's transcendence of
   the admissible bournes."
  (declare (type Grid     grid))
  (declare (type location point))
  (verify-the-grid-point grid point)
  (with-slots (cells) grid
    (declare (type character-matrix cells))
    (the character
      (gethash point cells #\_))))

;;; -------------------------------------------------------

(defmethod print-object ((grid Grid) (stream T))
  (declare (type Grid        grid))
  (declare (type destination stream))
  (with-slots (width height) grid
    (declare (type non-negative-integer width))
    (declare (type non-negative-integer height))
    (format stream "~&Grid with ~d columns and ~d rows:" width height)
    (dotimes (y height)
      (declare (type non-negative-integer y))
      (format stream "~&|")
      (dotimes (x width)
        (declare (type non-negative-integer x))
        (format stream "~c"
          (get-cell-at grid
            (make-a-location x y))))
      (format stream "|")))
  (the Grid grid))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((grid
    :initarg       :grid
    :initform      (error "Missing character grid.")
    :reader        interpreter-grid
    :type          Grid
    :documentation "The character grid representation of the program.")
   (ip
    :initform      (prepare-a-pointer)
    :accessor      interpreter-ip
    :type          Pointer
    :documentation "The instruction pointer (IP).")
   (stack
    :initform      NIL
    :accessor      interpreter-stack
    :type          stack
    :documentation "The program stack"))
  (:documentation
    "The ``Interpreter'' class applies itself to the evaluation of a
     \"Third Party Contractor Accused Of A Robbery\" program
     communicated in the guise of a character grid in order to entalent
     the same with actual operative causata."))

;;; -------------------------------------------------------

(defun prepare-an-interpreter (grid)
  "Creates and returns a fresh ``Interpreter'' instance dedicated to the
   \"Third Party Contractor Accused Of A Robbery\" program in the form
   of the GRID."
  (declare (type Grid grid))
  (the Interpreter
    (make-instance 'Interpreter :grid grid)))

;;; -------------------------------------------------------

(defun program-is-completed-p (interpreter)
  "Determines whether the program maintained by the INTERPRETER is
   completed, which proceeds from the presence of the number 911 on the
   stack, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (the boolean
      (not (null
        (find 911 stack :test #'=))))))

;;; -------------------------------------------------------

(defun advance-to-the-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   cell in the underlying grid and returns no value."
  (declare (type Interpreter interpreter))
  (move-the-pointer
    (interpreter-ip interpreter))
  (values))

;;; -------------------------------------------------------

(defun ensure-a-non-empty-stack (interpreter)
  "Ascertains that the stack maintained by the INTERPRETER comprehends
   at least one element by pushing the value zero (0) unto its top upon
   its vacancy, otherwise abstaining from exercising any effect, and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (stack) interpreter
    (declare (type stack stack))
    (unless stack
      (push 0 stack)))
  (values))

;;; -------------------------------------------------------

(defun peek-the-top-stack-element (interpreter)
  "Returns without removing the INTERPRETER stack's top element."
  (declare (type Interpreter interpreter))
  (ensure-a-non-empty-stack interpreter)
  (the non-negative-integer
    (first
      (interpreter-stack interpreter))))

;;; -------------------------------------------------------

(defun pop-the-top-stack-element (interpreter)
  "Removes and returns the INTERPRETER stack's top element."
  (declare (type Interpreter interpreter))
  (ensure-a-non-empty-stack interpreter)
  (the non-negative-integer
    (pop
      (interpreter-stack interpreter))))

;;; -------------------------------------------------------

(defun push-unto-the-stack (interpreter new-value)
  "Pushes the NEW-VALUE unto the INTERPRETER's stack and returns no
   value."
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer new-value))
  (push new-value
    (interpreter-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun replace-the-top-stack-element (interpreter new-value)
  "Sets the INTERPRETER's top stack element to the NEW-VALUE and returns
   no value."
  (declare (type Interpreter          interpreter))
  (declare (type non-negative-integer new-value))
  (ensure-a-non-empty-stack interpreter)
  (setf (first (interpreter-stack interpreter))
        new-value)
  (values))

;;; -------------------------------------------------------

(defun clear-the-stack (interpreter)
  "Clears the INTERPRETER's stack and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-stack interpreter) NIL)
  (values))

;;; -------------------------------------------------------

(defun verify-the-instruction-pointer-location (interpreter)
  "Determines whether the INTERPRETER's instruction pointer (IP) resides
   inside of the program grid's bournes, returning on confirmation no
   value, while signaling upon a violation of the same an error of an
   unspecified type."
  (declare (type Interpreter interpreter))
  (with-slots (grid ip) interpreter
    (declare (type Grid    grid))
    (declare (type Pointer ip))
    (unless (valid-grid-point-p grid (pointer-location ip))
      (error "The instruction pointer (IP) location (~d, ~d) ~
              violates the program grid's bournes."
        (get-the-location-x (pointer-location ip))
        (get-the-location-y (pointer-location ip)))))
  (values))

;;; -------------------------------------------------------

(defun get-the-current-character (interpreter)
  "Returns the character located at the INTERPRETER's instruction
   pointer (IP) location in the program grid."
  (declare (type Interpreter interpreter))
  (verify-the-instruction-pointer-location interpreter)
  (the character
    (with-slots (grid ip) interpreter
      (declare (type Grid    grid))
      (declare (type Pointer ip))
      (get-cell-at grid
        (pointer-location ip)))))

;;; -------------------------------------------------------

(defun get-the-current-command (interpreter)
  "Returns the command code answering to the INTERPRETER stack's top
   element, supputated by a modulo 8 transformation, and returns the
   thus produced integral value in the range [0, 7]."
  (declare (type Interpreter interpreter))
  (ensure-a-non-empty-stack interpreter)
  (the (integer 0 7)
    (mod (peek-the-top-stack-element interpreter) 8)))

;;; -------------------------------------------------------

(defun execute-the-current-command (interpreter)
  "Evaluates the current command, deriving from the INTERPRETER's top
   stack element, and returns no value."
  (declare (type Interpreter interpreter))
  (case (get-the-current-command interpreter)
    (0
      (replace-the-top-stack-element interpreter
        (or (second (interpreter-stack interpreter))
            0)))
    
    (1
      (rotate-the-pointer-widdershins
        (interpreter-ip interpreter)))
    
    (2
      (let ((tos (peek-the-top-stack-element interpreter)))
        (declare (type non-negative-integer tos))
        (write-char
          (code-char
            (floor (- tos 2) 8)))))
    
    (3
      (let ((tos (peek-the-top-stack-element interpreter)))
        (declare (type non-negative-integer tos))
        (replace-the-top-stack-element interpreter
          (* tos (floor (- tos 3) 8)))))
    
    (4
      (pop-the-top-stack-element interpreter)
      (unless (zerop (pop-the-top-stack-element interpreter))
        (rotate-the-pointer-deasil
          (interpreter-ip interpreter))
        (pop-the-top-stack-element interpreter)))
    
    (5
      NIL)
    
    (6
      (clear-the-stack interpreter))
    
    (7
      (replace-the-top-stack-element interpreter 0)
      (push-unto-the-stack   interpreter 0))
    
    (otherwise
      (error "The command ~d, corresponding to the top stack element ~
              ~d, does not associate with any recognized action."
        (get-the-current-command    interpreter)
        (peek-the-top-stack-element interpreter))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-program (interpreter)
  "Evaluates the program maintained by the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-completed-p interpreter) do
    (case (get-the-current-character interpreter)
      (#\Space
        (replace-the-top-stack-element interpreter
          (1+ (peek-the-top-stack-element interpreter))))
      (#\&
        (execute-the-current-command interpreter))
      (otherwise
        NIL))
    (advance-to-the-next-instruction interpreter))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the entry operation.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-TPCAOAR (code)
  "Interprets the pice of \"Third Party Contractor Accused Of A
   Robbery\" source CODE and returns no value."
  (declare (type string code))
  (interpret-the-program
    (prepare-an-interpreter
      (construct-a-grid-for code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generate the ASCII code of the character "A" (= 65) in a form
;; covenable to the output formula
;; 
;;   floor((topStackElement - 2) / 8)
;; 
;; that is, the number 522; print this character, increment the top
;; stack element to 911, and thus terminate the program.
(interpret-TPCAOAR
  "                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          &                                                                                                                                                                                                                                                                                                                                                                                                     ")
