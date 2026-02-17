;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Direction", invented by the Esolang user "BestCoder" and
;; presented on September 7th, 2024, its haecceity's commorancy the
;; programs' arrangement in a two-dimensional Cartesian reticulation
;; whose operative symbols, either conditionally or unconditionally as
;; a proprium of the respective specimen, may be expunged from the grid
;; in a sequela to their execution, while concomitantly operating on a
;; stack of signed integer numbers.
;; 
;; 
;; Concept
;; =======
;; The Direction programming language's edification is empight on the
;; firmament of a two-dimensional Cartesian reticulum's plasmature
;; inwith whose marches an instruction pointer (IP) peragrates,
;; executing symbolic instruction identifiers whose vanishment or
;; persistence ensues from the personal haecceity in a coefficient
;; effort with the contemporaneous configuration, operating upon a
;; stack of signed integer elements.
;; 
;; == THE PROGRAM: A TWO-DIMENSIONAL GRID OF CHARACTERS ==
;; A Direction program's conformation assigns to the same a reticulum
;; apposted in a two-dimensional Cartesian grid's semblance.
;; 
;; Engendered by this plasmature, a cell's ponibility is founded upon
;; an (x, y)-tuple's diorism, both among this twissel's lateralities
;; designated by an integral subscript from the integral range
;; commencing from inclusive zero (0).
;; 
;; == THE INSTRUCTION POINTER PERAGRATES THE FIELD ==
;; An incolant of this structure, an instruction pointer (IP), empight
;; at the execution's inchoacy in the top-left cell and, gressible in
;; its nature, oriented in this status of nascency along the dextral
;; airt, engages in a perpetual peragration athwart the field. Upon an
;; operative character's allision with the cursor, its affiliated
;; epiphenomena's accompassing is peracted.
;; 
;; == A VIOLATION OF THE FIELD BOURNES INCURS AN ERROR ==
;; If the instruction pointer's trajectory concludes in a manuduction
;; ayond the field's admissible mears, the program abort in an erroneous
;; fashion. This infliction is counterdistiguished from an orderly
;; termination via the "E" operation.
;; 
;; == DATA CASTALDY IS CONCREDITED TO A STACK'S BAILIWICK ==
;; The dever of the program data's management accompts for a stack's
;; involvement, each element's moutenance a signed integer datum, the
;; bournes of which are, hypothetically spoken, deprived of imposition.
;; 
;; 
;; Instructions
;; ============
;; Direction's instruction set tallies a cardinality of 17 members, the
;; bailiwicks amplected in its compass such in dedication to the
;; program field's navigation, the memory stack's manipulation,
;; arithmetics, input and output intercourse, as well as the reversal
;; of the perishable and permanent operations' removal policy.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be satisfied in a requisite mete
;; of nortelry's adhibition concerning the language's operative
;; warklumes:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   NAVIGATIONAL INSTRUCTIONS
;;   ------------------------------------------------------------------
;;   <       | Redirects the instruction pointer to the left.
;;           |---------------------------------------------------------
;;           | Ensuing from its execution, this symbol normally
;;           | perishes from the program grid by being substituted with
;;           | a non-operative character.
;;   ..................................................................
;;   >       | Redirects the instruction pointer to the right.
;;           |---------------------------------------------------------
;;           | Ensuing from its execution, this symbol normally
;;           | perishes from the program grid by being substituted with
;;           | a non-operative character.
;;   ..................................................................
;;   ^       | Redirects the instruction pointer upwards.
;;           |---------------------------------------------------------
;;           | Ensuing from its execution, this symbol normally
;;           | perishes from the program grid by being substituted with
;;           | a non-operative character.
;;   ..................................................................
;;   v       | Redirects the instruction pointer downwards.
;;           |---------------------------------------------------------
;;           | Ensuing from its execution, this symbol normally
;;           | perishes from the program grid by being substituted with
;;           | a non-operative character.
;;   ..................................................................
;;   ?       | Pops the top element from the memory stack; if the thus
;;           | obtained value is less than or equal to zero (0),
;;           | redirects the instruction pointer (IP) to the left;
;;           | otherwise, if the number was greater than zero (0),
;;           | changes the instruction pointer's orientation to the
;;           | right.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | execution, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ==================================================================
;;   STACK CONTENT MANIPULATION OPERATIONS
;;   ------------------------------------------------------------------
;;   0       | Pushes the number zero (0) onto the memory stack.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ..................................................................
;;   1       | Pushes the number one (1) onto the memory stack.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ..................................................................
;;   D       | Duplicates the memory stack's top element by pushing a
;;           | copy of the same onto the stack.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | execution, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ==================================================================
;;   ARITHMETIC OPERATIONS
;;   ------------------------------------------------------------------
;;   +       | Pops the top element from the memory stack, here nevend
;;           | "a", ere removing the new top element, "b", supputates
;;           | the sum (b + a), and pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a      <- stack.pop()
;;           |   let b      <- stack.pop()
;;           |   let result <- b + a
;;           |   stack.push(result)
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements,
;;           | an error of the type "EmptyStackError" is signaled
;;           | during the violating removal request.
;;   ..................................................................
;;   -       | Pops the top element from the memory stack, here nevend
;;           | "a", ere removing the new top element, "b", supputates
;;           | the difference (b - a), and pushes the result onto the
;;           | stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a      <- stack.pop()
;;           |   let b      <- stack.pop()
;;           |   let result <- b - a
;;           |   stack.push(result)
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements,
;;           | an error of the type "EmptyStackError" is signaled
;;           | during the violating removal request.
;;   ..................................................................
;;   *       | Pops the top element from the memory stack, here nevend
;;           | "a", ere removing the new top element, "b", supputates
;;           | the product (b * a), and pushes the result onto the
;;           | stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a      <- stack.pop()
;;           |   let b      <- stack.pop()
;;           |   let result <- b * a
;;           |   stack.push(result)
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements,
;;           | an error of the type "EmptyStackError" is signaled
;;           | during the violating removal request.
;;   ..................................................................
;;   /       | Pops the top element from the memory stack, here nevend
;;           | "a", ere removing the new top element, "b", supputates
;;           | the integer quotient floor(b / a), and pushes the result
;;           | onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           |   let a      <- stack.pop()
;;           |   let b      <- stack.pop()
;;           |   let result <- floor(b / a)
;;           |   stack.push(result)
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate at least two elements,
;;           | an error of the type "EmptyStackError" is signaled
;;           | during the violating removal request.
;;   ==================================================================
;;   INPUT AND OUTPUT OPERATIONS
;;   ------------------------------------------------------------------
;;   I       | Queries the standard input for a signed or unsigned
;;           | integer number and pushes the response onto the memory
;;           | stack.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ..................................................................
;;   O       | Pops the top element from the memory stack and prints
;;           | thilk in its verbatim numeric form.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this operation's
;;           | execution, an error of the type "EmptyStackError" is
;;           | signaled.
;;   ==================================================================
;;   COMMAND DELETION AND RETENTION OPERATIONS
;;   ------------------------------------------------------------------
;;   S       | If the next visited command constitutes a perishable
;;           | specimen, that is, either "<", ">", "^", or "v", the
;;           | same will not be deleted after its execution; otherwise,
;;           | for a permanent command or a non-operative symbol, no
;;           | further causatum is accompassed.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ..................................................................
;;   N       | If the next visited command constitutes a permanent
;;           | specimen, the same will be deleted after its execution;
;;           | otherwise, for a perishable command or a non-operative
;;           | symbol, the default behavior applies.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ==================================================================
;;   CONTROL FLOW DUCTION OPERATIONS
;;   ------------------------------------------------------------------
;;   E       | Immediately terminates the program in an orderly
;;           | fashion.
;;           |---------------------------------------------------------
;;           | This symbol, in the normal case, does not perish after
;;           | its execution.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter, implemented in the programming language Common
;; Lisp, peracts its wike's fulfilment by a rearrangement of the
;; Direction source code string into a veridical two-dimensional grid
;; format, serving as a parasceve to the ultimate entelechy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-02-14
;; 
;; Sources:
;;   [esolang:2025:Direction]
;;   The Esolang contributors, "Direction", January 5th, 2025
;;   URL: "https://esolangs.org/wiki/Direction"
;;   
;; Sources:
;;   [goodrich:2006:datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 187--203 describe the concept and two implementations
;;        of the stack abstract data type (ADT).
;;        o The pages 192--196 describe an array-based implementation
;;          of the stack ADT.
;;        o The pages 197--198 describe the implementation of the stack
;;          ADT via a singly linked list.
;;          - This implementation, norned "NodeStack", is founded upon a
;;            bespoke node class and an ensuing linked list.
;;   
;;   [goodrich:2014:datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 226--237 describe the concept and two implementations
;;        of the stack abstract data type (ADT).
;;       o The pages 230--232 describe an array-based implementation
;;          of the stack ADT.
;;       o The page 233 describes the implementation of the stack ADT
;;         via a singly linked list.
;;         - This implementation, nevend "LinkedStack", relies on the
;;           "SinglyLinkedList" class implemented in the section 3.2.1,
;;           occupying the pages 126--127.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes the NAME's
   dation, and which acts as a pernor to the LAMBDA-LIST's ipsissima
   verba specifications as its personal formal parameters, concomitantly
   assigning the probed object to the CANDIDATE-NAME, evaluates the BODY
   forms, and construes the desinent form's primary return value as the
   docimasy's adjudgment, a \"generalized boolean\" truth value of
   \"true\" peracting a successful compatibility's assessment's
   signification, while a \"false\" response concludes in the
   candidate's rejection.
   ---
   The first BODY form, in the case of its resolution to a string
   object, is adhibited the role of a documentation string to the type
   definition, being, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
       ,(or (and (stringp (first body))
               (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency tallying zero or more entries, everichon member among
   these dimidiated into a key compliant with the KEY-TYPE and an allied
   value of the VALUE-TYPE, both governed by a configuration which
   assigns the generic sentinel ``*'' as the default state."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*))
  "The ``list-of'' type defines a linked list comprehending zero or more
   members, each element partaking of the same complying with the
   ELEMENT-TYPE, for thilk is specified the generic sentinel ``*'' as
   the default."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, whose componency's edification amplects zero or more entries,
   everichon among these a cons cell whose sinistral compartment lends
   a commorancy to the key, adhering to the KEY-TYPE, while the dextral
   moeity accommodates the associated value, subsuming into the
   VALUE-TYPE, for both is imposed the default configuration of the
   generic sentinel ``*''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional coordinates twissel
   comprehending an x-coordinate, consanguinous with a column index, and
   a y-coordinate that corresponds to a row's subscript, ensconced in a
   complex of a ``fixnum'' componency or a scalar ``fixnum''.
   ---
   The concrete resolution of the ``location'' to either a biscalar or
   scalar integral number ensues from the diorisms of the system of
   nomothesia known in Common Lisp as the \"rule of canonical
   representation for complex number\"; it imposes the imperative
   conversion of an originally complex object into a scalar subsumed
   into the provenance's real part's species if and only if the
   imaginary part complies with the integer type and entertains an
   owelty with the value zero (0).
   ---
   In our particular context and construe, the ``location'' will be
   transformed into a ``fixnum'' scalar in the aefauld case of its
   y-coordinate's equiparation with zero (0); in any other circumstance,
   the representation retains its biscalar constitution."
  '(or fixnum (complex fixnum)))

;;; -------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a sparse two-dimensional
   reticulation thilk apposts its character-valued cells in a mode of
   ponibility that complies to an access via an (x, y)-tuple subscript,
   its manifestation that of a hash table whose keys furnish the
   position designator as a ``location'', and whose values are accompted
   for by characters."
  '(hash-table-of location character))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized variation on
   directions along which the program flow may peragrate."
  '(member :left :right :up :down))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a character endowed with
   the capacitation for a linebreak's incurrence, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (member candidate '(10 11 12 13)
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun perishable-command-p (candidate)
  "Determines whether the COMMAND tallies among the commands of a
   perishable haecceity, intended for an expungement succeeding their
   execution, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (find candidate "<>^v" :test #'char=))))

;;; -------------------------------------------------------

(defun permanent-command-p (candidate)
  "Determines whether the COMMAND tallies among the commands of a
   permanent haecceity, intended for their retention succeeding their
   execution, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (find candidate "?01DIO+-*/SNE" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-the-lines (&rest lines)
  "Creates and returns a fresh simple string by concatenating the LINES
   in their specified order, each twissel's interstice's alligation
   peracted by adminiculum of a single newline character.
   ---
   This operation's telos wones in the commodious furnishment of a
   warklume for the assemblage of pseudo-two-dimensional strings, nait
   already for their rearrangement into a ``Field'' through the efforts
   of the function ``build-a-field-from''."
  (declare (type (list-of string) lines))
  (the simple-string
    (coerce
      (format NIL "~{~a~^~%~}" lines)
      'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the location operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun specify-a-location (x y)
  "Creates and returns a fresh ``location'' specified by the horizontal
   X-coordinate and the vertical Y-coordinate."
  (declare (type fixnum x))
  (declare (type fixnum y))
  (the location
    (complex x y)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program code field.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Field ()
  ((width
    :initform      0
    :accessor      field-width
    :type          fixnum
    :documentation "The non-negative tally of columns edifying the
                    field.")
   (height
    :initform      0
    :accessor      field-height
    :type          fixnum
    :documentation "The non-negative tally of rows edifying the field.")
   (cells
    :initform      (make-hash-table :test #'eql)
    :accessor      field-cells
    :type          character-matrix
    :documentation "A sparse two-dimensional array of characters, its
                    cells' amenability airted towards ``location''
                    designators as (x, y)-tuple representatives."))
  (:documentation
    "The ``Field'' class serves in the representation of a Direction
     source code string in a veridicous two-dimensional Cartesian
     plasmature."))

;;; -------------------------------------------------------

(defun valid-field-coordinates-p (field x y)
  "Determines whether the position specified via the coordinates twain
   (X, Y) designates a valid cell in the FIELD, and returns three
   values:
     (1) If the coordinates jumelle (X, Y) is valid, a ``boolean'' value
         of ``T'', otherwise ``NIL''.
     (2) The maximum valid x-coordinate into the FIELD.
     (3) The maximum valid y-coordinate into the FIELD."
  (declare (type Field  field))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (the (values boolean fixnum fixnum)
    (let ((maximum-x (1- (field-width  field)))
          (maximum-y (1- (field-height field))))
      (declare (type fixnum maximum-x))
      (declare (type fixnum maximum-y))
      (values
        (resolve-to-a-boolean-value
          (and (<= 0 x maximum-x)
               (<= 0 y maximum-y)))
        maximum-x
        maximum-y))))

;;; -------------------------------------------------------

(defun validate-the-field-coordinates (field x y)
  "Determines whether the position specified via the coordinates twain
   (X, Y) designates a valid cell in the FIELD, returning on
   confirmation no value without accompassing further causata; otherwise
   signals an error of an unspecified type."
  (declare (type Field  field))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (multiple-value-bind (valid-coordinates-p maximum-x maximum-y)
      (valid-field-coordinates-p field x y)
    (declare (type boolean valid-coordinates-p))
    (declare (type fixnum  maximum-x))
    (declare (ignorable    maximum-x))
    (declare (type fixnum  maximum-y))
    (declare (ignorable    maximum-y))
    (unless valid-coordinates-p
      (error "The point (x = ~d, y = ~d) violates the admissible ~
              field bournes of [0, ~d] x [0, ~d]."
        x y maximum-x maximum-y)))
  (values))

;;; -------------------------------------------------------

(defun request-the-field-character-at (field x y)
  "Returns the character located at the zero-based coordinates (X, Y)
   into the FIELD's cells."
  (declare (type Field  field))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (validate-the-field-coordinates field x y)
  (the character
    (gethash
      (specify-a-location x y)
      (field-cells field)
      #\Space)))

;;; -------------------------------------------------------

(defun change-the-field-character-at (field x y new-character)
  "Substitutes the FIELD character located at the position (X, Y) by
   the NEW-CHARACTER and returns no value."
  (declare (type Field     field))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type character new-character))
  (validate-the-field-coordinates field x y)
  (setf
    (gethash
      (specify-a-location x y)
      (field-cells field))
    new-character)
  (values))

;;; -------------------------------------------------------

(defun build-a-field-from (source)
  "Creates and returns a fresh ``Field'', intended to provide a
   veridicous two-dimensional plasmature to the Direction SOURCE
   string."
  (declare (type string source))
  (let ((new-field (make-instance 'Field)))
    (declare (type Field new-field))
    (with-slots (width height cells) new-field
      (declare (type fixnum           width))
      (declare (type fixnum           height))
      (declare (type character-matrix cells))
      (loop
        with x             of-type fixnum    = 0
        with y             of-type fixnum    = 0
        
        for  current-token of-type character across source
        
        if (newline-character-p current-token) do
          (setf x      0)
          (incf y      1)
          (incf height 1)
        else do
          (setf (gethash (specify-a-location x y) cells)
                current-token)
          (incf x)
          (setf width  (max x width))
          (setf height (max 1 height))
        end
        
        do (setf height (max y height))))
    (the Field new-field)))

;;; -------------------------------------------------------

(defun print-the-field-character-at (field x y destination)
  "Prints the character located at the zero-based coordinates (X, Y)
   into the FIELD's cells to the DESTINATION and returns no value."
  (declare (type Field  field))
  (declare (type fixnum x))
  (declare (type fixnum y))
  (declare (type stream destination))
  (write-char
    (request-the-field-character-at field x y)
    destination)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((field Field) (stream T))
  (declare (type Field  field))
  (declare (type stream stream))
  (format stream "~&Field of ~d x ~d cells:"
    (field-width  field)
    (field-height field))
  (format stream "~&-----------------------")
  (dotimes (y (field-height field))
    (declare (type fixnum y))
    (fresh-line stream)
    (dotimes (x (field-width field))
      (declare (type fixnum x))
      (print-the-field-character-at field x y stream)))
  (the Field field))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the cursor.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cursor ()
  ((location
    :initform      (specify-a-location 0 0)
    :accessor      cursor-location
    :type          location
    :documentation "The current location in the ensconcing field.")
   (direction
    :initform      :right
    :accessor      cursor-direction
    :type          direction
    :documentation "The current peragration airt."))
  (:documentation
    "The ``Cursor'' class limns the dever's pernor which encumbers thilk
     with the representation of an instruction pointer (IP), commorant
     in the discrete two-dimensional Cartesian space defined by integral
     intervals, its diorism a complex of a location and a direction,
     their champarty capacitates its vagility."))

;;; -------------------------------------------------------

(defun prepare-a-cursor ()
  "Creates and returns a fresh ``Cursor'', at its inchoate state being
   empight on the left upper corner (x = 0, y = 0)."
  (the Cursor
    (make-instance 'Cursor)))

;;; -------------------------------------------------------

(defun locate-the-cursor (cursor)
  "Returns the CURSOR's location as two values:
     (1) The cursor's x-coordinate.
     (2) The cursor's y-coordinate."
  (declare (type Cursor cursor))
  (the (values fixnum fixnum)
    (values
      (realpart (cursor-location cursor))
      (imagpart (cursor-location cursor)))))

;;; -------------------------------------------------------

(defun advance-the-cursor (cursor)
  "Advances the CURSOR one step into its contemporaneously empight
   direction and returns no value."
  (declare (type Cursor cursor))
  (with-slots (location direction) cursor
    (declare (type location  location))
    (declare (type direction direction))
    (incf location
      (case direction
        (:left     (complex -1  0))
        (:right    (complex +1  0))
        (:up       (complex  0 -1))
        (:down     (complex  0 +1))
        (otherwise (error "The direction ~s is invalid." direction)))))
  (values))

;;; -------------------------------------------------------

(defun redirect-the-cursor-to (cursor new-direction)
  "Modulates the CURSOR's airt to the NEW-DIRECTION and returns no
   value."
  (declare (type Cursor    cursor))
  (declare (type direction new-direction))
  (setf (cursor-direction cursor) new-direction)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the linked stack's node.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor prepare-a-node (element successor)))
  "The ``Node'' class furnishes an implementation of a singly linked 
   list node, nait for the purpose of its deployment in the linked
   ``Stack'' implementation.
   ---
   Such a node's conformation enumerates the represented element, as
   well as an optional reference to the successor node, which, in the
   parlance of the stack data structure, conflates with the next lower
   node in the vertically bedight catena."
  (element   0   :type integer        :read-only T)
  (successor NIL :type (or null Node) :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the node-based integer stack.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Stack ()
  ((top
    :initform      NIL
    :accessor      stack-top
    :type          (or null Node)
    :documentation "The node located at the stack's top position.
                    ---
                    If the stack is empty, this object amounts to the
                    ``NIL'' sentinel.")
   (size
    :initform      0
    :accessor      stack-size
    :type          (integer 0 *)
    :documentation "The tally of elements concredited to this stack's
                    castaldy."))
  (:documentation
    "The ``Stack'' class implements the stack abstract data type (ADT)
     edified upon the firmament of a singly linked list.
     ---
     This species of implementation maintains a reference to the top
     node, at its inchoacy ``NIL''-valued, which points to the successor
     as the next lower element on the stack; proceeding in a recursive
     fashion to the bottom node, for whom no succeeding link
     exists."))

;;; -------------------------------------------------------

(defun prepare-an-empty-stack ()
  "Creates and returns an initially empty ``Stack''."
  (the Stack
    (make-instance 'Stack)))

;;; -------------------------------------------------------

(defun stack-is-empty-p (stack)
  "Determines whether the STACK is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Stack stack))
  (the boolean
    (resolve-to-a-boolean-value
      (zerop
        (stack-size stack)))))

;;; -------------------------------------------------------

(defun push-onto-the-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the STACK's top position and returns no
   value."
  (declare (type Stack   stack))
  (declare (type integer new-element))
  (setf (stack-top stack)
    (prepare-a-node new-element
      (stack-top stack)))
  (incf (stack-size stack))
  (values))

;;; -------------------------------------------------------

(defun peek-into-the-stack (stack)
  "Returns without its removal the top element from the STACK.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Stack stack))
  (the integer
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error
        :format-control "You cannot peek into an empty stack.")
      (node-element
        (stack-top stack)))))

;;; -------------------------------------------------------

(defun pop-from-the-stack (stack)
  "Removes and returns the STACK's top element.
   ---
   If the STACK is empty at the instant of this operation's invocation,
   an error of the type ``Empty-Stack-Error'' is signaled."
  (declare (type Stack stack))
  (the integer
    (if (stack-is-empty-p stack)
      (error 'Empty-Stack-Error
        :format-control "You cannot pop from an empty stack.")
      (prog1
        (node-element
          (stack-top stack))
        (setf (stack-top stack)
          (node-successor
            (stack-top stack)))
        (decf (stack-size stack))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke condition types.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Empty-Stack-Error (simple-error)
  ()
  (:documentation
    "The ``Empty-Stack-Error'' condition type serves in the apprizal
     about an anomalous circumstance whose etiology wones in the trial
     to access a stack, either by attending to a mere perquisition or
     a deleterious modification applied upon its top position, while
     the salvatory's status is empight in vacancy."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Direction interpreter.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Interpreter) (values))
                update-the-current-character))

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((field
    :initarg       :field
    :initform      (error "No program field has been furnished.")
    :type          Field
    :documentation "The Direction code bedight in a two-dimensional
                    reticulation's plasmature.")
   (cursor
    :initform      (prepare-a-cursor)
    :accessor      program-cursor
    :type          Cursor
    :documentation "The instruction pointer (IP) as a composition of a
                    two-dimensional position and a direction.")
   (current-character
    :initform      NIL
    :accessor      current-field-character
    :type          (or null character)
    :documentation "The character located at the CURSOR's position into
                    the FIELD.")
   (memory
    :initform      (prepare-an-empty-stack)
    :type          Stack
    :documentation "The program memory as a stack of signed integer
                    numbers.")
   (retains-next-perishable-command-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the next
                    command, if, by its own haecceity, would conclude
                    its processing via a consequent expungement, shall
                    yet be retained.")
   (removes-next-permanent-command-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the next
                    command, if, by its own haecceity, would conclude
                    its processing without a consequent vanishment,
                    shall yet be expunged.")
   (program-has-halted-p
    :initform      NIL
    :accessor      program-has-halted-p
    :type          boolean
    :documentation "A Boolean flag which determines whether the
                    program's execution has ceased as a consectary
                    begotten from the \"E\" command's evaluation."))
  (:documentation
    "The ``Interpreter'' class applies itself to the dever of
     accompassing actual efficacy to a Direction program whose code has
     been communicated in the plasmature of a two-dimensional field of
     characters."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Stores the character located under the INTERPRETER's cursor in the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (update-the-current-character interpreter)
  (values))

;;; -------------------------------------------------------

(defun prepare-an-interpreter-for (field)
  "Creates and returns a fresh ``Interpreter'' dedicated to the
   Direction program FIELD's execution."
  (declare (type Field field))
  (the Interpreter
    (make-instance 'Interpreter :field field)))

;;; -------------------------------------------------------

(defun furnish-an-interpreter-for (field)
  "Creates and returns a fresh ``Interpreter'' assigned to the wike of
   the Direction program FIELD's execution."
  (declare (type Field field))
  (the Interpreter
    (make-instance 'Interpreter :field field)))

;;; -------------------------------------------------------

(defun locate-the-current-field-cell (interpreter)
  "Returns the current position of the INTERPRETER's field cursor as
   two values:
     (1) The cursor's x-coordinate.
     (2) The cursor's y-coordinate."
  (declare (type Interpreter interpreter))
  (the (values fixnum fixnum)
    (locate-the-cursor
      (program-cursor interpreter))))

;;; -------------------------------------------------------

(defun request-the-current-character (interpreter)
  "Returns the character contemporaneously selected by the INTERPRETER's
   cursor."
  (declare (type Interpreter interpreter))
  (the character
    (with-slots (field) interpreter
      (declare (type Field field))
      (multiple-value-bind (cursor-x cursor-y)
          (locate-the-current-field-cell interpreter)
        (declare (type fixnum cursor-x))
        (declare (type fixnum cursor-y))
        (request-the-field-character-at field cursor-x cursor-y)))))

;;; -------------------------------------------------------

(defun update-the-current-character (interpreter)
  "Stores the character located under the INTERPRETER's cursor in the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (multiple-value-bind (cursor-x cursor-y)
      (locate-the-current-field-cell interpreter)
    (declare (type fixnum cursor-x))
    (declare (type fixnum cursor-y))
    (with-slots (current-character field) interpreter
      (declare (type (or null character) current-character))
      (declare (type Field               field))
      (setf current-character
        (when (valid-field-coordinates-p field cursor-x cursor-y)
          (request-the-current-character interpreter)))))
  (values))

;;; -------------------------------------------------------

(defun apply-the-binary-operator-to-the-stack (interpreter operator)
  "Removes the top element from the INTERPRETER's memory stack, here
   nevened \"a\", succeeded by a removal of the new top item as \"b\",
   invokes the OPERATOR with \"b\" as the sinistral (first) operand and
   \"a\" as the dextral (second) input, supputates the result, pushes
   thilk onto the stack, and returns no value.
   ---
   In a mode entalented with compendiousness, the following pseudocode
   formula applies to the principle:
     let a      <- stack.pop()
     let b      <- stack.pop()
     let result <- b operator a
     stack.push(result)"
  (declare (type Interpreter                          interpreter))
  (declare (type (function (integer integer) integer) operator))
  (with-slots (memory) interpreter
    (declare (type Stack memory))
    (let ((right-operand (pop-from-the-stack memory))
          (left-operand  (pop-from-the-stack memory)))
      (declare (type integer right-operand))
      (declare (type integer left-operand))
      (push-onto-the-stack memory
        (funcall operator left-operand right-operand))))
  (values))

;;; -------------------------------------------------------

(defun current-character-shall-be-removed-p (interpreter)
  "Determines whether the INTERPRETER's currently traversed character
   shall be deleted after its execution, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-slots (current-character
                 retains-next-perishable-command-p
                 removes-next-permanent-command-p)
        interpreter
      (declare (type (or null character) current-character))
      (declare (type boolean retains-next-perishable-command-p))
      (declare (type boolean removes-next-permanent-command-p))
      (or (and (perishable-command-p current-character)
               (not retains-next-perishable-command-p))
          (and (permanent-command-p current-character)
               removes-next-permanent-command-p)))))

;;; -------------------------------------------------------

(defun delete-the-current-character (interpreter)
  "Deletes the character in the INTERPRETER's currently processed field
   cell by its supersession via a space and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (field) interpreter
    (declare (type Field field))
    (multiple-value-bind (cursor-x cursor-y)
        (locate-the-current-field-cell interpreter)
      (declare (type fixnum cursor-x))
      (declare (type fixnum cursor-y))
      (change-the-field-character-at field cursor-x cursor-y #\Space)))
  (values))

;;; -------------------------------------------------------

(defun process-the-current-character (interpreter)
  "Evaluates the INTERPRETER's currently selected field symbol and
   returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (cursor
               memory
               retains-next-perishable-command-p
               removes-next-permanent-command-p
               program-has-halted-p)
      interpreter
    (declare (type Cursor  cursor))
    (declare (ignorable    cursor))
    (declare (type boolean retains-next-perishable-command-p))
    (declare (ignorable    retains-next-perishable-command-p))
    (declare (type boolean removes-next-permanent-command-p))
    (declare (ignorable    removes-next-permanent-command-p))
    (declare (type boolean program-has-halted-p))
    (declare (ignorable    program-has-halted-p))
    
    (let ((deletes-the-current-character
            (current-character-shall-be-removed-p interpreter)))
      (declare (type boolean deletes-the-current-character))
      
      (case (current-field-character interpreter)
        ;; Outside of the program field's admissible bournes?
        ((NIL)
          (multiple-value-bind (cursor-x cursor-y)
              (locate-the-current-field-cell interpreter)
            (declare (type fixnum cursor-x))
            (declare (type fixnum cursor-y))
            (error "The program cursor, located at (x = ~d, y = ~d), ~
                    has left the field while peragrating in the ~
                    direction ~s."
              cursor-x cursor-y
              (cursor-direction
                (program-cursor interpreter)))))
        
        ;; Navigational operations.
        (#\<
          (redirect-the-cursor-to cursor :left))
        (#\>
          (redirect-the-cursor-to cursor :right))
        (#\^
          (redirect-the-cursor-to cursor :up))
        (#\v
          (redirect-the-cursor-to cursor :down))
        (#\?
          (if (<= (pop-from-the-stack memory) 0)
            (redirect-the-cursor-to cursor :left)
            (redirect-the-cursor-to cursor :right)))
        
        ;; Stack content manipulation operations.
        (#\0
          (push-onto-the-stack memory 0))
        (#\1
          (push-onto-the-stack memory 1))
        (#\D
          (push-onto-the-stack memory
            (peek-into-the-stack memory)))
        
        ;; Arithmetic operations.
        (#\+
          (apply-the-binary-operator-to-the-stack interpreter #'+))
        (#\-
          (apply-the-binary-operator-to-the-stack interpreter #'-))
        (#\*
          (apply-the-binary-operator-to-the-stack interpreter #'*))
        (#\/
          (apply-the-binary-operator-to-the-stack interpreter #'floor))
        
        ;; Input/output operations.
        (#\I
          (format T "~&>> ")
          (finish-output)
          (push-onto-the-stack memory
            (parse-integer
              (read-line NIL NIL "0")))
          (clear-input))
        (#\O
          (format T "~&~d"
            (pop-from-the-stack memory)))
        
        ;; Command deletion and retention operations.
        (#\S
          (setf retains-next-perishable-command-p T))
        (#\N
          (setf removes-next-permanent-command-p T))
        
        ;; Control flow duction operations.
        (#\E
          (setf program-has-halted-p T))
        
        (otherwise
          NIL))
      
      (when deletes-the-current-character
        (delete-the-current-character interpreter))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-character (interpreter)
  "Advances the INTERPRETER's field cursor to the next cell along its
   currently configured trajectory and returns no value."
  (declare (type Interpreter interpreter))
  (advance-the-cursor
    (program-cursor interpreter))
  (update-the-current-character interpreter)
  (values))

;;; -------------------------------------------------------

(defun execute-the-program (interpreter)
  "Executes the Direction program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-has-halted-p interpreter) do
    (process-the-current-character interpreter)
    (advance-to-the-next-character interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-direction-code (code)
  "Interprets the piece of Direction source CODE and returns no value."
  (declare (type string code))
  (execute-the-program
    (prepare-an-interpreter-for
      (build-a-field-from code)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-direction-code-lines (&rest code-lines)
  "Concatenates the CODE-LINES into a single string, interprets the
   same as a piece of DIRECTION source code, executes thilk, and returns
   no value."
  (declare (type (list-of string) code-lines))
  (interpret-the-direction-code
    (apply #'concatenate-the-lines code-lines))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the countdown sequence 3, 2, 1.
(interpret-the-direction-code-lines
  "111++SvS      <"
  "      D        "
  "     v?DO1-Sv S"
  "     E      S  "
  "            >S^")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-the-direction-code "IOE")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a zero (0) number input.
(interpret-the-direction-code-lines
  "Sv      <"
  " S      S"
  " >IDOSv ^"
  "        S"
  "     E?S^")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-direction-code-lines
  "IDOSvSOD<"
  "    D   S"
  "   E?S>S^")
