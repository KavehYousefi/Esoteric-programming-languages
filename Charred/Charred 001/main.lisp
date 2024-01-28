;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Charred", invented by the Esolang user "SpaceByte" and
;; presented on July 17th, 2022, which, maugre the partiality in lealty
;; to Urban Mueller's "brainfuck", in particular with respect to the
;; tape-based architecture and the symbols employed for the instruction
;; designation, deploys a dioristic species of character encoding
;; eloigned from the ASCII standard for its data castaldy and
;; communicative purposes, as well as a divergent set of warklumes for
;; the control flow management.
;; 
;; 
;; Concept
;; =======
;; The Charred programming language's kenspeckle episema amplects in its
;; perimeter a set of niladic instructions denoted by aefauld symbols,
;; a tape of 30000 cells' contribution, each such an integer number's
;; salvatory, the datum of which is desumed from the closed interval
;; [0, 52], with the currently active member consigned to a cell
;; pointer's dever, as well as a peculiarly encoded character
;; repertoire, entalented with a 53 components' cardinality, assigning
;; to the integral codes from the range [0, 52] the space, as well as
;; the Latin minuscules and majuscules.
;; 
;; == CHARRED EMPLOYS A BESPOKE CHARACTER REPERTOIRE ==
;; A bespoke regulation exercises its purview over the character
;; encoding in Charred, deviating from the ASCII standard's impositions,
;; reserving a tally of 53 slots, its numeric gamut expanding from
;; inclusive zero (0) to inclusive 52, the dioristic circumference of
;; which assigns to the minimum code the space character, succeeded by
;; the Latin minuscules, and concluding with their majuscular
;; equivalents.
;; 
;; A coarse subsumption principle assigns to the code ranges the
;; following stipulations:
;; 
;;   --------------------------------------
;;   Charred code range | Character species
;;   -------------------+------------------
;;   0                  | whitespaces
;;   ......................................
;;   1--26              | Latin minuscules
;;   ......................................
;;   27--52             | Latin majuscules
;;   --------------------------------------
;; 
;; A more detailed exposition shall apply itself to the separate Charred
;; character codes, their represented characters, and the ASCII coding
;; paregal's equiparation:
;; 
;;   -------------------------------------
;;   Charred code | Character | ASCII code
;;   -------------+-----------+-----------
;;   0            | (space)   | 32
;;   .....................................
;;   1            | a         | 97
;;   .....................................
;;   2            | b         | 98
;;   .....................................
;;   3            | c         | 99
;;   .....................................
;;   4            | d         | 100
;;   .....................................
;;   5            | e         | 101
;;   .....................................
;;   6            | f         | 102
;;   .....................................
;;   7            | g         | 103
;;   .....................................
;;   8            | h         | 104
;;   .....................................
;;   9            | i         | 105
;;   .....................................
;;   10           | j         | 106
;;   .....................................
;;   11           | k         | 107
;;   .....................................
;;   12           | l         | 108
;;   .....................................
;;   13           | m         | 109
;;   .....................................
;;   14           | n         | 110
;;   .....................................
;;   15           | o         | 111
;;   .....................................
;;   16           | p         | 112
;;   .....................................
;;   17           | q         | 113
;;   .....................................
;;   18           | r         | 114
;;   .....................................
;;   19           | s         | 115
;;   .....................................
;;   20           | t         | 116
;;   .....................................
;;   21           | u         | 117
;;   .....................................
;;   22           | v         | 118
;;   .....................................
;;   23           | w         | 119
;;   .....................................
;;   24           | x         | 120
;;   .....................................
;;   25           | y         | 121
;;   .....................................
;;   26           | z         | 122
;;   .....................................
;;   27           | A         | 65
;;   .....................................
;;   28           | B         | 66
;;   .....................................
;;   29           | C         | 67
;;   .....................................
;;   30           | D         | 68
;;   .....................................
;;   31           | E         | 69
;;   .....................................
;;   32           | F         | 70
;;   .....................................
;;   33           | G         | 71
;;   .....................................
;;   34           | H         | 72
;;   .....................................
;;   35           | I         | 73
;;   .....................................
;;   36           | J         | 74
;;   .....................................
;;   37           | K         | 75
;;   .....................................
;;   38           | L         | 76
;;   .....................................
;;   39           | M         | 77
;;   .....................................
;;   40           | N         | 78
;;   .....................................
;;   41           | O         | 79
;;   .....................................
;;   42           | P         | 80
;;   .....................................
;;   43           | Q         | 81
;;   .....................................
;;   44           | R         | 82
;;   .....................................
;;   45           | S         | 83
;;   .....................................
;;   46           | T         | 84
;;   .....................................
;;   47           | U         | 85
;;   .....................................
;;   48           | V         | 86
;;   .....................................
;;   49           | W         | 87
;;   .....................................
;;   50           | X         | 88
;;   .....................................
;;   51           | Y         | 89
;;   .....................................
;;   52           | Z         | 90
;;   -------------------------------------
;; 
;; == THE PROGRAM MEMORY: 30,000 CHARACTERS ==
;; A remote derivation of brainfuck's principles, Charred's program
;; memory tallies a linear arrangement of 30,000 cells, each such an
;; aefauld character's abode, with the repertoire that 53 choices
;; already elucidated aboon.
;; 
;; Conditioned by its content, a cell's compatibility involves the
;; stipulation of its range as an closed integral species in [0, 52],
;; the state of its incipiency the minimum at the minimum bourne of
;; zero (0). If increment above its upper extremum, the value
;; automatically relapses to this lower march; at the athwart
;; laterality, a deduction naturally endowed with the capacity to
;; violate the smaller threshold will resort in the maximum's
;; assumption.
;; 
;; A cell pointer, initially empight at the first cell at the index
;; zero (0), designates at any instant the currently active unit, siccan
;; is entalented as the sole member with a respondency to perquisitions
;; and modulations. The pointer's amenability to translations along the
;; tape homologates an alternation in its selection.
;; 
;; == CHARRED SOURCE FILES ARE IDENTIFIED BY THE EXTENSION ".chr" ==
;; Charred source file are expected to bear the extension ".chr".
;; 
;; 
;; Instructions
;; ============
;; Its foundry the brainfuck cleronomy, Charred augments the
;; stock-father's octuple instruction set to a capacity of eleven
;; participants, introducing a numeric output facility, and, most
;; conspicable in its divergence, caups the jump-based iteration
;; mechanism for a twissel of conditional execution and a line-based
;; goto supplement.
;; 
;; Any content not reserved for an operative purpose constitutes a thing
;; of neglect, and, as a consectary, may serve the wike of commentary
;; intentions.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall provide a cursory mete of gnarity anenst the
;; language's operative warklooms:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right, if
;;           | possible; otherwise, if already empight on the desinent
;;           | cell, accompasses no result.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left, if
;;           | possible; otherwise, if already empight on the first
;;           | cell, accompasses no result.
;;   ..................................................................
;;   +       | Increments the current cell value by one (1).
;;           | If the new value exceeds the upper bourne of 52, the
;;           | value wraps around to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1).
;;           | If the new value descends below the lower bourne of zero
;;           | (0), the value wraps around to the maximum of 52.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | its Charred character code in the current cell.
;;           |---------------------------------------------------------
;;           | If the received character cannot be replicated by means
;;           | of Charred's encoding, an error of the type
;;           | "InvalidCharacterError" is signaled.
;;   ..................................................................
;;   .       | Prints the character whose Charred character code
;;           | corresponds to the current cell value to the standard
;;           | output.
;;   ..................................................................
;;   '       | Prints the current cell value in its verbatim numeric
;;           | form to the standard output.
;;   ..................................................................
;;   \       | Prints a single linebreak to the standard output.
;;   ..................................................................
;;   |       | Clears the console.
;;   ..................................................................
;;   :       | If the current cell value does not equal zero (0),
;;           | executes the immediately following instruction;
;;           | otherwise skips the same.
;;   ..................................................................
;;   /       | Moves the instruction pointer to the start of the
;;           | one-based line number equal to the current cell's value.
;;           |---------------------------------------------------------
;;           | If the destination line number transgresses the
;;           | program's circumference, the program immediately
;;           | terminates.
;;           |---------------------------------------------------------
;;           | Please note that blank lines, such entailing no
;;           | instructions at all, regardless of commentary content,
;;           | are discarded from the program, and also from the line
;;           | numbering.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-26
;; 
;; Sources:
;;   [esolang2022Charred]
;;   The Esolang contributors, "Charred", July 17th, 2022
;;   URL: "https://esolangs.org/wiki/Charred"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest arguments)
     &body body)
  "Defines a derived type utilizing the ``deftype'' infrastructure in
   conjunction with the ``satisfies'' type specifier, the new species
   being norned by the TYPE-NAME, and its argument list assumed verbatim
   from the given ARGUMENTS, while its implicitly specified predicate
   function recognizes the fathomed objects by the CANDIDATE-VARIABLE
   name, evaluating the BODY forms, whose desinent result's primary
   value for a non-``NIL'' object is construed as ascertaining the
   candidate's covenableness, otherwise, if ``NIL'', the docimasy
   reckons the same ineligible.
   ---
   If the first BODY form constitutes a string, its presence is
   conflated with a documentation string's communication for the new
   derived type; as a corollary, this item is popped from the forms and
   installed at the respective ``deftype'' documentation location."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@arguments)
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Defines the derived type ``~s''" type-name))
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variation of Charred
   instructions."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output-character
    :input-character
    :output-number
    :execute-if
    :print-linebreak
    :go-to-line
    :clear-console))

;;; -------------------------------------------------------

(define-predicated-type association-list-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each key of which conforms
   to the KEY-TYPE and associated with a value of the VALUE-TYPE, both
   defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (loop
      for element of-type T in (the list candidate)
      always (typep element `(cons ,key-type ,value-type)))))

;;; -------------------------------------------------------

(deftype identifier-entry ()
  "The ``identifier-entry'' type defines an entry from the global
   identifier table, represented as a cons, the sinistral compartment of
   which carries the token in a ``character'' form, whereas the dextral
   moeity amplects the represented ``command'' item."
  '(cons character command))

;;; -------------------------------------------------------

(deftype character-code ()
  "The ``character-code'' type defines a valid character code in
   concord with the Charred's accommodated encoding system, spanning an
   expanse of 53 integral values desumed from the closed range [0, 52]."
  '(integer 0 52))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Charred program as a
   one-dimensional simple array of zero or more ``Program-Line''
   instances."
  '(simple-array Program-Line (*)))

;;; -------------------------------------------------------

(deftype cell-array ()
  "The ``cell-array'' type defines the program memory's data segment as
   a one-dimensional simple array enumerating 30000 cells, each such a
   salvatory to a scalar ``character-code'', which itself denotes an
   integer in the interval [0, 52]."
  '(simple-array character-code (30000)))

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines an index into the program memory as
   an integral object impounded in the non-negative range [0, 29999]."
  '(integer 0 29999))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines a non-negative integer
   number greater than or equal to zero (0), but bourneless along the
   upper extremum, by which diorism its occupancy conflates with the
   interval [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   same embraces, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of character command) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '((#\> . :move-right)
    (#\< . :move-left)
    (#\+ . :increment)
    (#\- . :decrement)
    (#\. . :output-character)
    (#\, . :input-character)
    (#\' . :output-number)
    (#\: . :execute-if)
    (#\\ . :print-linebreak)
    (#\/ . :go-to-line)
    (#\| . :clear-console))
  "Maps the command tokens to representative ``command'' objects.")

;;; -------------------------------------------------------

(defun get-identifier-entry (token)
  "Returns the entry in the entry in the +COMMANDS+ table for the TOKEN,
   or ``NIL'' if the same does not represents a recognized identifier
   therefrom."
  (declare (type character token))
  (the (or null identifier-entry)
    (assoc token +IDENTIFIERS+ :test #'char=)))

;;; -------------------------------------------------------

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a command identifier,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (get-identifier-entry candidate)))))

;;; -------------------------------------------------------

(defun look-up-command (token)
  "Returns the command associated with the TOKEN, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type character token))
  (let ((entry-for-token (get-identifier-entry token)))
    (declare (type (or null identifier-entry) entry-for-token))
    (the command
      (if entry-for-token
        (cdr entry-for-token)
        (error "The token \"~c\" does not identify a command."
          token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of Charred character repertoire.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 53) +ALPHABET+))

;;; -------------------------------------------------------

(defparameter +ALPHABET+
  " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Enumerates the alphabet deployed by the Charred programming language,
   each of its 53 members being located at the zero-based index
   corresponding to their bespoke character code.")

;;; -------------------------------------------------------

(defun decode-character (code)
  "Returns the character corresponding to the Charred character CODE, or
   signals an error of an unspecified type upon its violation of the
   recognized repertoire gamut."
  (declare (type character-code code))
  (the character
    (schar +ALPHABET+ code)))

;;; -------------------------------------------------------

(defun encode-character (character)
  "Returns the Charred character code for the CHARACTER, or signals an
   error of an unspecified type upon its absence from the underlying
   alphabet."
  (declare (type character character))
  (the character-code
    (or (position character +ALPHABET+ :test #'char=)
        (error "The character \"~c\" cannot be represented in ~
                Charred's alphabet." character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program line.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program-Line
  (:constructor make-program-line
    (command-list
     &aux (commands
            (coerce command-list
              '(simple-array command (*)))))))
  "The ``Program-Line'' class encapsulates a single line composed of
   zero or more commands."
  (commands (error "Missing commands.")
            :type      (simple-array command (*))
            :read-only T))

;;; -------------------------------------------------------

(defun empty-program-line-p (line)
  "Determines whether the program LINE is destitute of any commands,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Program-Line line))
  (the boolean
    (not (null
      (zerop (length (program-line-commands line)))))))

;;; -------------------------------------------------------

(defun valid-program-line-index-p (line index)
  "Determines whether the INDEX constitutes a valid position into the
   program LINE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Program-Line line))
  (declare (type fixnum       index))
  (the boolean
    (not (null
      (array-in-bounds-p
        (program-line-commands line)
        index)))))

;;; -------------------------------------------------------

(defun get-command-at (line index)
  "Returns the command located at the zero-based INDEX on the program
   LINE, or signals an error of an unspecified type upon its bournes'
   transgression."
  (declare (type Program-Line line))
  (declare (type fixnum       index))
  (the command
    (aref
      (program-line-commands line)
      index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-next-command-position (source start)
  "Proceeding from the START position into the SOURCE, searches for the
   next command, returning on confirmation its position into the same,
   otherwise responding with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'command-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun parse-line (line)
  "Extracts from the LINE the incorporated commands and returns a
   ``Line'' representation thereof."
  (declare (type string line))
  (the Program-Line
    (loop
      for position
        of-type fixnum
        =       (find-next-command-position line 0)
        then    (find-next-command-position line (1+ position))
      while   (< position (length line))
      collect (look-up-command (char line position))
        into commands
      finally
        (return
          (make-program-line commands)))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Parses the piece of Charred source CODE and returns a vector of its
   lines."
  (declare (type string code))
  (with-input-from-string (input-stream code)
    (declare (type string-stream input-stream))
    (the program
      (coerce
        (loop
          for input-line
            of-type (or null string)
            =       (read-line input-stream NIL)
          while input-line
          for program-line
            of-type Program-Line
            =       (parse-line input-line)
          unless (empty-program-line-p program-line)
            collect program-line)
        '(simple-array Program-Line (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command iterator.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command-Iterator ()
  ((line
    :initform      NIL
    :type          (or null Program-Line)
    :documentation "The program line whose commands shall be accessed.")
   (index
    :initform      0
    :type          fixnum
    :documentation "The index into the LINE, designated the currently
                    selected command."))
  (:documentation
    "The ``Command-Iterator'' class furnishes a conductible mechanism
     for the traversal of a program line's commands."))

;;; -------------------------------------------------------

(defun make-command-iterator ()
  "Creates and returns a fresh ``Command-Iterator'' instance."
  (the Command-Iterator
    (make-instance 'Command-Iterator)))

;;; -------------------------------------------------------

(defun set-line (iterator new-line)
  "Stores the NEW-LINE in the command ITERATOR and returns the modified
   ITERATOR."
  (declare (type Command-Iterator       iterator))
  (declare (type (or null Program-Line) new-line))
  (setf (slot-value iterator 'line)  new-line)
  (setf (slot-value iterator 'index) 0)
  (the Command-Iterator iterator))

;;; -------------------------------------------------------

(defun line-exhausted-p (iterator)
  "Determines whether the command ITERATOR is exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Command-Iterator iterator))
  (with-slots (line index) iterator
    (declare (type (or null Program-Line) line))
    (declare (type fixnum                 index))
    (the boolean
      (or (null line)
          (not (valid-program-line-index-p line index))))))

;;; -------------------------------------------------------

(defun get-iterated-command (iterator)
  "Returns the command ITERATOR's currently selected command, or ``NIL''
   upon its bournes' transcendence."
  (declare (type Command-Iterator iterator))
  (the (or null command)
    (unless (line-exhausted-p iterator)
      (with-slots (line index) iterator
        (declare (type (or null Program-Line) line))
        (declare (type fixnum                 index))
        (get-command-at line index)))))

;;; -------------------------------------------------------

(defun advance-to-next-command (iterator)
  "Advances to the next command in the command ITERATOR, if possible,
   and returns no value."
  (declare (type Command-Iterator iterator))
  (unless (line-exhausted-p iterator)
    (with-slots (index) iterator
      (declare (type fixnum index))
      (incf index)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((iterator Command-Iterator) (stream T))
  (declare (type Command-Iterator iterator))
  (declare (type destination      stream))
  (format stream "(Command-Iterator index=~d command=~s)"
    (slot-value iterator 'index)
    (get-iterated-command iterator)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of execution context.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Execution-Context ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :type          program
    :documentation "The Charred program whose commands shall be
                    traversed.")
   (line-index
    :initform      0
    :type          fixnum
    :documentation "The index of the current selected line among the
                    PROGRAM lines.")
   (current-line
    :initform      (make-command-iterator)
    :type          Command-Iterator
    :documentation "An iterator over the commands comprehended in the
                    PROGRAM's line at the LINE-INDEX."))
  (:documentation
    "The ``Execution-Context'' class encapsulates an executable Charred
     program in a pursuit to accommodate eath access to its commands
     across both the vertical dimension of their lines and the
     horizontal spatiality of each such expanse's operations."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((context Execution-Context) &key)
  (declare (type Execution-Context context))
  (with-slots (program line-index current-line) context
    (declare (type program          program))
    (declare (type fixnum           line-index))
    (declare (type Command-Iterator current-line))
    (when (array-in-bounds-p program line-index)
      (set-line current-line
        (aref program line-index))))
  (values))

;;; -------------------------------------------------------

(defun make-execution-context (program)
  "Creates and returns a fresh ``Execution-Context'' instance."
  (declare (type program program))
  (the Execution-Context
    (make-instance 'Execution-Context :program program)))

;;; -------------------------------------------------------

(defun program-exhausted-p (context)
  "Determines whether the Charred program under the execution CONTEXT's
   castaldy is exhausted, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Execution-Context context))
  (with-slots (program line-index current-line) context
    (declare (type program          program))
    (declare (type fixnum           line-index))
    (declare (type Command-Iterator current-line))
    (the boolean
      (not (array-in-bounds-p program line-index)))))

;;; -------------------------------------------------------

(defun get-current-command (context)
  "Returns the currently selected command in the execution CONTEXT, or
   ``NIL'' upon its program's exhaustion."
  (declare (type Execution-Context context))
  (the (or null command)
    (get-iterated-command
      (slot-value context 'current-line))))

;;; -------------------------------------------------------

(defun advance-program (context)
  "Advances to the next command in the execution CONTEXT's program, if
   possible, and returns no value."
  (declare (type Execution-Context context))
  (with-slots (program line-index current-line) context
    (declare (type program          program))
    (declare (ignorable             program))
    (declare (type fixnum           line-index))
    (declare (type Command-Iterator current-line))
    (advance-to-next-command current-line)
    (when (line-exhausted-p current-line)
      (incf line-index)
      (set-line current-line
        (when (array-in-bounds-p program line-index)
          (aref program line-index)))))
  (values))

;;; -------------------------------------------------------

(defun move-to-line-at (context new-line-index)
  "Relocates the execution CONTEXT's line cursor to the zero-based
   NEW-LINE-INDEX and returns no value."
  (declare (type Execution-Context context))
  (declare (type fixnum            new-line-index))
  (with-slots (program line-index current-line) context
    (declare (type program          program))
    (declare (type fixnum           line-index))
    (declare (type Command-Iterator current-line))
    (setf line-index new-line-index)
    (set-line current-line
      (when (array-in-bounds-p program line-index)
        (aref program line-index))))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((context Execution-Context) (stream T))
  (declare (type Execution-Context context))
  (declare (type destination       stream))
  (format stream "(Execution-Program ~
                   line-index=~d ~
                   current-line=~s ~
                   current-command=~s)"
    (slot-value context  'line-index)
    (slot-value context  'current-line)
    (get-current-command context)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of sequence operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-simple-array (element-type length initial-element)
  "Creates and returns a one-dimensional simple array comprehending the
   LENGTH tally of elements, each such conforming to the ELEMENT-TYPE,
   and initialized to the INITIAL-ELEMENT."
  (declare (type symbol element-type))
  (declare (type fixnum length))
  (declare (type T      initial-element))
  (the (simple-array * (*))
    (make-array length
      :element-type    element-type
      :initial-element initial-element
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-simple-array 'character-code 30000 0)
    :type          cell-array
    :documentation "The program memory as a composition of 3000 accolent
                    cells, each storing a character code from the
                    integral range [0, 52].")
   (pointer
    :initform      0
    :type          cell-index
    :documentation "The cell pointer whose value designates the
                    currently active member among the CELLS."))
  (:documentation
    "The ``Memory'' class furnishes a representation of the Charred
     program memory as a one-dimensional array of 30000 adjacent cells,
     each storing an integral value desumed from the closed interval
     [0, 52], with the currently active member among these designated at
     any instant by the mobile cell pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a fresh ``Memory'' instance."
  (the Memory (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the character-code
    (aref
      (slot-value memory 'cells)
      (slot-value memory 'pointer))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   wrapping the input around in order to respect the valid marches of
   [0, 52], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (with-slots (cells pointer) memory
    (declare (type cell-array cells))
    (declare (type cell-index pointer))
    (setf (aref cells pointer)
      (mod new-value 53)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left, if
   feasible, and returns no value."
  (declare (type Memory memory))
  (with-slots (pointer) memory
    (declare (type cell-index pointer))
    (when (plusp pointer)
      (decf pointer)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right, if
   feasible, and returns no value."
  (declare (type Memory memory))
  (with-slots (cells pointer) memory
    (declare (type cell-array cells))
    (declare (type cell-index pointer))
    (when (array-in-bounds-p cells (1+ pointer))
      (incf pointer)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type non-negative-integer +DEFAULT-CONSOLE-HEIGHT+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CONSOLE-HEIGHT+ 34
  "The default viewport height imputed for a contingent console inside
   of which the Charred program may be executed.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((context
    :initarg       :context
    :initform      (error "Missing execution context for interpreter.")
    :type          Execution-Context
    :documentation "An execution context which encapsulates the Charred
                    program intended for its evaluation.")
   (memory
    :initform      (make-memory)
    :type          Memory
    :documentation "The program memory, composed of a finite tape of
                    character codes and a mobile cell pointer.")
   (console-height
    :initarg       :console-height
    :initform      +DEFAULT-CONSOLE-HEIGHT+
    :type          non-negative-integer
    :documentation "The viewport height of the console, either such
                    being a physical veridicality, or a virtual
                    concoction."))
  (:documentation
    "The ``Interpreter'' class is encumbered with that dever which
     involves the execution of a parsed Charred program."))

;;; -------------------------------------------------------

(defun make-interpreter (program
                         &key (console-height +DEFAULT-CONSOLE-HEIGHT+))
  "Creates and returns a fresh ``Interpreter'' dedicated to the Charred
   PROGRAM's evaluation."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter
      :context        (make-execution-context program)
      :console-height console-height)))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Evaluates the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defun advance-ip (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (context) interpreter
    (declare (type Execution-Context context))
    (advance-program context))
  (values))

;;; -------------------------------------------------------

(defun move-ip-to (interpreter new-line-index)
  "Relocates the INTERPRETER's instruction pointer (IP) to the start of
   the line designated by the one-based NEW-LINE-INDEX in its program
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (context) interpreter
    (declare (type Execution-Context context))
    (move-to-line-at context
      (1- new-line-index)))
  (values))

;;; -------------------------------------------------------

(defmacro define-command-processor (command (interpreter-variable)
                                    &body body)
  "Defines an implementation of the generic function
   ``process-command'', utilizing the INTERPRETER-VARIABLE as the first
   argument name, an automatically generated designation for the second,
   on which, by equality with the COMMAND, the method is dispatched,
   executes the BODY forms, and returns no value."
  (let ((command-variable (gensym)))
    (declare (type symbol command-variable))
    `(defmethod process-command
         ((,interpreter-variable Interpreter)
          (,command-variable     (eql ,command)))
       (declare (type Interpreter ,interpreter-variable))
       (declare (ignorable        ,interpreter-variable))
       (declare (type command     ,command-variable))
       (declare (ignore           ,command-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-command-processor :move-right (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (move-cell-pointer-right memory))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :move-left (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (move-cell-pointer-left memory))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :increment (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (incf (current-cell-value memory)))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :decrement (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (decf (current-cell-value memory)))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :output-character (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (write-char
      (decode-character
        (current-cell-value memory))))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :input-character (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (format T "~&>> ")
    (finish-output)
    (setf (current-cell-value memory)
      (encode-character
        (read-char)))
    (clear-input))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :output-number (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (format T " ~d "
      (current-cell-value memory)))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :print-linebreak (interpreter)
  (terpri)
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :execute-if (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (when (zerop (current-cell-value memory))
      (advance-ip interpreter)))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(define-command-processor :go-to-line (interpreter)
  (with-slots (memory) interpreter
    (declare (type Memory memory))
    (move-ip-to interpreter
      (current-cell-value memory))))

;;; -------------------------------------------------------

(define-command-processor :clear-console (interpreter)
  (with-slots (console-height) interpreter
    (declare (type non-negative-integer console-height))
    (format T "~v%" console-height))
  (advance-ip interpreter))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Charred program governed by the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (context) interpreter
    (declare (type Execution-Context context))
    (loop until (program-exhausted-p context) do
      (process-command interpreter
        (get-current-command context))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Charred (code)
  "Interprets the piece of Charred source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file loaders.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric read-file-content (source)
  (:documentation
    "Returns a string comprehending the SOURCE file or stream content.")
  
  (:method ((source pathname))
    "Loads the SOURCE file designated by the pathname and returns its
     content as a string."
    (declare (type pathname source))
    (with-open-file (input-stream source
                     :direction         :input
                     :element-type      'character
                     :if-does-not-exist :error)
      (declare (type file-stream input-stream))
      (the simple-string
        (let ((buffer (make-string (file-length input-stream))))
          (declare (type simple-string buffer))
          (read-sequence buffer input-stream)
          (the simple-string buffer)))))
  
  (:method ((source string))
    "Loads the SOURCE addressed via a string designating its file path
     and returns its content as a string."
    (declare (type string source))
    (the simple-string
      (read-file-content
        (parse-namestring source))))
  
  (:method ((source stream))
    "Loads the SOURCE accessed by the STREAM and returns its content as
     a string."
    (declare (type stream source))
    (the string
      (with-output-to-string (file-content)
        (declare (type string-stream file-content))
        (let ((buffer (make-string 100)))
          (declare (type (simple-string 100) buffer))
          (loop
            for position
              of-type (integer 0 100)
              =       (read-sequence buffer source)
            when (plusp position) do
              (format file-content "~a"
                (subseq buffer 0 position))
            when (< position (length buffer)) do
              (return)))))))

;;; -------------------------------------------------------

(defun load-Charred-file (source)
  "Loads the Charred program from the SOURCE file or stream, interprets
   the same, and returns no value."
  (declare (type (or pathname stream string) source))
  (interpret-Charred
    (read-file-content source))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World", the Charred character codes of which
;; are enumerated as:
;;   34 5 12 12 15 0 49 15 18 12 4
;;   H  e l  l  o    W  o  r  l  d
(interpret-Charred
  "++++++++++++++++++++++++++++++++++.
   -----------------------------.
   +++++++..
   +++.
   ---------------.
   +++++++++++++++++++++++++++++++++++++++++++++++++.
   ----------------------------------.
   +++.
   ------.
   --------.")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Charred ",.")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Charred
  "++
   >
   ,.
   </")

;;; -------------------------------------------------------

;; Counter which prints ten (10) times the symbol "X".
(interpret-Charred
  "
  [Line 1] Set output cell in memory(0) to the symbol X
  ++++++++++++++++++++++++++++++++++++++++++++++++++
  [Line 2] Set the counter in memory(1) to 10
  >++++++++++
  [Line 3] Set the terminating goto cell in memory(2) to 8
  >++++++++
  [Line 4] Set the looping goto cell in memory(3) to 5
  >+++++
  [Line 5] Change to counter cell in memory(1)
  <<
  [Line 6] Output X and decrement the counter
  <.>-
  [Line 7] Either jump to [Line 5] via memory(3) or the end via memory(2)
  :>>/
  ")

;;; -------------------------------------------------------

;; This program perpetually prints the entire Charred alphabet in the
;; correct arrangement of its symbols.
(interpret-Charred
  "++
   >.+
   </")
