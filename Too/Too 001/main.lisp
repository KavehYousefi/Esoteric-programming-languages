;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Too", invented by the Esolang user "ChuckEsoteric08" and
;; presented on April 9th, 2023, the haecceity of which is expressed in
;; a basic consanguinity with Urban Mueller's "brainfuck", operating on
;; a tape of bilaterally infinite cells, the sole responsive instance of
;; which answers to the mobile cell pointer, but diverging in its usage
;; of 2-bit values, or dibits, to delineate each cell's capacity, as
;; well as ostending a furnishment of kenspeckle control flow
;; mechanisms, and actuating the input as a singular parasceuastic
;; occasion preceding the perpetual program loop.
;; 
;; 
;; Concept
;; =======
;; The Too programming language appropriate a moeity of brainfuck's
;; competences, namelty those regarding the memory cell pointer helming
;; and the basic arithmetics, as well as a preponderance among the
;; architectural concepts, but furnishes its dioristic solutions to
;; conditional execution, input reception, and a cell capacity reduced
;; to a twain of bits, or a dibit.
;; 
;; == EACH PROGRAM COMMENCES WITH INPUT ==
;; A program's inchoation steadily conflates with a request for input by
;; the user via the standard input conduit. If the response's obtention
;; signifies a value transformable into the integral dibit range [0, 3],
;; its immediate induction into the first memory cell instigates the
;; program, otherwise no input accompanies its procession.
;; 
;; == A PROGRAM OPERATES IN A PERPETUAL CYCLE ==
;; Too programs wone in a cycle bound to perpetuality, constinuing one
;; patration of the instruction processing by an iterum sojourn.
;; 
;; == THE TAPE: AN INFINITE EXPANSE OF DIBITS ==
;; A counterdistinguished proprium of Too, concomitant to its
;; appropriation of brainfuck's bilaterally infinite tape length and
;; cell pointer, the derivative eschews the byte content for a capacity
;; restricted two to bits, or one dibit.
;; 
;; This dibit species occupies the integral range of [0, 3]. Upon any of
;; its two bournes' transgression, the cell state wraps around to the
;; athwart extremum, returning from the maximum of 3 to 0, and from 0
;; to 3.
;; 
;; 
;; Instructions
;; ============
;; Too's instruction set amplects a composition of a sextuple
;; cardinality, a quadruple share of which assumes from its brainfuck
;; entheus, yet appropriated to the personal diorisms, to which a
;; twissel of conditional execution solutions are supplemented.
;; 
;; The language, commorant in a perpetual iteration, offers an aefauld
;; input encheson as a program's parasceve, ere its actual application,
;; but is deprieved of any output facilities.
;; 
;; == OVERVIEW ==
;; The operations commorant in Too shall be a cursory apercu's material:
;; 
;;   ------------------------------------------------------------------
;;   Command    | Effect
;;   -----------+------------------------------------------------------
;;   >          | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   <          | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   +          | Increments the current cell value by one. If the new
;;              | cell value exceeds the upper bourne of three (3), the
;;              | cell state wraps around to the minimum of zero (0).
;;   ..................................................................
;;   -          | Decrements the current cell value by one. If the new
;;              | cell value descends below the lower extremum of zero
;;              | (0), the cell state wraps around to the maximum of
;;              | three (3).
;;   ..................................................................
;;   o commands | If the current cell value equals three (3), executes
;;     ******** | the five subsequent {commands}. Otherwise skips
;;              | these.
;;              |------------------------------------------------------
;;              | {commands} must be a sequence of zero to five
;;              | instructions.
;;   ..................................................................
;;   | command  | Repeats the {command} until either the current cell
;;     *******  | value equals zero (0), or the instruction pointer
;;              | (IP) transcends the program's bournes.
;;              |------------------------------------------------------
;;              | {command} must be zero or one instruction.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This program has been implemented in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-03
;; 
;; Sources:
;;   [esolang2023Too]
;;   The Esolang contributors, "Too", April 9th, 2023
;;   URL: "https://esolangs.org/wiki/Too"
;;   
;;   [farlex2023dibit]
;;   Farlex, Inc.,
;;     "Dibit | Article about dibit by The Free Dictionary",
;;     October 3rd, 2023"
;;   URL: "https://encyclopedia2.thefreedictionary.com/dibit"
;;   Notes:
;;     - Defines the term "dibit" as a sequence of two bits.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype dibit ()
  "The ``dibit'' type defines a sequence of two consecutive bits,
   occupying the closed integral range [0, 3]."
  '(unsigned-byte 2))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, the keys of which conform to the KEY-TYPE and associate
   with a value of the VALUE-TYPE, both defaulting to the generic ``*''
   sentinel."
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

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of dibit-valued
   cells, realized as a hash table whose signed integer keys maintain
   the cell indices and associate with the ``dibit'' cell contents."
  '(hash-table-of integer dibit))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized variation on the
   Too language's operation contingency, extended by the no-operation
   sentinel ``:nop'', serving to communicate a surrogate instruction in
   cases where exhaustion redes an dioristic entity.
   ---
   The following affiliations govern the mapping betwixt ``command''
   members and Too identifier tokens:
     ---------------------------------
     ``command-type'' | Too identifier
     -----------------+---------------
     :move-right      | >
     :move-left       | <
     :increment       | +
     :decrement       | -
     :execute-if-3    | o
     :execute-until-0 | |
     ---------------------------------"
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :execute-if-3
    :execute-until-0
    :nop))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic ``*'' sentinel."
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

(deftype command-list ()
  "The ``command-list'' type defines a list of zero or more Too
   ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype too-program ()
  "The ``too-program'' type defines an executable program in the Too
   programming language as a list composed of zero or more ``Command''
   instances."
  '(list-of Command))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Tape".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-table
    :documentation "A sparse vector of dibit-valued cells, each such
                    relayed to a entry in the hash table, the key of
                    which defines the cell index, associated with the
                    cell value.")
   (pointer
    :initform      0
    :type          integer
    :accessor      tape-pointer
    :documentation "Selects the currently active cell by storing its
                    index, that is, its key in the CELLS table.")
   (minimum-cell-index
    :initform      0
    :type          integer
    :documentation "The inclusive smallest cell index sojourned by the
                    POINTER, its castaldy an adminiculum for the tape's
                    ordered printing.")
   (maximum-cell-index
    :initform      0
    :type          integer
    :documentation "The inclusive largest cell index sojourned by the
                    POINTER, its castaldy an adminiculum for the tape's
                    ordered printing."))
  (:documentation
    "The ``Tape'' class serves in the simulation of the Too program
     memory, conceived as a bilaterally infinite tape of cells,
     everichon among these a salvatory to two bit, or a dibit, which
     comprehends the range [0, 3], and operated upon by a cell pointer
     responsible for the current unit's designation."))

;;; -------------------------------------------------------

(defun make-tape ()
  "Creates and returns an empty ``Tape''."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun tape-current-cell (tape)
  "Returns the dibit stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the dibit
    (with-slots (cells pointer) tape
      (declare (type cell-table cells))
      (declare (type integer    pointer))
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf tape-current-cell) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingenly preceded
   by a wrapping of the same into the valid dibit range [0, 3], and
   returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-slots (cells pointer) tape
    (declare (type cell-table cells))
    (declare (type integer    pointer))
    (setf (gethash pointer cells 0)
          (mod new-value 4)))
  (values))

;;; -------------------------------------------------------

(defun tape-move-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (with-slots (pointer maximum-cell-index) tape
    (declare (type integer pointer))
    (declare (type integer maximum-cell-index))
    (incf pointer)
    (setf maximum-cell-index
          (max maximum-cell-index pointer)))
  (values))

;;; -------------------------------------------------------

(defun tape-move-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (with-slots (pointer minimum-cell-index) tape
    (declare (type integer pointer))
    (declare (type integer minimum-cell-index))
    (incf pointer)
    (setf minimum-cell-index
          (min minimum-cell-index pointer)))
  (values))

;;; -------------------------------------------------------

(defun tape-print (tape)
  "Outputs information concerning the TAPE to the standard output and
   returns no value."
  (with-slots (cells minimum-cell-index maximum-cell-index) tape
    (declare (type cell-table cells))
    (declare (type integer    minimum-cell-index))
    (declare (type integer    maximum-cell-index))
    (loop
      for current-cell-index
        of-type integer
        from    minimum-cell-index
        to      maximum-cell-index
      do
        (format T "[~d]"
          (gethash current-cell-index cells 0))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type &optional (body NIL))))
  "The ``Command'' class serves in the encapsulation of a Too
   operation."
  (type (error "Missing command type.") :type command-type)
  (body NIL                             :type command-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (string fixnum (integer 0 *))
                          (values command-list fixnum))
                parse-command-body))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Probes the CANDIDATE anenst its membership among the whitespace
   characters, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces, and returns the position of
   the first non-whitespace character in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION in the SOURCE, or ``NIL'' if
   the location violates the SOURCE's bournes."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun parse-command (source start)
  "Proceeding from the START position into the SOURCE, potentially
   preceded by a sequence of ignored whitespaces, parses a single Too
   command, and returns two values:
     (1) The detected Too command as a ``Command'' object.
     (2) The position in the SOURCE immediately succeeding the parsed
         command."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (the (values Command fixnum)
      (case (get-character-at source position)
        ((NIL)
          (values
            (make-command :nop)
            position))
        (#\>
          (values
            (make-command :move-right)
            (1+ position)))
        (#\<
          (values
            (make-command :move-left)
            (1+ position)))
        (#\+
          (values
            (make-command :increment)
            (1+ position)))
        (#\-
          (values
            (make-command :decrement)
            (1+ position)))
        (#\o
          (incf position)
          (multiple-value-bind (body new-position)
              (parse-command-body source position 5)
            (declare (type command-list body))
            (declare (type fixnum       new-position))
            (values
              (make-command :execute-if-3 body)
              new-position)))
        (#\|
          (incf position)
          (multiple-value-bind (body new-position)
              (parse-command-body source position 1)
            (declare (type command-list body))
            (declare (type fixnum       new-position))
            (values
              (make-command :execute-until-0 body)
              new-position)))
        (otherwise
          (error "Invalid command token \"~c\" at position ~d."
            (get-character-at source position)
            position))))))

;;; -------------------------------------------------------

(defun parse-command-body (source start number-of-commands)
  "Proceeding from the START position into the SOURCE, potentially
   preceded by a sequence of whitespaces, reads a tally of
   NUMBER-OF-COMMANDS Too commands, and returns two values:
     (1) A list comprehending the detected and parsed Too ``Command''
         objects.
     (2) The position in the SOURCE immediately succeeding the desinent
         parsed command."
  (declare (type string        source))
  (declare (type fixnum        start))
  (declare (type (integer 0 *) number-of-commands))
  (let ((position start))
    (declare (type fixnum position))
    (flet ((collect-command (command end-position)
            "Sets the POSITION to the END-POSITION and returns the
             COMMAND."
            (declare (type Command command))
            (declare (type fixnum  end-position))
            (the Command
              (prog1 command
                (setf position end-position)))))
      (the (values command-list fixnum)
        (loop
          repeat  number-of-commands
          collect (multiple-value-call #'collect-command
                    (parse-command source position))
          into    body
          finally
            (return
              (values body position)))))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of Too SOURCE code and a ``too-program''
   representation thereof."
  (declare (type string source))
  (let ((position (skip-whitespaces source 0)))
    (declare (type fixnum position))
    (the too-program
      (loop
        while (< position (length source))
        collect
          (multiple-value-bind (next-command new-position)
              (parse-command source position)
            (declare (type Command next-command))
            (declare (type fixnum  new-position))
            (setf position
              (skip-whitespaces source new-position))
            next-command)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Loop-Exhausted-Condition (condition)
  ()
  (:documentation
    "The ``Loop-Exhausted-Condition'' condition type serves to signal a
     request for a loop's immediate cessation founded upon the
     instruction pointer's (IP) transgression of the program bournes."))

;;; -------------------------------------------------------

(defun signal-loop-exhausted-condition ()
  "Signals a condition of the type ``Loop-Exhausted-Condition''."
  (signal 'Loop-Exhausted-Condition))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (real 0 *) +DEFAULT-CYCLE-DELAY+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CYCLE-DELAY+ 1
  "The default number of seconds to wait atwixt two consecutive program
   cyles.")

;;; -------------------------------------------------------

(defun print-program-state (tape)
  "Prints information regarding the program state, in particular the
   TAPE, to the standard output and returns no value."
  (declare (type Tape tape))
  (format T "~&Tape content: ")
  (tape-print tape)
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defun query-for-input ()
  "Queries the user for an integer number, returning for a valid
   response the integral value wrapped into the dibit range of [0, 3],
   otherwise, upon a unrecognized answer, returning the ``NIL'' value."
  (format T "~&>> Please input the current cell value (0--3): ")
  (finish-output)
  (let ((input (parse-integer (read-line) :junk-allowed T)))
    (declare (type (or null integer) input))
    (the (or null dibit)
      (when input
        (mod input 4)))))

;;; -------------------------------------------------------

(defun initialize-tape (tape)
  "Queries the user for an optional dibit to store in the TAPE's first
   cell and returns no value."
  (declare (type Tape tape))
  (let ((input (query-for-input)))
    (declare (type (or null dibit) input))
    (when input
      (setf (tape-current-cell tape) input)))
  (values))

;;; -------------------------------------------------------

(defun process-command (command tape)
  "Processes the COMMAND utilizing the TAPE for access to the program
   memory and returns no value."
  (declare (type Command command))
  (declare (type Tape    tape))
  (case (command-type command)
    (:move-right
      (tape-move-right tape))
    (:move-left
      (tape-move-left tape))
    (:increment
      (incf (tape-current-cell tape)))
    (:decrement
      (decf (tape-current-cell tape)))
    (:execute-if-3
      (when (= (tape-current-cell tape) 3)
        (dolist (body-command (command-body command))
          (declare (type Command body-command))
          (process-command body-command tape))))
    (:execute-until-0
      (handler-case
        (loop until (zerop (tape-current-cell tape)) do
          (dolist (body-command (command-body command))
            (declare (type Command body-command))
            (process-command body-command tape)))
        (Loop-Exhausted-Condition ()
          NIL)))
    (:nop
      (signal-loop-exhausted-condition))
    (otherwise
      (error "Invalid command: ~s." command)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Too-program (program)
  "Interprets the piece Too PROGRAM and returns no value.
   ---
   The Too language's lacunae in an output facility begets its impotence
   in regards to communications towards the user. In an attempt to
   edulcorate this circumstance, each program cycle's ultimity is
   succeeded by the current program state's output to the respective
   standard conduit and a fixed delay that admits the missive's
   scrutiny."
  (declare (type too-program program))
  (let ((tape (make-tape)))
    (declare (type Tape tape))
    (initialize-tape tape)
    (loop do
      (loop for command of-type Command in program do
        (process-command command tape))
      (print-program-state tape)
      (sleep +DEFAULT-CYCLE-DELAY+)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Too (code)
  "Interprets the piece of Too source CODE and returns no value.
   ---
   The Too language's lacunae in an output facility begets its impotence
   in regards to communications towards the user. In an attempt to
   edulcorate this circumstance, each program cycle's ultimity is
   succeeded by the current program state's output to the respective
   standard conduit and a fixed delay that admits the missive's
   scrutiny."
  (declare (type string code))
  (interpret-Too-program
    (parse-program code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Query the user for an input and upon each cycle increment the same,
;; contingently wrapping around initial cell state.
(interpret-Too "+")

;;; -------------------------------------------------------

;; If the user input equals three (3), advances to the dextral cell,
;; sets it to two (2), returns to the original cell, and increments it
;; by one to zero (0), such as to disable this behavior in the next
;; iteration. For any other input, abstains from further actions.
(interpret-Too "o>++<+")

;;; -------------------------------------------------------

;; Set the user input to zero (0) by decrementing conditionally during
;; each cycle.
(interpret-Too "|-")
