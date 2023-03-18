;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "InfiniTick", introduced by the Esolang user "Dpleshkov" in
;; the year 2017, designed as a more potent version of the same author's
;; language "Tick" --- the latter being a restricted derivation of Urban
;; Mueller's "brainfuck" ---, however, operating in an implicit infinite
;; loop, with the ability to skip commands based upon the current cell
;; value.
;; 
;; 
;; Concept
;; =======
;; An InfiniTick program executes inside of an infinite loop, while
;; operating on an infinite tape of unsigned-byte-valued cells.
;; 
;; Facilities exist to increment and decrement the currently active
;; cell, as well as such to translate the cell selection cursor --- a
;; warkloom used to designate the currently receptive unit --- in
;; graduation.
;; 
;; Destitute of an input conduit, the sole expression appertaining to
;; the language is represented by the output of the ASCII characters
;; corresponding to the numeric cell value.
;; 
;; A vestigial ilk of control flow resides in the conditional skipping
;; of a command founded upon the preceding indagation of a dedicated
;; operation applied to the active cell of preterite validity.
;; 
;; If pursuing to terminate a program, the error instigation command
;; must be invoked, as a concomitant escaping the loop.
;; 
;; 
;; Architecture
;; ============
;; InfiniTick programs operate on a tape of cells, extending its
;; components bilaterally into infinity, with each cell amplecting a
;; single 8-bit unsigned byte datum, corresponding to the integer range
;; [0, 255]. If faced with an attempt to increment its value above the
;; octet interval's maximum of 255, the cell wraps around to the lowest
;; state of zero (0). A descent alow the minimum of zero (0) incites an
;; automatic alteration to the maximum 255.
;; 
;; At the program's inchoation empight on an initial unit, a special
;; cursor, known as the "selector", refers at any instant to the
;; currently active cell, the sole entity amenable to indagations and
;; manipulations. Instructions exist to gradually shift the cursor along
;; the tape.
;; 
;; 
;; Data Types
;; ==========
;; InfiniTick employs two major categories of data: octets and
;; characters.
;; 
;; == BYTES ==
;; Specifying each cell's content, unsigned bytes of eight bit size,
;; also known as octets and spanning the integer range [0, 255], enjoy a
;; paravaunt significance.
;; 
;; == CHARACTERS ==
;; Engaging in a puisne role, ASCII characters form an expression of
;; the unsigned byte basis commorant in the cells, deployed solely for
;; output purposes.
;; 
;; 
;; Syntax
;; ======
;; The sole significant tokens in a piece of InfiniTick source code are
;; exhausted by the octuple single-character command identifiers, with
;; any other content's homologation adhibiting nothing more than the
;; puissance of apostilles.
;; 
;; 
;; Instructions
;; ============
;; Appropriated from its inspiration, InfiniTick assumes the entirety of
;; Tick's and a subset of brainfuck's abilities, enhancing the former's
;; by a twifold tally, and superseding the latter's three facilities by
;; new operations.
;; 
;; == OVERVIEW ==
;; An apercu of cursory penetration shall administer a slight ilk of
;; education about the language's appliances:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell selector one step to the right.
;;   ..................................................................
;;   <       | Moves the cell selector one step to the left.
;;   ..................................................................
;;   +       | Increments the current memory cell's value by one.
;;           | If surpassing the maximum bourne of 255, the cell value
;;           | relapses to the minimum of zero (0).
;;   ..................................................................
;;   -       | Decrements the current memory cell's value by one.
;;           | If descending below the minimum bourne of zero (0), the
;;           | cell value resorts to the maximum of 255.
;;   ..................................................................
;;   *       | Prints to the standard output the character
;;           | corresponding to the current memory cell's value when
;;           | construed as an ASCII code.
;;   ..................................................................
;;   /       | If the current cell value equals zero (0), skips the
;;           | next command; otherwise executes the same.
;;   ..................................................................
;;   \       | If the current cell value does not equal zero (0), skips
;;           | the next command; otherwise executes the same.
;;   ..................................................................
;;   &       | Raises an error, thus terminating the implicitly
;;           | operating infinite program loop.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-03-16
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/InfiniTick"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized InfiniTick commands."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :raise-error
    :skip-if-zero
    :skip-if-not-zero))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype infiniTick-program ()
  "The ``infiniTick-program'' type defines an executable form of an
   InfiniTick program as a list of zero or more ``command'' objects."
  '(list-of command))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight
   consecutive bits, thus being commorant in the closed integer range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype cell-map ()
  "The ``cell-map'' type defines a sparse vector of unsigned-bytes,
   indexed via arbitrary integer subscripts, implemented in the form of
   a hash table whose keys assume the integer type, mapping to octet
   values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 8) +COMMAND-TOKENS+))

;;; -------------------------------------------------------

(defparameter +COMMAND-TOKENS+ "><+-*&/\\"
  "Enumerates the recognized command tokens.")

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Determines whether the TOKEN represents an InfiniTick command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token +COMMAND-TOKENS+ :test #'char=)))))

;;; -------------------------------------------------------

(defun get-command (token)
  "Returns the command affiliated with the TOKEN, or signals an error of
   an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command
    (case token
      (#\>       :move-right)
      (#\<       :move-left)
      (#\+       :increment)
      (#\-       :decrement)
      (#\*       :output)
      (#\&       :raise-error)
      (#\/       :skip-if-zero)
      (#\\       :skip-if-not-zero)
      (otherwise (error "No command token: ~s." token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts from the piece of InfiniTick source CODE a list of the
   comprehended commands and returns the same."
  (declare (type string code))
  (the infiniTick-program
    (loop
      for token of-type character across code
      when (command-token-p token)
        collect (get-command token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-map
    :documentation "Maps the cell indices to cell values.")
   (selector
    :initarg       :selector
    :initform      0
    :type          integer
    :documentation "Stores the index of the current cell, that is, the
                    active cell's key in the CELLS hash table."))
  (:documentation
    "The ``Memory'' class represents a tape composed of an infinite
     tally of cells, liberally expanding upon both lateralities, each
     such a salvatory to a single unsigned byte datum, and operated upon
     by a cursor, known as the \"selector\", which at any instant
     denotes the currently active cell."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the current MEMORY cell's value."
  (declare (type Memory memory))
  (the octet
    (gethash (slot-value memory 'selector)
      (slot-value memory 'cells) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Sets the current MEMORY cell's value to the NEW-VALUE, contingently
   wrapping it around the unsigned byte range [0, 255] prior to its
   induction, and returns no value."
  (declare (type integer new-value))
  (declare (type Memory   memory))
  (setf (gethash (slot-value memory 'selector)
          (slot-value memory 'cells) 0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the current MEMORY cell's value by one, contingently
   wrapping it around to the minimum of zero (0) if threatening to
   violate the upper byte march of 255, and returns no value."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the current MEMORY cell's value by one, contingently
   wrapping it around to the maximum of 255 if threatening to violate
   the lower byte march of zero (0), and returns no value."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (values))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Translates the MEMORY's cell selector one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (slot-value memory 'selector))
  (values))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Translates the MEMORY's cell selector one step to the left and
   returns no value."
  (declare (type Memory memory))
  (decf (slot-value memory 'selector))
  (values))

;;; -------------------------------------------------------

(defun memory-print-current-cell (memory &optional (destination T))
  "Prints the character associated with the current MEMORY cell,
   construed as an ASCII code, to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type Memory      memory))
  (declare (type destination destination))
  (the (or null string)
    (format destination "~c"
      (code-char
        (memory-current-cell memory)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "InfiniTick-Error".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition InfiniTick-Error (error)
  ()
  (:report
    (lambda (condition stream)
      (declare (type InfiniTick-Error condition)
               (ignore                condition))
      (declare (type destination      stream))
      (format stream "*** An InfiniTick-internal error was raised. ~
                      The program loop will be terminated. ***~%")))
  (:documentation
    "The ``InfiniTick-Error'' condition is used to notify about the
     desire to terminate the program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-commands (commands)
  "Evaluates the COMMANDS and returns no value."
  (declare (type infiniTick-program commands))
  (let ((memory              (make-memory))
        (skip-next-command-p NIL))
    (declare (type Memory  memory))
    (declare (type boolean skip-next-command-p))
    (handler-case
      (loop do
        (loop for command of-type command in commands do
          (if skip-next-command-p
            (setf skip-next-command-p NIL)
            (case command
              (:move-right
                (memory-move-right memory))
              (:move-left
                (memory-move-left memory))
              (:increment
                (memory-increment memory))
              (:decrement
                (memory-decrement memory))
              (:output
                (memory-print-current-cell memory))
              (:raise-error
                (error 'InfiniTick-Error))
              (:skip-if-zero
                (when (zerop (memory-current-cell memory))
                  (setf skip-next-command-p T)))
              (:skip-if-not-zero
                (unless (zerop (memory-current-cell memory))
                  (setf skip-next-command-p T)))
              (otherwise
                (error "Invalid command: ~s." command))))))
      (InfiniTick-Error (error)
        (format T "~&~a" error))))
  (values))

;;; -------------------------------------------------------

(defun interpret-InfiniTick (code)
  "Interprets the piece of InfiniTick source CODE and returns no value."
  (process-commands
    (extract-commands code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A" and terminate the program using an error.
(interpret-InfiniTick
  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*&")

;;; -------------------------------------------------------

;; Print "Hello world!" to the standard output and terminate the
;; program.
(interpret-InfiniTick
"
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*+++++++++++++++++++++++++++++*+++++++**+++*>+++++++++++++++++++++++++++
+++++*<++++++++*>+++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*+++*>++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++*
&
")

;;; -------------------------------------------------------

;; Infinitely print "Hello world!" to the standard output.
(interpret-InfiniTick
"
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*+++++++++++++++++++++++++++++*+++++++**+++*>+++++++++++++++++++++++++++
+++++*<++++++++*>+++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*+++*>++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*>++++
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
++++++++++++++++++++++++*>+++++++++++++++++++++++++++++++++*
>
")

;;; -------------------------------------------------------

;; Print all 256 ASCII characters and terminate the program.
;; The termination command is skipped as long as the current cell value
;; does not equal zero (0); the predicate is satisfied using the
;; wrapping nature of the cells following the incrementing of 255 to 0.
(interpret-InfiniTick
  "*
   +
   \\&")
