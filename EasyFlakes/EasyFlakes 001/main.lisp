;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "EasyFlakes", invented by the Esolang user "Cinnamony" and
;; presented on June 15th, 2023, the haecceity of which constitutes a
;; verbatim appropriation of Urban Mueller's "brainfuck" in all aspects,
;; except for the donet, the same acquires a more lucid design, akin to
;; English language expressions.
;; 
;; 
;; Concept
;; =======
;; The EasyFlakes programming language is based upon a reformulation of
;; brainfuck's instructions in a more human-readable form, with
;; concomitant retention of all other characteristics.
;; 
;; 
;; Instructions
;; ============
;; A legatee of brainfuck's concepts, EasyFlakes appropriates the
;; octuple instruction set's entirety, expressed, however, in a distinct
;; raiment.
;; 
;; == OVERVIEW ==
;; A foundational mete of gnarity concenring the EasyFlakes capacities
;; shall be communicated by the following tabular illustration:
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   up       | Increments the current cell by one.
;;            | If the new value transgresses the upper bourne of 255,
;;            | the state wraps around to the minimum of zero (0).
;;   ..................................................................
;;   down     | Decrements the current cell by one.
;;            | If the new value transgresses the lower bourne of zero
;;            | (0), the state wraps around to the maximum of 255.
;;   ..................................................................
;;   right    | Translates the memory's cell pointer one step to the
;;            | right.
;;   ..................................................................
;;   left     | Translates the memory's cell pointer one step to the
;;            | left.
;;   ..................................................................
;;   output   | Prints the character whose ASCII code corresponds to
;;            | the current cell value to the standard output.
;;   ..................................................................
;;   input    | Queries the standard input for an ASCII character whose
;;            | character code is subsequently stored in the current
;;            | cell.
;;   ..................................................................
;;   bracket( | While the current cell value does not equal zero (0),
;;            | repeatedly executes the commands betwixt this marker
;;            | and the matching ")" command.
;;   ..................................................................
;;   )        | Demarcates the end of the loop body commenced by the
;;            | matching "bracket(" command.
;;   ------------------------------------------------------------------
;; 
;; == EASYFLAKES AND BRAINFUCK ==
;; The inambiguous equiparation governing the relations betwixt
;; EasyFlakes and its sire brainfuck that is capacitated to establish
;; shall be limned in a juxtaposing format:
;; 
;;   ----------------------
;;   EasyFlakes | brainfuck
;;   -----------+----------
;;   up         | +
;;   ......................
;;   down       | -
;;   ......................
;;   right      | >
;;   ......................
;;   left       | <
;;   ......................
;;   output     | .
;;   ......................
;;   input      | ,
;;   ......................
;;   bracket(   | [
;;   ......................
;;   )          | ]
;;   ----------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-01
;; 
;; Sources:
;;   [esolang2023EasyFlakes]
;;   The Esolang contributors, "EasyFlakes", June 15th, 2023
;;   URL: "https://esolangs.org/wiki/EasyFlakes"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   conforming to the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, the keys of which conforms to the KEY-TYPE and
   associate with a value of the VALUE-TYPE, both defaulting to the
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

(deftype command-type ()
  "The ``command-type'' type enumerates the recognized EasyFlakes
   instruction variants."
  '(member
    :right
    :left
    :up
    :down
    :output
    :input
    :bracket))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of command names to
   representative objects, realized as a hash table whose string keys
   associate with ``command'' values."
  '(hash-table-of string command-type))

;;; -------------------------------------------------------

(deftype atomic-command ()
  "The ``atomic-command'' type defines an atomic command as a simple
   instance of the ``command-type'', destitute of subordinate elements."
  'command-type)

;;; -------------------------------------------------------

(deftype compound-command ()
  "The ``compound-command'' type defines a command composed of zero or
   more subordinate commands, that is, a loop body."
  '(list-of command))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type defines a general instruction as either an
   ``atomic-command'' or a ``compound-command'' which comprehends
   further instances."
  '(or atomic-command
       compound-command))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an EasyFlakes program as an ordered list
   of zero or more ``command'' instances."
  '(list-of command))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   consecutive bits, thus a commorant of the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a bilaterally
   boundless tape of octet-valued cells, each amenable to a unique
   integer index, and realized in a hash table that maps the integer
   locators to the octet values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   embracing in their definition, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized identifiers, that is, command names, with
   representative objects.")

;;; -------------------------------------------------------

(setf (gethash "bracket" +IDENTIFIERS+) :bracket)
(setf (gethash "down"    +IDENTIFIERS+) :down)
(setf (gethash "input"   +IDENTIFIERS+) :input)
(setf (gethash "left"    +IDENTIFIERS+) :left)
(setf (gethash "output"  +IDENTIFIERS+) :output)
(setf (gethash "right"   +IDENTIFIERS+) :right)
(setf (gethash "up"      +IDENTIFIERS+) :up)

;;; -------------------------------------------------------

(defun parse-command-type (identifier)
  "Returns the command type associated with the IDENTIFIER, or signals
   an error of an unspecified type upon its ineligibility."
  (declare (type string identifier))
  (the command-type
    (or (gethash identifier +IDENTIFIERS+)
        (error "Unrecognized identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position
   immediately succeeding the skipped portion in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word
   composed of letters only, contingently preceded by a series of zero
   or more whitespaces, and returns two values:
     (1) The consumed word as a fresh string.
     (2) The position into the SOURCE immediately succeeding the
         consumed word and, if such follow the token, any accolent
         whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (word (make-string-output-stream))
      (declare (type string-stream word))
      (loop
        for position
          of-type fixnum
          from    (skip-whitespaces source start)
          below   (length source)
        for character
          of-type character
          =       (char source position)
        while (alpha-char-p character)
        do    (write-char character word)
        finally
          (return
            (values
              (get-output-stream-string word)
              (skip-whitespaces source position)))))))

;;; -------------------------------------------------------

(defun read-command-type (source start)
  "Proceeding from the START position into the SOURCE, reads a command
   type, contingently preceded by a series of zero or more whitespaces,
   and returns two values:
     (1) The consumed command type as a ``command-type'' object.
     (2) The position into the SOURCE immediately succeeding the
         consumed command type and, if such follow the identifier, the
         accolent whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values command-type fixnum)
    (multiple-value-bind (word position)
        (read-word source start)
      (declare (type string word))
      (declare (type fixnum position))
      (values
        (parse-command-type word)
        position))))

;;; -------------------------------------------------------

(defun expect-character (source start expected-character)
  "Determines whether, proceeding from the START position into the
   SOURCE, the EXPECTED-CHARACTER, contingently preceded by whitespaces,
   occurs, on confirmation returning the position following the
   EXPECTED-CHARACTER in the SOURCE, skipping any succeeding
   whitespaces."
  (declare (type string    source))
  (declare (type fixnum    start))
  (declare (type character expected-character))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (cond
      ((not (array-in-bounds-p source position))
        (error "Expected the character \"~c\" at position ~d, ~
                but found the source exhausted."
          expected-character position))
      ((char/= (char source position) expected-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\"."
          expected-character position
          (char source position)))
      (T
        NIL))
    (the fixnum
      (skip-whitespaces source (1+ position)))))

;;; -------------------------------------------------------

(defun parse-commands (source start)
  "Proceeding from the START position into the SOURCE, extracts and
   returns a sequence of zero or more commands, contingently preceded
   by whitespaces, and returns two values:
     (1) A list of the detected commands in their correct order.
     (2) The position immediately succeding the desinent command,
         potentially skipping any following whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((commands NIL)
        (position start))
    (declare (type program commands))
    (declare (type fixnum  position))
    (flet ((accept-command-type (command-type end)
            "Updates the POSITION cursor to the END location and returns
             the COMMAND-TYPE."
            (declare (type command-type command-type))
            (declare (type fixnum       end))
            (setf position end)
            (the command-type command-type))
           
           (command-follows-p ()
            "Determines whether, proceeding from the current POSITION,
             a command name follows, returning on confirmation a
             ``boolean'' value of ``T'', otherwise ``NIL''."
            (the boolean
              (not (null
                (and (< position (length source))
                     (alpha-char-p (char source position))))))))
      (loop
        initially
          (setf position
            (skip-whitespaces source position))
        while
          (command-follows-p)
        for command-type
          of-type command-type
          =       (multiple-value-call #'accept-command-type
                    (read-command-type source position))
        do
          (case command-type
            (:bracket
              (setf position
                (expect-character source position #\())
              (multiple-value-bind (loop-body new-position)
                  (parse-commands source position)
                (declare (type program loop-body))
                (declare (type fixnum  new-position))
                (setf position new-position)
                (push loop-body commands))
              (setf position
                (expect-character source position #\))))
            (otherwise
              (push command-type commands)))))
    (the (values program fixnum)
      (values
        (the program
          (nreverse commands))
        position))))

;;; -------------------------------------------------------

(defun expect-end-of-program (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, the latter comprehends no other content apart from contingent
   whitespaces, on confirmation returning no value, otherwise signaling
   an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-whitespaces source start)))
    (declare (type fixnum position))
    (when (array-in-bounds-p source position)
      (error "Expected the source to be exhausted, but encountered ~
              the character \"~c\" at position ~d."
        (char source position) position)))
  (values))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Extracts and returns a list of the EasyFlakes commands entailed in
   the SOURCE."
  (declare (type string source))
  (let ((commands NIL)
        (position 0))
    (declare (type program commands))
    (declare (type fixnum  position))
    (multiple-value-setq (commands position)
      (parse-commands source position))
    (expect-end-of-program source position)
    (the program commands)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-type (command)
  "Returns the COMMAND's command type, or signals an error of an
   unspecified type if the same eludes a determination."
  (declare (type command command))
  (typecase command
    (atomic-command   command)
    (compound-command :bracket)
    (otherwise
      (error "Cannot determine the command type: ~s." command))))

;;; -------------------------------------------------------

(defun interpret-program (program)
  "Executes the EasyFlakes PROGRAM and returns no value."
  (declare (type program program))
  (let ((memory       (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type memory  memory))
    (declare (type integer cell-pointer))
    (labels
        ((current-cell ()
          "Returns the MEMORY's current cell value."
          (the octet
            (gethash cell-pointer memory 0)))
         
         ((setf current-cell) (new-value)
          "Stores the NEW-VALUE in the MEMORY's current cell,
           contingently preceded by wrapping the same into the valid
           unsigned byte range of [0, 255], and returns no value."
          (setf (gethash cell-pointer memory 0)
                (mod new-value 256))
          (values))
         
         (process-command (command)
          "Processes the EasyFlakes COMMAND and returns no value."
          (declare (type command command))
          (case (get-command-type command)
            (:up     (incf (current-cell)))
            (:down   (decf (current-cell)))
            (:right  (incf cell-pointer))
            (:left   (decf cell-pointer))
            (:output
              (write-char
                (code-char
                  (current-cell))))
            (:input
              (format *standard-output* "~&>> ")
              (force-output *standard-output*)
              (setf (current-cell)
                    (char-code
                      (read-char *standard-input* NIL #\Null)))
              (clear-input *standard-input*))
            (:bracket
              (loop until (zerop (current-cell)) do
                (dolist (statement command)
                  (declare (type command statement))
                  (process-command statement))))
            (otherwise
              (error "Unrecognized command: ~s." command)))
          (values)))
      (loop
        for command of-type command in program do
          (process-command command))))
  (values))

;;; -------------------------------------------------------

(defun interpret-EasyFlakes (code)
  "Interprets the piece of EasyFlakes source CODE and returns no value."
  (interpret-program
    (parse-program code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-EasyFlakes converter.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-EasyFlakes (brainfuck-code
                                        &key (destination NIL))
  "Generates for the BRAINFUCK-CODE an equivalent EasyFlakes program,
   writes the same to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((requires-separator-p NIL))
        (declare (type boolean requires-separator-p))
        (flet ((write-text (text)
                "Writes the TEXT to the DESTINATION, prepending, upon
                 the REQUIRES-SEPARATOR-P flag's affirmation, a space,
                 and returns no value."
                (declare (type string))
                (when requires-separator-p
                  (format destination " "))
                (format destination "~a" text)
                (setf requires-separator-p T)
                (values)))
          (loop for bf-token of-type character across brainfuck-code do
            (case bf-token
              (#\+       (write-text "up"))
              (#\-       (write-text "down"))
              (#\>       (write-text "right"))
              (#\<       (write-text "left"))
              (#\.       (write-text "output"))
              (#\,       (write-text "input"))
              (#\[       (write-text "bracket(")
                         (setf requires-separator-p NIL))
              (#\]       (format destination ")")
                         (setf requires-separator-p T))
              (otherwise "")))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-EasyFlakes brainfuck-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-EasyFlakes
  "up bracket(down down right down bracket(right right up right down down down down down left left) left down down left down down down) right down output right right right up output right right output output up up up bracket(output right) left left left left output up up up output down down down down down down output left left down output right right right right up output")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a
;; "null character" input.
(interpret-EasyFlakes "up bracket(input output)")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-EasyFlakes
  "input output bracket(down down right up bracket(right right) left bracket(output) left left)")

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello, World!" program to EasyFlakes code and
;; interpret it.
(interpret-EasyFlakes
  (convert-brainfuck-to-EasyFlakes
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))
