;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "7-8", created by the Esolang user "Bot4ol", and intended
;; as an equivalent of the inspiring esoteric peer "brainfuck" by Urban
;; Mueller, substituting the cleronomy's instructions by the 7-8 meme.
;; 
;; A dioristic variation, a command is identified by the tally of "7-8"
;; tokens commorant on a line, thus each non-empty row resolves to a
;; single instruction.
;; 
;; 
;; Instructions
;; ============
;; Conceptually, 7-8 employs the same instructions as brainfuck, however
;; encoded in the number of "7-8" tokens sharing one line.
;; 
;; == OVERVIEW ==
;; The 7-8 programming language receives from its brainfuck cleronomy
;; a command set of octuple cardinality; an inchoate apercu shall
;; educate about its circumference, ere a more complete rendition
;; intrudes into the intrinsics.
;; 
;;   ------------------------------------------------------------------
;;   7-8 sequence                    | Tally | Command name
;;   --------------------------------+-------+-------------------------
;;   7-8                             | 1     | jump back
;;   ..................................................................
;;   7-8 7-8                         | 2     | jump forward
;;   ..................................................................
;;   7-8 7-8 7-8                     | 3     | output
;;   ..................................................................
;;   7-8 7-8 7-8 7-8                 | 4     | increment
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8             | 5     | decrement
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8         | 6     | move left
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8 7-8     | 7     | input
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8 | 8     | move right
;;   ------------------------------------------------------------------
;; 
;; The commands shall now be subject to a more throughout treatise:
;; 
;;   ------------------------------------------------------------------
;;   Tally of 7-8s | Command      | Effect
;;   --------------+--------------+------------------------------------
;;   1             | jump back    | If the current cell value is not
;;                 |              | zero (0), jumps back to the
;;                 |              | position following the matching
;;                 |              | "jump forward" command. Otherwise
;;                 |              | proceeds as usual.
;;   ..................................................................
;;   2             | jump forward | If the current cell value equals
;;                 |              | zero (0), jumps forward to the
;;                 |              | position following the matching
;;                 |              | "jump back" command. Otherwise
;;                 |              | proceeds as usual.
;;   ..................................................................
;;   3             | output       | Prints to the standard output the
;;                 |              | ASCII character associated with the
;;                 |              | current cell value.
;;   ..................................................................
;;   4             | increment    | Increments the current cell value
;;                 |              | by one.
;;   ..................................................................
;;   5             | decrement    | Decrements the current cell value
;;                 |              | by one.
;;   ..................................................................
;;   6             | move left    | Moves the cell pointer one cell to
;;                 |              | the left.
;;   ..................................................................
;;   7             | input        | Queries the user for an input
;;                 |              | character and stores its ASCII code
;;                 |              | in the current cell.
;;   ..................................................................
;;   8             | move right   | Moves the cell pointer one cell to
;;                 |              | right.
;;   ------------------------------------------------------------------
;; 
;; == RELATIONS TO BRAINFUCK ==
;; Its equipollence to brainfuck imparts 7-8 an unambiguous association
;; of its command set to that of its stock-father:
;; 
;;   ------------------------------------------------------------------
;;   7-8 sequence                    | Tally | brainfuck equivalent
;;   --------------------------------+-------+-------------------------
;;   7-8                             | 1     | ]
;;   ..................................................................
;;   7-8 7-8                         | 2     | [
;;   ..................................................................
;;   7-8 7-8 7-8                     | 3     | .
;;   ..................................................................
;;   7-8 7-8 7-8 7-8                 | 4     | +
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8             | 5     | -
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8         | 6     | <
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8 7-8     | 7     | ,
;;   ..................................................................
;;   7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8 | 8     | >
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/7-8"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associating with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of loop endpoints in the
   form of a hash table which maps fixnum positions, doing so by
   associating with each loop start point the matching end point and
   vice versa."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype tape ()
  "The ``tape'' type defines the program memory as a hash table of
   integers mapped to the same data type, the former being
   representatives of the cell indices, the latter defining the
   respective cell values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized 7-8 instruction
   types, which concomitantly conflate with brainfuck's."
  '(member
    :decrement
    :increment
    :input
    :loop-start
    :loop-end
    :move-left
    :move-right
    :output))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 3) +7-8-TOKEN+))

;;; -------------------------------------------------------

(declaim (type (or null string)  *line*))
(declaim (type fixnum            *position*))
(declaim (type character         *character*))
(declaim (type boolean           *characters-pending-p*))

;;; -------------------------------------------------------

(defparameter +7-8-TOKEN+ "7-8"
  "The singular instruction constituent \"7-8\", whose tally on a line
   determines the corresponding brainfuck command.")

;;; -------------------------------------------------------

(defparameter *line* NIL
  "The currently analyzed and parsed 7-8 source code line.")

(defparameter *position* 0
  "The index into the current *LINE*.")

;;; -------------------------------------------------------

(define-symbol-macro *character*
  (the character
    (char *line* *position*)))

;;; -------------------------------------------------------

(define-symbol-macro *characters-pending-p*
  (the boolean
    (not (null
      (and *line*
           (array-in-bounds-p *line* *position*))))))

;;; -------------------------------------------------------

(defun set-line (new-line)
  "Sets the currently processed 7-8 source code *LINE* to the NEW-LINE,
   resets all appertaining state variables, and returns the modified
   *LINE*."
  (declare (type (or null string) new-line))
  (setf *line*     new-line)
  (setf *position* 0)
  (the (or null string) *line*))

;;; -------------------------------------------------------

(defun space-character-p (character)
  "Checks whether the CHARACTER constitutes a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Starting at the current *POSITION*, skips a sequence of zero or more
   consecutive spaces and returns no value."
  (loop
    while (and *characters-pending-p*
               (space-character-p *character*))
    do    (incf *position*))
  (values))

;;; -------------------------------------------------------

(defun space-follows-p ()
  "Checks whether the current *CHARACTER* constitutes a space, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (not (null
      (and *characters-pending-p*
           (space-character-p *character*))))))

;;; -------------------------------------------------------

(defun expect-space ()
  "Checks whether the current *CHARACTER* represents a space, on
   confirmation relocating the *POSITION* pointer past the same and
   returning no value; otherwise signaling an error of an unspecified
   type."
  (if (and *characters-pending-p*
           (space-character-p *character*))
    (incf *position*)
    (error "Expected a space, but encountered ~s at position ~d."
      *character* *position*))
  (values))

;;; -------------------------------------------------------

(defun expect-token ()
  "Checks whether the character sequence starting at the current
   *POSITION* into the *LINE* matches the \"7-8\" token, on confirmation
   relocating the *POSITION* pointer past the conformant portion, before
   returning with no value; otherwise signaling an error of an
   unspecified type."
  (loop for expected-character of-type character across +7-8-TOKEN+ do
    (if (and *characters-pending-p*
             (char= *character* expected-character))
      (incf *position*)
      (error "Expected the token character ~c, ~
              but encountered ~s at position ~d."
        expected-character *character* *position*)))
  (values))

;;; -------------------------------------------------------

(defun count-7-8-tokens ()
  "Tallies and returns the number of occurrences of the \"7-8\" token
   on the current *LINE* while processing the same."
  (let ((number-of-7-8-tokens 0))
    (declare (type (integer 0 *) number-of-7-8-tokens))
    (flet
        ((read-optional-token ()
          "If possible, consumes a \"7-8\" token, potentially preceded
           by a sequence of spaces, and tallies it among the
           NUMBER-OF-7-8-TOKENS, returning in any case no value."
          (skip-spaces)
          (cond
            ((not *characters-pending-p*)
              NIL)
            ((and *characters-pending-p*
                  (char= *character* #\7))
              (expect-token)
              (incf number-of-7-8-tokens))
            (T
              (error "Expected either a token or the end of the line, ~
                      but encountered ~c at position ~d."
                *character* *position*)))
          (values)))
      
      (read-optional-token)
      
      ;; Consume contingent succeeding "7-8" tokens, demarcated by the
      ;; previous ones through at least one space character.
      (loop while *characters-pending-p* do
        (if (space-follows-p)
          (read-optional-token)
          (error "Invalid character ~a at position ~d."
            *character* *position*))))
    (the (integer 0 *) number-of-7-8-tokens)))

;;; -------------------------------------------------------

(defun get-command-for (number-of-7-8-tokens)
  "Returns the command corresponding with the NUMBER-OF-7-8-TOKENS, or
   signals an error if lacking such an association."
  (declare (type (integer 0 *) number-of-7-8-tokens))
  (the command
    (case number-of-7-8-tokens
      (1 :loop-end)
      (2 :loop-start)
      (3 :output)
      (4 :increment)
      (5 :decrement)
      (6 :move-left)
      (7 :input)
      (8 :move-right)
      (otherwise
        (error "No command associated with the token count ~d."
          number-of-7-8-tokens)))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of 7-8 CODE a one-dimensional
   simple array of commands."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (with-input-from-string (input-stream code)
      (declare (type string-stream input-stream))
      (loop while (set-line (read-line input-stream NIL NIL)) do
        (let ((number-of-7-8-tokens (count-7-8-tokens)))
          (declare (type (integer 0 *) number-of-7-8-tokens))
          (when (plusp number-of-7-8-tokens)
            (push (get-command-for number-of-7-8-tokens)
                  instructions)))))
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (vector command *) *instructions*))
(declaim (type fixnum             *ip*))
(declaim (type (or null command)  *current-instruction*))
(declaim (type jump-table         *jump-table*))
(declaim (type boolean            *instructions-pending-p*))
(declaim (type tape               *tape*))
(declaim (type integer            *pointer*))
(declaim (type integer            *current-cell*))

;;; -------------------------------------------------------

(defparameter *instructions* (coerce NIL '(vector command *))
  "The instruction vector to process.")

(defparameter *ip*           0
  "The instruction pointer (IP) which references the currently processed
   instruction of the *INSTRUCTIONS* sequence.")

(defparameter *jump-table*   (make-hash-table :test #'eql)
  "A table which associates each loop start position in the
   *INSTRUCTIONS* vector to the matching end position and vice versa.")

(defparameter *tape*         (make-hash-table :test #'eql)
  "The program memory as a tape of cells, the hash table keys of which
   designate the cell indices, associated with the cell values.")

(defparameter *pointer*      0
  "The cell pointer which selects the currently active *TAPE* cell.")

;;; -------------------------------------------------------

(define-symbol-macro *current-instruction*
  (the command
    (aref *instructions* *ip*)))

;;; -------------------------------------------------------

(define-symbol-macro *instructions-pending-p*
  (the boolean
    (not (null
      (array-in-bounds-p *instructions* *ip*)))))

;;; -------------------------------------------------------

(define-symbol-macro *current-cell*
  (the integer
    (gethash *pointer* *tape* 0)))

;;; -------------------------------------------------------

(defun compute-jump-table ()
  "Populates the *JUMP-TABLE* with the detected and matched loop
   endpoints and returns the modified *JUMP-TABLE*.
   ---
   Please note that the *JUMP-TABLE* will not be cleared ere this
   operation's inception; thus, if optated, the step must precede the
   invocation."
  (let ((loop-starts NIL))
    (declare (type (list-of fixnum) loop-starts))
    (loop
      for command  of-type command across *instructions*
      and position of-type fixnum  from   0
      do
        (case command
          (:loop-start
            (push position loop-starts))
          (:loop-end
            (if loop-starts
              (let ((start-position (pop loop-starts)))
                (declare (type fixnum start-position))
                (setf (gethash start-position *jump-table*) position)
                (setf (gethash position *jump-table*) start-position))
              (error "Unmatched loop end at position ~d of the ~
                      instruction vector."
                position)))
          (otherwise
            NIL)))
    (when loop-starts
      (error "Unmatched loop starts: ~{~d~^, ~}." loop-starts)))
  (the jump-table *jump-table*))

;;; -------------------------------------------------------

(defun jump-forward ()
  "Relocates the instruction pointer *IP* to the loop end position
   associated with the *IP*'s current location and returns no value.
   ---
   An error of an unspecified type is signaled if no destination could
   be matched."
  (multiple-value-bind (new-position has-destination-p)
      (gethash *ip* *jump-table*)
    (declare (type (or null fixnum) new-position))
    (declare (type T                has-destination-p))
    (if has-destination-p
      (setf *ip* new-position)
      (error "No jump destination associated with the position ~d."
        *ip*)))
  (values))

;;; -------------------------------------------------------

(defun jump-back ()
  "Relocates the instruction pointer *IP* to the loop start position
   associated with the *IP*'s current location and returns no value.
   ---
   An error of an unspecified type is signaled if no provenance could
   be matched."
  (multiple-value-bind (new-position has-destination-p)
      (gethash *ip* *jump-table*)
    (declare (type (or null fixnum) new-position))
    (declare (type T                has-destination-p))
    (if has-destination-p
      (setf *ip* new-position)
      (error "No jump source associated with the position ~d." *ip*)))
  (values))

;;; -------------------------------------------------------

(defun set-instructions (new-instructions)
  "Sets the *INSTRUCTIONS* to the NEW-INSTRUCTIONS, resets the
   interpreter's state variables, and returns the modified
   *INSTRUCTIONS*."
  (declare (type (vector command *) new-instructions))
  (setf    *instructions* new-instructions)
  (compute-jump-table)
  (setf    *ip* 0)
  (clrhash *tape*)
  (setf    *pointer* 0)
  (the (vector command *) *instructions*))

;;; -------------------------------------------------------

(defun process-instructions ()
  "Processes the *INSTRUCTIONS* vector and returns no value."
  (loop while *instructions-pending-p* do
    (case *current-instruction*
      (:increment
        (incf *current-cell*))
      (:decrement
        (decf *current-cell*))
      (:move-right
        (incf *pointer*))
      (:move-left
        (decf *pointer*))
      (:output
        (write-char (code-char *current-cell*)))
      (:input
        (format T "~&Please input an ASCII character: ")
        (setf *current-cell* (char-code (read-char)))
        (clear-input))
      (:loop-start
        (when (zerop *current-cell*)
          (jump-forward)))
      (:loop-end
        (unless (zerop *current-cell*)
          (jump-back)))
      (otherwise
        (error "Invalid instruction ~s at position ~d."
          *current-instruction* *ip*)))
    (incf *ip*))
  (values))

;;; -------------------------------------------------------

(defun interpret-7-8 (code)
  "Interprets the piece of 7-8 CODE and returns no value."
  (set-instructions (extract-instructions code))
  (process-instructions)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-7-8 converter.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (character)
  "Checks whether the CHARACTER represents a brainfuck command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (find character "+-<>.,[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun get-7-8-tally-for-brainfuck-token (bf-token)
  "Returns the number of \"7-8\" tokens representing the brainfuck
   command character BF-TOKEN, or signals an error upon the absence of a
   correspondence."
  (declare (type character bf-token))
  (the (integer 1 8)
    (case bf-token
      (#\] 1)
      (#\[ 2)
      (#\. 3)
      (#\+ 4)
      (#\- 5)
      (#\< 6)
      (#\, 7)
      (#\> 8)
      (otherwise
        (error "Invalid brainfuck token: ~s." bf-token)))))

;;; -------------------------------------------------------

(defun output-7-8-tokens (number-of-7-8-tokens destination)
  "Prints a tally equal to the NUMBER-OF-7-8-TOKENS of \"7-8\" sequences
   to the DESTINATION and returns no value."
  (declare (type (integer 1 8) number-of-7-8-tokens))
  (declare (type destination   destination))
  (loop
    for    first-token-p of-type boolean = T then NIL
    repeat number-of-7-8-tokens
    do
      (unless first-token-p
        (format destination " "))
      (format destination +7-8-TOKEN+))
  (values))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-7-8 (bf-code &key (destination NIL))
  "Converts the piece of brainfuck code BF-CODE to a 7-8 program and
   writes it to the DESTINATION, returning for a non-``NIL'' DESTINATION
   the ``NIL'' value, otherwise responding with a fresh string
   comprehending the result."
  (declare (type string      bf-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for bf-character of-type character across bf-code do
        (when (brainfuck-command-p bf-character)
          (format destination "~&")
          (output-7-8-tokens
            (get-7-8-tally-for-brainfuck-token bf-character)
            destination)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-7-8 bf-code :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-7-8
"
7-8 7-8 7-8 7-8

7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8
7-8 7-8 7-8 7-8 7-8 7-8 7-8 7-8
7-8 7-8 7-8 7-8
7-8 7-8 7-8
")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-7-8
  "7-8 7-8 7-8 7-8 7-8 7-8 7-8
   7-8 7-8
   7-8 7-8 7-8
   7-8 7-8 7-8 7-8 7-8 7-8 7-8
   7-8")

;;; -------------------------------------------------------

;; Convert the brainfuck Truth-machine to 7-8 and print the resulting
;; program to the standard output.
(convert-brainfuck-to-7-8 ">>>,.[[->+<<+>]>-]<<<[<<]>[.]"
  :destination T)

;;; -------------------------------------------------------

;; Convert the brainfuck Truth-machine to 7-8 and interpret the
;; resulting program.
(interpret-7-8
  (convert-brainfuck-to-7-8 ">>>,.[[->+<<+>]>-]<<<[<<]>[.]"))
