;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "!!Fuck", presented by the Esolang user "Lebster" in the
;; year 2020, and "brainfuck" by Urban Mueller, as well as routines for
;; the encoding and decoding betwixt the two esoteric programming
;; languages.
;; 
;; 
;; Concept
;; =======
;; !!Fuck constitutes an equivalent of the esoteric programming
;; language brainfuck, the single-character instructions of which have
;; been substituted by series of exclamation marks ("!") terminated with
;; a number sign ("#"), the former character's tally determining the
;; actual command.
;; 
;; == CONVERSION OF BRAINFUCK TO !!FUCK ==
;; Being based on a trivial substitution, converting from brainfuck to
;; !!Fuck merely involves the substitution of the former's instructions
;; with the associated analogues of the latter. The following table
;; conveys the mapping in this direction:
;; 
;;   ------------------------------------------------------------------
;;   brainfuck instruction | !!Fuck equivalent | Tally of "!"
;;   ----------------------+-------------------+-----------------------
;;   >                     | !!!!!#            |  5
;;   <                     | !!!!!!#           |  6
;;   +                     | !!!!!!!#          |  7
;;   -                     | !!!!!!!!#         |  8
;;   ,                     | !!!!!!!!!#        |  9
;;   .                     | !!!!!!!!!!#       | 10
;;   [                     | !!!!!!!!!!!#      | 11
;;   ]                     | !!!!!!!!!!!!#     | 12
;;   ------------------------------------------------------------------
;; 
;; Note that brainfuck tacitly ignores any character not defined as an
;; instruction, an expression of tolerance that is usually appropriated
;; to state commands in the source code. The reservation of the two
;; substantial character "!" and "#" in !!Fuck imposes a risk if
;; occurring as comments in the brainfuck code. This in conjunction with
;; no explicit assurance of non-instruction characters being toleraded
;; in !!Fuck code, it is safe and advised to skip content not
;; appertaining to brainfuck commands.
;; 
;; == CONVERSION OF !!FUCK TO BRAINFUCK ==
;; The facility of the transcription model propagates through the
;; athwart direction. When converting !!Fuck instructions to brainfuck,
;; each instruction from the source must be substituted by the
;; destination language's equivalent. The following tabular
;; representation conforms to this perspective:
;; 
;;   -----------------------------------------
;;   !!Fuck instruction | brainfuck equivalent
;;   -------------------+---------------------
;;   !!!!!#             | >
;;   !!!!!!#            | <
;;   !!!!!!!#           | +
;;   !!!!!!!!#          | -
;;   !!!!!!!!!#         | ,
;;   !!!!!!!!!!#        | .
;;   !!!!!!!!!!!#       | [
;;   !!!!!!!!!!!!#      | ]
;;   -----------------------------------------
;; 
;; By transferring the brainfuck instructions into a common sink, the
;; decoded program is produced. If non-instruction characters have been
;; utilized as comments in the !!Fuck code, and their conveyance to the
;; brainfuck sink shall be exercised, it is important to avoid
;; collisions with the latter's instruction set.
;; 
;; 
;; Architecture
;; ============
;; !!Fuck's architecture, having been begotten by brainfuck's concepts,
;; bifurcates in its principles' lealty along two not necessarily
;; equipotent interpretations, with a memory whose potence designs it
;; permissive to any tally of cells, these, however, being restricted to
;; a byte-valued capacity.
;; 
;; == THE MEMORY: AN INFINITE TALLY OF CELLS ==
;; The scion administers a contingency to the memory's construction
;; siclike to the brainfuck stock-father in the presentation of a
;; theoretically infinite account of cells, linearly arranged, and
;; expanding into both the positive and negative airt without
;; limitations.
;; 
;; == EACH CELL CONTAINS ONE OCTET ==
;; A cell's haecceity is circumscribed by its capacity to hold a scalar
;; byte-valued entity, unsigned and thus restricted to the range
;; [0, 255]. A transgression along the lower boundary of zero (0)
;; redirects the value to the upper bound's 255; whereas in the athwart
;; extremity the maximum of 255, when exceeded, resorts to the minimum
;; zero (0).
;; 
;; == THE CURRENT CELL IS MARKED BY A POINTER ==
;; At the start of the program empight on the first cell, a dedicated
;; reference, known as the "cell pointer", or simply "pointer",
;; determines the currently active cell, the sole instance at any moment
;; of an !!Fuck program's execution being amenable to indagations and
;; modifications.
;; 
;; A motile entity, certain commands translate the cell pointer
;; sinistrally or dextrally by single steps. The memory's bilateral
;; infinite expansion debels further requisites for wrapping or
;; modifying the motion in additional ways.
;; 
;; 
;; Implementation
;; ==============
;; Despite Common Lisp's lacuna of sophisticated string replacement or
;; even regular expression facilities, the retrieval of adjacent
;; characters tallies among the less demanding operations.
;; 
;; == CONVERSION PROCESSES OPERATE ON CHARACTER STREAMS ==
;; For the sake of enhanced abstraction and potential for future
;; extensions from string to stream manipulations, the conversion in
;; both linguistic directions proceeds by a parasceve that converts the
;; character sequence into a stream form.
;; 
;; == AN ASSOCIATION LIST PROVIDES THE TRANSLATION TABLE ==
;; The transcription from !!Fuck commands to brainfuck equivalents, and
;; vice versa, is implemented in terms of an association list, or alist,
;; with the former's instruction set supplying the keys, mapping to the
;; single-character operations from the latter species. This dictionary
;; type's structure, linear in its nature, permits look-ups employing
;; both entry parts, thus being bilaterally steadable.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-12
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/!!Fuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each of which conforms to the ELEMENT-TYPE, defaulting to
   the comprehensive ``T''."
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

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, of zero or more entries, each member of which either
   constitutes the ``NIL'' value or a cons whose sinistral compartment
   conforms to the KEY-TYPE, associated with the VALUE-TYPE governing
   in the dextral moeity, with both specifiers defaulting to the
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
                  (typep element
                    `(or null (cons ,key-type ,value-type))))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE, associated
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of the locations of
   forward jump instructions inside of a piece of brainfuck code to
   matching back jump instruction positions, and vice versa, implemented
   in the form of a hash table which associates fixnum specifier keys to
   equally typed values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   thus occupying the range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as an association of
   arbitrary integer-valued cell indices with byte-valued cell values,
   manifesting as a hash table with integer keys mapping to ``octet''
   values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "A ``destination'' defines a sink for output operations, including,
   among others, the ``format'' and ``write-char'' functions."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of string character) +CODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +CODING-TABLE+
  '(("!!!!!#"        . #\>)
    ("!!!!!!#"       . #\<)
    ("!!!!!!!#"      . #\+)
    ("!!!!!!!!#"     . #\-)
    ("!!!!!!!!!#"    . #\,)
    ("!!!!!!!!!!#"   . #\.)
    ("!!!!!!!!!!!#"  . #\[)
    ("!!!!!!!!!!!!#" . #\]))
  "An association list which maps each !!Fuck instruction to the
   corresponding brainfuck equivalent.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of !!Fuck-to-brainfuck converter.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-brainfuck-command (!!Fuck-command)
  "Returns the brainfuck command associated with the !!FUCK-COMMAND
   or ``NIL'', if no such mapping exists."
  (declare (type string !!Fuck-command))
  (the character
    (or (cdr (assoc !!Fuck-command +CODING-TABLE+ :test #'string=))
        (error "The !!Fuck token ~s does not correlate to any ~
                brainfuck instruction."
          !!Fuck-command))))

;;; -------------------------------------------------------

(defun read-!!Fuck-token (source)
  "Reads from the SOURCE an !!Fuck command as a sequence of one or more
   exclamation marks ('!'), followed by one hash sign '#', and returns
   the !!Fuck token as a string; or signals an error if no such command
   can be constructed."
  (declare (type stream source))
  (let ((character (read-char source NIL NIL)))
    (declare (type (or null character) character))
    (the string
      (with-output-to-string (!!Fuck-command)
        (declare (type string-stream !!Fuck-command))
        ;; Read the exclamation marks ("!").
        (loop while (and character (char= character #\!)) do
          (write-char character !!Fuck-command)
          (setf character (read-char source NIL NIL)))
        ;; Read the concluding hash sign ("#").
        (if (char= character #\#)
          (write-char character !!Fuck-command)
          (error "Expected '#', but encountered ~s." character))))))

;;; -------------------------------------------------------

(defun convert-!!Fuck-to-brainfuck (!!Fuck-code
                                    &key (destination NIL))
  "Converts the !!FUCK-CODE to brainfuck code which is written to the
   DESTINATION, this sink defaulting to the standard output."
  (declare (type string      !!Fuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (with-input-from-string (source !!Fuck-code)
        (declare (type string-stream source))
        (let ((character NIL))
          (declare (type (or null character) character))
          (flet
              ((peek-next-character ()
                "Returns without removing the next character from the
                 SOURCE stream, or ``NIL'' upon the same's exhaustion."
                (the (or null character)
                  (peek-char NIL source NIL NIL NIL)))
               (read-next-character ()
                "Reads the next character from the SOURCE and stores in the
                 variable CHARACTER, returning no value."
                (setf character (read-char source NIL NIL NIL))
                (values)))
            (loop do
              (case (peek-next-character)
                ((NIL)
                  (loop-finish))
                (#\!
                  (let ((brainfuck-command
                          (get-brainfuck-command
                            (read-!!Fuck-token source))))
                    (declare (type character brainfuck-command))
                    (write-char brainfuck-command destination)))
                (otherwise
                  (read-next-character)))))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-!!Fuck-to-brainfuck !!Fuck-code
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-!!Fuck converter.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (token)
  "Checks whether the TOKEN represents a brainfuck command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (rassoc token +CODING-TABLE+ :test #'char=)))))

;;; -------------------------------------------------------

(defun get-!!Fuck-command (brainfuck-command)
  "Returns the !!Fuck command associated with the BRAINFUCK-COMMAND or
   ``NIL'', if no such mapping exists."
  (declare (type character brainfuck-command))
  (the string
    (or (car (rassoc brainfuck-command +CODING-TABLE+ :test #'char=))
        (error "The brainfuck token ~s does not correlate to any ~
                !!Fuck instruction."
          brainfuck-command))))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-!!Fuck (brainfuck-code
                                    &key (destination NIL)
                                         (command-separator NIL))
  "Converts the BRAINFUCK-CODE to !!Fuck code which is written to the
   DESTINATION, this sink defaulting to the standard output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (declare (type T           command-separator))
  (the (or null string)
    (if destination
      (with-input-from-string (source brainfuck-code)
        (declare (type string-stream source))
        (loop
          with first-command-p
            of-type boolean
            =       T
          for brainfuck-token
            of-type (or null character)
            =       (read-char source NIL NIL)
          while brainfuck-token
          do
            (when (brainfuck-command-p brainfuck-token)
              (cond
                (first-command-p
                  (setf first-command-p NIL))
                (T
                  (when command-separator
                    (format destination "~a" command-separator))))
              (format destination
                (get-!!Fuck-command brainfuck-token)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-!!Fuck brainfuck-code
          :destination       output
          :command-separator command-separator)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (brainfuck-code)
  "Creates and returns a jump table for the piece of BRAINFUCK-CODE
   which associates each forward jump instruction's location in the same
   with the matching back jump position, and vice versa."
  (declare (type string brainfuck-code))
  (let ((jump-table             (make-hash-table :test #'eql))
        (forward-jump-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-positions))
    (loop
      for token    of-type character across brainfuck-code
      and position of-type fixnum    from   0 by 1
      do
        (case token
          (#\[
            (push position forward-jump-positions))
          (#\]
            (cond
              (forward-jump-positions
                (let ((forward-jump-position
                        (pop forward-jump-positions))
                      (back-jump-position position))
                  (declare (type fixnum forward-jump-position))
                  (declare (type fixnum back-jump-position))
                  (setf (gethash forward-jump-position jump-table)
                        back-jump-position)
                  (setf (gethash back-jump-position jump-table)
                        forward-jump-position)))
              (T
                (error "Unterminated back jump instruction at ~
                        position ~d."
                  position))))
          (otherwise
            NIL)))
    
    (when forward-jump-positions
      (error "Unmatched forward jump instructions at ~
              positions ~{~d~^, ~}."
        forward-jump-positions))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun interpret-brainfuck (brainfuck-code)
  "Interprets the piece of BRAINFUCK-CODE and returns no value."
  (declare (type string brainfuck-code))
  (when (plusp (length brainfuck-code))
    (let ((ip         0)
          (token      (char brainfuck-code 0))
          (jump-table (build-jump-table brainfuck-code))
          (memory     (make-hash-table :test #'eql))
          (pointer    0))
      (declare (type fixnum              ip))
      (declare (type (or null character) token))
      (declare (type jump-table          jump-table))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the BRAINFUCK-CODE, if possible, updates the current TOKEN,
             and returns no value."
            (setf token
              (when (array-in-bounds-p brainfuck-code (1+ ip))
                (char brainfuck-code (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expecting it to reside at a jump instruction, relocates the
             instruction pointer IP to the jump boundary opposite to the
             current position, updates the current TOKEN, and returns no
             value."
            (setf ip
              (or (gethash ip jump-table)
                  (error "No jump instruction associated with ~
                          position ~d."
                    ip)))
            (setf token
              (when (array-in-bounds-p brainfuck-code ip)
                (char brainfuck-code ip)))
            (values))
           
           (current-cell ()
            "Returns the MEMORY cell value located under the cell
             POINTER."
            (the octet
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Sets the content of the MEMORY cell residing under the cell
             POINTER to the NEW-VALUE and returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0)
                  (mod new-value 256))
            (values)))
        
        (loop while token do
          (case token
            ((NIL)
              (loop-finish))
            
            ;; Move cell pointer one step to the right.
            (#\>
              (incf pointer))
            
            ;; Move cell pointer one step to the left.
            (#\<
              (decf pointer))
            
            ;; Increment current cell value by one.
            (#\+
              (incf (current-cell)))
            
            ;; Decrement current cell value by one.
            (#\-
              (decf (current-cell)))
            
            ;; Query user input and store it in the current cell.
            (#\,
              (format T "~&Please enter an ASCII character: ")
              (setf (current-cell)
                    (char-code (read-char)))
              (clear-input))
            
            ;; Print the current cell's ASCII character value.
            (#\.
              (write-char (code-char (current-cell))))
            
            ;; Jump forward to "]" if current cell contains zero.
            (#\[
              (when (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            ;; Jump back to "[" if current cell does not contain zero.
            (#\]
              (unless (zerop (current-cell))
                (jump-to-opposite-boundary)))
            
            ;; Skip commentary content.
            (otherwise
              NIL))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-!!Fuck (!!Fuck-code)
  "Interprets the piece of !!FUCK-CODE and returns no value."
  (declare (type string !!Fuck-code))
  (interpret-brainfuck
    (convert-!!Fuck-to-brainfuck !!Fuck-code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program, equivalent to brainfuck's
;;   ,.[,.]
(interpret-!!Fuck
  "!!!!!!!!!#
   !!!!!!!!!!#
   !!!!!!!!!!!#
   !!!!!!!!!#
   !!!!!!!!!!#
   !!!!!!!!!!!!#")

;;; -------------------------------------------------------

;; Print the message "hello world".
(interpret-!!Fuck
  "!!!!!!!#!!!!!!!!!!!#!!!!!!!!#!!!!!!!!!!!#!!!!!!#!!!!!!#!!!!!!!!!!!#!!!!!!!#!!!!!!!!!!!#!!!!!!!!#!!!!!!!!#!!!!!!!!#!!!!!#!!!!!!!!!!!!#!!!!!!!!#!!!!!!!!!!!#!!!!!!#!!!!!!#!!!!!!#!!!!!!!!!!!!#!!!!!!!!!!!!#!!!!!!!!!!!!#!!!!!#!!!!!#!!!!!#!!!!!!!!#!!!!!!!!!!!!#!!!!!#!!!!!!!!#!!!!!!!!!!#!!!!!!!!#!!!!!!!!#!!!!!!!!#!!!!!!!!!!#!!!!!#!!!!!!!!!!#!!!!!!!!!!#!!!!!#!!!!!!!!!!#!!!!!!#!!!!!!#!!!!!!#!!!!!!#!!!!!!!!#!!!!!!!!!!#!!!!!!#!!!!!!!#!!!!!!!!!!#!!!!!#!!!!!#!!!!!#!!!!!#!!!!!#!!!!!!!!!!#!!!!!#!!!!!!!!!!#!!!!!!#!!!!!!#!!!!!!!!!!#!!!!!!#!!!!!!!!#!!!!!!!!!!#")

;;; -------------------------------------------------------

(interpret-!!Fuck
  "
  !!!!!!!#
  !!!!!!!!!!!#
  !!!!!!!!#
  !!!!!!!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!!!!!!#
  !!!!!!!#
  !!!!!!!!!!!#
  !!!!!!!!#
  !!!!!!!!#
  !!!!!!!!#
  !!!!!#
  !!!!!!!!!!!!#
  !!!!!!!!#
  !!!!!!!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!!!!!!!#
  !!!!!!!!!!!!#
  !!!!!!!!!!!!#
  !!!!!#
  !!!!!#
  !!!!!#
  !!!!!!!!#
  !!!!!!!!!!!!#
  !!!!!#
  !!!!!!!!#
  !!!!!!!!!!#
  !!!!!!!!#
  !!!!!!!!#
  !!!!!!!!#
  !!!!!!!!!!#
  !!!!!#
  !!!!!!!!!!#
  !!!!!!!!!!#
  !!!!!#
  !!!!!!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!!!#
  !!!!!!!!!!#
  !!!!!!#
  !!!!!!!#
  !!!!!!!!!!#
  !!!!!#
  !!!!!#
  !!!!!#
  !!!!!#
  !!!!!#
  !!!!!!!!!!#
  !!!!!#
  !!!!!!!!!!#
  !!!!!!#
  !!!!!!#
  !!!!!!!!!!#
  !!!!!!#
  !!!!!!!!#
  !!!!!!!!!!#
  ")
