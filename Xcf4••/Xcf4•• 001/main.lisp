;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Xcf4••", invented by the Esolang user "Cinnamony" and
;; presented on June 20th, 2023, intended as a variation on Urban
;; Mueller's "brainfuck", attending to a reformulation of the octuple
;; instruction set by jumelles of "☺", "☻", and "π" symbols, and the
;; induction of a cell value transfer facility.
;; 
;; 
;; Instructions
;; ============
;; A fidel to its brainfuck cleronomy, Xcf4•• appropriates the
;; conceptual capabilities from its octuple original, the provenance,
;; natheless, enjoying an advenient operation for moving the current
;; cell's value to its dextral neighbor.
;; 
;; All operations are expressed in a twain of characters --- an indicium
;; acting as a sepiment to brainfuck's aefauld identifiers.
;; 
;; == OVERVIEW ==
;; An apercu shall attend to the reader's apprehension of Xcf4••'s
;; competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   π☻      | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   π☺      | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   ☺☺      | Increments the current cell value by one.
;;           | If the new value transgresses the upper march of 255,
;;           | it is modified to resort to the minimum of zero (0).
;;   ..................................................................
;;   ☻☻      | Decrements the current cell value by one.
;;           | If the new value transgresses the lower march of zero
;;           | (0), it is modified to resort to the maximum of 255.
;;   ..................................................................
;;   ☺☻      | Queries the standard input for an ASCII character and
;;           | stores its character code in the current cell.
;;   ..................................................................
;;   ππ      | Prints the character whose ASCII code corresponds to the
;;           | current cell to the standard output.
;;   ..................................................................
;;   ☺π      | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "☻π". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ☻π      | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "☺π". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ☻☺      | Increments the current cell's dextral neighbor by the
;;           | value stored in the current cell, while concomitantly
;;           | decrementing the latter to zero (0).
;;   ------------------------------------------------------------------
;; 
;; == Xcf4•• AND BRAINFUCK ==
;; The cognate vinculum betwixt Xcf4•• and brainfuck capacitates an
;; equiparation of their compatible instruction set:
;; 
;;   ------------------
;;   Xcf4•• | brainfuck
;;   -------+----------
;;   π☻     | >
;;   ..................
;;   π☺     | <
;;   ..................
;;   ☺☺     | +
;;   ..................
;;   ☻☻     | -
;;   ..................
;;   ☺☻     | ,
;;   ..................
;;   ππ     | .
;;   ..................
;;   ☺π     | [
;;   ..................
;;   ☻π     | ]
;;   ..................
;;   ☻☺     | [>+<-]
;;   ------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in Common Lisp, employing for
;; epideictic purposes special variables as surrogates for classes and
;; parameters to functions.
;; 
;; == SPECIAL VARIABLES: A BÊTE NOIRE ==
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; It constitutes a peisant element of gnarity to remember that special
;; variables, ligated into a consanguinity with global variables as a
;; general species, and exacerbated by their implicit and contingently
;; arbitrary declarations, merit the wite of encumbering programs with
;; superfluous complexity. For a more detailed treatise on the
;; contingency for detriments incurred by this feature, their repute as
;; bête noires' etiology, please refer to [stackoverflow2019q56725814].
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-23
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Xcf4••]
;;   The Esolang contributors, "Xcf4••", June 20th, 2023
;;   URL: "https://esolangs.org/wiki/Xcf4%E2%80%A2%E2%80%A2"
;;   
;;   [stackoverflow2012q41091118]
;;   The Stack Overflow contributors,
;;     "What's the canonical way to join strings in a list?", 2012
;;   URL: "https://stackoverflow.com/a/41091118"
;;   Notes:
;;     - Demonstrates the usance of special variables in the context of
;;       the ``format'' function.
;;   
;;   [stackoverflow2019q56725814]
;;   The Stack Overflow contributors, "Using Local Special Variables",
;;     2019
;;   URL: "https://stackoverflow.com/questions/56725814/
;;         using-local-special-variables"
;;   Notes:
;;     - Discusses the disadvantages of special variables, which
;;       comprehend:
;;        o Lack of referential transparency, ...
;;          ... which renders it more difficult to reason functionally
;;          about one's code, meaning that functions may produce
;;          different results with syntactically equivalent calls.
;;        o Introduction of bugs, ...
;;          ... as lexical variable at other locations in the code,
;;          e.g. in a system function, will be overwritten.
;;        o Confusion ...
;;          .. for readers unacquainted with special (dynamic) binding
;;        o Dubious necessity, ...
;;          ... as lexical binding or even anaphoric macros may be
;;          utilized instead.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, the keys of which conform to the KEY-TYPE and associate
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

(deftype command ()
  "The ``command'' type enumerates the recognized Xcf4•• commands, an
   augmentation attends to the no-operation sentinel ``:nop''."
  '(member
    :increment
    :decrement
    :output
    :move-left
    :move-right
    :jump-forward
    :jump-back
    :input
    :transfer-value
    :nop))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump positions
   to back jump locations in a command vector, the incarnation applied
   to the same assuming the guise of a hash table that associates fixnum
   keys to fixnum values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   and thus a commorant in the integer range [0, 255], yet extended to
   amplect the values -1 and 256 in order to accommodate the rigor
   incarnated in Common Lisp's type system during certain operations."
  '(or (unsigned-byte 8)
       (integer -1 256)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory in terms of a hash
   table that maps signed integer cell indices to octet values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output oeprations, such
   as ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-next-token ()
  "Returns the next token from the CODE, composed of the next zero to
   two characters starting from the INDEX."
  (declare (special code))
  (declare (special index))
  (the string
    (subseq code index
      (min (+ index 2)
           (length code)))))

;;; -------------------------------------------------------

(defun get-command (token)
  "Returns the command associated with the TOKEN, which might comprehend
   the ``:nop'' sentinel upon its disrespondency."
  (declare (type string token))
  (the command
    (cond
      ((string= token "☺☺") :increment)
      ((string= token "☻☻") :decrement)
      ((string= token "ππ") :output)
      ((string= token "π☺") :move-left)
      ((string= token "π☻") :move-right)
      ((string= token "☺π") :jump-forward)
      ((string= token "☻π") :jump-back)
      ((string= token "☺☻") :input)
      ((string= token "☻☺") :transfer-value)
      (T                    :nop))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-command-vector ()
  "Sets the global COMMANDS vector to an empty instance and returns the
   same."
  (declare (special commands))
  (setf commands
    (make-array 0
      :element-type    'command
      :initial-element :nop
      :adjustable      T
      :fill-pointer    0))
  (the (vector command *) commands))

;;; -------------------------------------------------------

(defun extract-commands ()
  "Proceeding from the INDEX into the CODE, extracts the entailed
   commands and inserts these into the COMMANDS vector, finally
   returning the same."
  (declare (special code index commands))
  (loop
    while (< index (length code))
    for token   of-type string  = (get-next-token)
    for command of-type command = (get-command token)
    if (eq command :eof) do
      (incf index)
    else do
      (vector-push-extend command commands)
      (incf index 2))
  (the (vector command *) commands))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-jump-table ()
  "Initializes the global JUMP-TABLE to an empty instance and returns
   the same."
  (declare (special jump-table))
  (setf jump-table
    (make-hash-table :test #'eql))
  (the jump-table jump-table))

;;; -------------------------------------------------------

(defun build-jump-table ()
  "Populates the JUMP-TABLE with the COMMANDS' jump points and returns
   the modified JUMP-TABLE."
  (declare (special jump-table))
  (declare (special commands))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type command across commands
      and position of-type fixnum  from   0 by 1
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched back jump point at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points))))
  (the jump-table jump-table))

;;; -------------------------------------------------------

(defun jump ()
  "Expecting the instruction pointer IP to currently reside at a jump
   forward or back command in the COMMANDS vector, relocates the IP to
   the opposite boundary and returns no value.
   ---
   An error of an unspecified type transpires upon the current IP
   position's disrespondency in the JUMP-TABLE."
  (declare (special jump-table))
  (declare (special ip))
  (setf ip
    (or (gethash ip jump-table)
        (error "No jump target associated with the position ~d." ip)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-memory ()
  "Initializes the global MEMORY to an empty instance and returns the
   same."
  (declare (special memory))
  (setf memory
    (make-hash-table :test #'eql))
  (the memory memory))

;;; -------------------------------------------------------

(defun cell-at-pointer ()
  "Returns the byte stored at the CELL-POINTER index in the MEMORY."
  (declare (special memory))
  (declare (special cell-pointer))
  (the octet
    (gethash cell-pointer memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-at-pointer) (new-value)
  "Stores the NEW-VALUE in the MEMORY cell at the CELL-POINTER,
   contingently preceded by a wrapping into the byte range [0, 255], and
   returns the ultimately stored value."
  (declare (special      memory))
  (declare (special      cell-pointer))
  (declare (type integer new-value))
  (setf (gethash cell-pointer memory 0)
        (mod new-value 256))
  (the octet
    (gethash cell-pointer memory 0)))

;;; -------------------------------------------------------

(define-symbol-macro current-cell
  (the octet
    (cell-at-pointer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-commands ()
  "Processes the global COMMANDS vector and returns no value."
  (declare (special commands))
  (declare (special ip))
  (declare (special jump-table))
  (declare (special memory))
  (declare (special cell-pointer))
  
  (loop while (< ip (length commands)) do
    (case (aref commands ip)
      (:nop
        (incf ip))
      
      (:increment
        (incf current-cell))
      
      (:decrement
        (decf current-cell))
      
      (:move-right
        (incf cell-pointer))
      
      (:move-left
        (decf cell-pointer))
      
      (:input
        (format *standard-output* "~&>> ")
        (force-output *standard-output*)
        (setf current-cell
          (char-code
            (read-char *standard-input* NIL #\Null)))
        (clear-input *standard-input*))
      
      (:output
        (write-char
          (code-char current-cell)))
      
      (:jump-forward
        (when (zerop current-cell)
          (jump)))
      
      (:jump-back
        (unless (zerop current-cell)
          (jump)))
      
      (:transfer-value
        (loop until (zerop current-cell) do
          (incf cell-pointer)
          (incf current-cell)
          (decf cell-pointer)
          (decf current-cell)))
      
      (otherwise
        (error "Invalid command ~s at position ~d."
          (aref commands ip)
          ip)))
    
    (incf ip))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Xcf4•• (code)
  "Interprets the piece of Xcf4•• source CODE and returns no value."
  (declare (special     code))
  (declare (type string code))
  (let ((index    0)
        (commands (initialize-command-vector)))
    (declare (special                 code))
    (declare (type string             code))
    (declare (special                 index))
    (declare (type fixnum             index))
    (declare (special                 commands))
    (declare (type (vector command *) commands))
    (extract-commands)
    
    (let ((jump-table (initialize-jump-table)))
      (declare (special         jump-table))
      (declare (type hash-table jump-table))
      (build-jump-table)
      
      (let ((ip           0)
            (memory       (initialize-memory))
            (cell-pointer 0))
        (declare (special      ip))
        (declare (type fixnum  ip))
        (declare (special      memory))
        (declare (type memory  memory))
        (declare (special      cell-pointer))
        (declare (type integer cell-pointer))
        (process-commands))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converters.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-Xcf4•• (brainfuck-code
                                    &key (destination NIL))
  "Converts the piece of BRAINFUCK-CODE into an equivalent Xcf4••
   program and writes the resulting code to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending thr output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for bf-token of-type character across brainfuck-code do
        (format destination "~a"
          (case bf-token
            (#\+       "☺☺")
            (#\-       "☻☻")
            (#\.       "ππ")
            (#\<       "π☺")
            (#\>       "π☻")
            (#\[       "☺π")
            (#\]       "☻π")
            (#\,       "☺☻")
            (otherwise ""))))
      (with-output-to-string (xcf4••-code)
        (declare (type string-stream xcf4••-code))
        (convert-brainfuck-to-Xcf4•• brainfuck-code
          :destination xcf4••-code)))))

;;; -------------------------------------------------------

(defun convert-Xcf4••-to-brainfuck (xcf4••-code
                                    &key (destination NIL))
  "Converts the piece of XCF4••-CODE into an equivalent brainfuck
   program and writes the resulting code to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending thr output."
  (declare (type string      xcf4••-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((position 0))
        (declare (type fixnum position))
        (loop
          while (< position (length xcf4••-code))
          for token
            of-type string
            =       (subseq xcf4••-code position
                      (min (+ position 2)
                           (length xcf4••-code)))
          for command
            of-type command
            =       (get-command token)
          do
            (format destination "~a"
              (case command
                (:move-right     ">")
                (:move-left      "<")
                (:increment      "+")
                (:decrement      "-")
                (:input          ",")
                (:output         ".")
                (:jump-forward   "[")
                (:jump-back      "]")
                (:transfer-value "[>+<-]")
                (:nop            "")
                (otherwise
                  (error "Invalid token at position ~d: ~s."
                    token position))))
            (if (eq command :nop)
              (incf position 1)
              (incf position 2))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (convert-Xcf4••-to-brainfuck xcf4••-code
          :destination brainfuck-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ASCII cycle.
(interpret-Xcf4•• "☺☺☺πππ☺☺☻π")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Xcf4•• "☺☻ππ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Xcf4•• "☺☻ππ☺π☻☻☻☻π☻☺☺☺ππ☻π☻☻ππ☺☺πππ☻ππ☺π☺☻π")
