;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language ".:iI1l!¡", conceived by the Esolang user "Username1234" and
;; presented on August 30th, 2022, its haecceity's preponderance issuing
;; from the Urban Mueller's "brainfuck" programming language's dation,
;; ostending a discrepancy, besides the single-character instructions'
;; reformulation in symbols resembling dots and bars, by the expression
;; of its comments, the same, as counterdistinguished from the
;; stock-father's liberality at any location, requires their demarcation
;; via the "¡" entity.
;; 
;; 
;; Concept
;; =======
;; The .:iI1l!¡ language's provenance is constituted by a new set of
;; symbols' application unto brainfuck, in all further aspects, except
;; for its more stringent handling of comments, a perfect legatee.
;; 
;; == ".:iI1l!¡": AN AGNOMINATION WHICH REPLICATES ITS INSTRUCTIONS ==
;; The language kenspeckle proprium is expressed in its own
;; agnomination, concomitantly an enumeration of most of its deployed
;; instruction identifiers.
;; 
;; == THE MEMORY: A BILATERALLY INFINITE EXPANSE OF UNSIGNED BYTES ==
;; The propagation of brainfuck's influence perpetuates in .:iI1l!¡'s
;; architecture, its reification a bilaterally infinite dispansion of
;; unsigned byte-valued cells, each such an aefauld integer's salvatory,
;; imposing a closed range of [0, 255] which, upon any of its bourne
;; twissel's infringement, wraps around to the obverse march. Every cell
;; assumes at the program's inchoation the default value of zero (0).
;; 
;; A mobile cell pointer selects at any instant the active cell, the
;; sole instance entalented with an amenability to perquisitions and
;; modifications.
;; 
;; 
;; Syntax
;; ======
;; The .:iI1l!¡ programming language's obtention of a preponderance
;; among its facilities capacitates an eath ilk of treatise's adhibition
;; to its syntactical aspect, employing single-character identifier for
;; its nonuplet instructions.
;; 
;; == WHITESPACES ==
;; Whitespaces are encountered with leniency in their spatial and
;; quantitative measurement.
;; 
;; == COMMENT ==
;; In regards of its effective construe the most conspicable item of
;; disparateness from its cleronomy, .:iI1l!¡ mandates its comments'
;; introduction or amplectation by the "¡" symbol.
;; 
;; The occurrence of a "¡" inchoates such an apostille, which either
;; terminates with a subsequent instance of the same, or the source
;; code's exhaustion. The former proprium excludes the contingency for
;; nested comments.
;; 
;; == GRAMMAR ==
;; A formal expression of the language's donet shall be the following
;; Extended Backus-Naur Form's (EBNF) cynosure:
;; 
;;   program        := { command | limitedComment | whitespaces } ;
;;                  ,  [ tailComment ] ;
;;   command        := "." | ":" | "i" | "I" | "|" | "!" | "l" | "1" ;
;;   limitedComment := "¡" , { character - "¡" } , "¡" ;
;;   tailComment    := "¡" , { character - "¡" } ;
;;   whitespaces    := whitespace , { whitespace } ;
;;   whitespace     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The .:iI1l!¡ language employs a nonuplet cardinality of operations,
;; enumerating among these the comment dedication, the competences
;; ceteris paribus subscribe to an equiparation with brainfuck.
;; 
;; == OVERVIEW ==
;; .:iI1l!¡'s nonuplet instruction set shall be 
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   .       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   :       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   i       | Increments the current cell's value by one.
;;           | If the new value exceeds the upper bourne of 255, the
;;           | state relapses to the lower extremum of zero (0).
;;   ..................................................................
;;   I       | Decrements the current cell's value by one.
;;           | If the new value descends below the minimum of zero (0),
;;           | the state wraps around to the upper extremum of 255.
;;   ..................................................................
;;   |       | Prints to the standard output the character whose ASCII
;;           | code matches the current cell's value.
;;   ..................................................................
;;   !       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;   ..................................................................
;;   l       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "1" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   1       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "l" command.
;;           | Otherwise proceeds as usual.
;;   ..................................................................
;;   ¡       | Introduces a comment section which extends either to the
;;           | next "¡" token, or, alternatively, the end of the file.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp.
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
;; Date:   2023-12-10
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023.:iI1l!¡]
;;   The Esolang contributors, ".:iI1l!¡", September 21st, 2023
;;   URL: "https://esolangs.org/wiki/.:iI1l!%C2%A1"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-variable &rest lambda-list)
     &body BODY)
  "Defines a new derived type utilizing the ``deftype'' infrastructure
   in conjunction with the ``satisfies'' type specifier, begetting by
   their coefficient adminiculum a new species nevened by the TYPE-NAME,
   its parameters supplied by the optional LAMBDA-LIST, while
   concomitantly assigning to the probed object the CANDIDATE-VARIABLE
   as a designator, accessible by the BODY forms, whose desinent
   specimen ought to return a generalized Boolean value, amounting to a
   non-``NIL'' response if the subject identified via the
   CANDIDATE-VARIABLE subsumes into this type, otherwise answering with
   ``NIL''.
   ---
   Please heed that the first BODY form, if evaluating to a string,
   experiences an interpretation as the new type's documentation string,
   for which causatum it is reappropriated."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   '*)
                                              (value-type '*))
  "The ``hash-table-of'' type defines a hash table, the entries of which
   establish composition of a key adhering to the KEY-TYPE and a value
   subsumed into the VALUE-TYPE, both species defaulting to the generic
   ``*'' sentinel."
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
             (typep value value-type)))))

;;; -------------------------------------------------------

(define-bespoke-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list compact of zero or more elements,
   each member of which is desumed from the ELEMENT-TYPE, its default
   resolving to the generic sentinel ``*''."
  (and
    (listp candidate)
    (flet ((element-type-matches-p (probed-element)
            "Determines whether the PROBED-ELEMENT matches the
             ELEMENT-TYPE, returning on confirmation a ``boolean'' value
             of ``T'', otherwise ``NIL''."
            (declare (type T probed-element))
            (the boolean
              (not (null
                (typep probed-element element-type))))))
      (every #'element-type-matches-p
        (the list candidate)))))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized members of .:iI1l!¡
   operations."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :jump-forward
    :jump-back
    :output
    :input))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a .:iI1l!¡ program as a vector of zero
   or more ``command'' objects."
  '(vector command *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type an association of forward and back jump
   commands in a .:iI1l!¡ program by mediation of their locations inside
   of the instruction sequence, realized as a hash table whose keys and
   value both assume the fixnum position specifications."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, thus commorant in the closed integral range of
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the .:iI1l!¡ program memory as a sparse
   vector of unsigned byte-valued cells, unbridled along both axes in
   their tally, and represented by a hash table whose keys impose the
   cell indices, associating with the ``octet'' cell values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns commands entailed in the piece of .:iI1l!¡
   source CODE as a ``program''."
  (declare (type string code))
  (let ((position 0))
    (declare (type fixnum position))
    (symbol-macrolet
        ((has-next-token-p
          (the boolean
            (not (null
              (array-in-bounds-p code position)))))
         (current-token
          (the character
            (char code position))))
      (declare (type boolean   has-next-token-p))
      (declare (type character current-token))
      (flet
          ((skip-comment ()
            "Skips a sequence of zero or more characters until either
             the CODE Is exhausted or the comment sentinel \"¡\" is
             encountered, and returns no value."
            (loop
              while (and has-next-token-p (char/= current-token #\¡))
              do    (incf position))
            (values)))
        (the program
          (coerce
            (loop while has-next-token-p append
              (prog1
                (case current-token
                  (#\. (list :move-right))
                  (#\: (list :move-left))
                  (#\i (list :increment))
                  (#\I (list :decrement))
                  (#\l (list :jump-forward))
                  (#\1 (list :jump-back))
                  (#\| (list :output))
                  (#\! (list :input))
                  (#\¡ (incf position)
                       (skip-comment)
                       NIL)
                  ((#\Newline #\Space #\Tab) NIL)
                  (otherwise
                    (error "Invalid character \"~c\" at position ~d."
                      current-token position)))
                (incf position)))
            '(simple-array command (*))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the START-POINT and the END-POINT in a bilateral fashion in
   the JUMP-TABLE and returns no value."
  (declare (type jump-table jump-table))
  (setf (gethash start-point jump-table) end-point)
  (setf (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table (program)
  "Generates and returns for the .:iI1l!¡ PROGRAM a jump table, the same
   serves to affiliate each forward jump command with its matching back
   jump counterpart, and vice versa, delegated in their representation
   to their positions in the PROGRAM."
  (declare (type program program))
  (let ((jump-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    
    (loop
      for command  of-type command across program
      and position of-type fixnum  from   0 by 1
      
      if (eq command :jump-forward) do
        (push position start-points)
      else if (eq command :jump-back) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            position)
          (error "Unterminated back jump point at position ~d."
            position))
      end
      
      finally
        (when start-points
          (error "Unterminated forward jump point~p at position ~:p ~
                  ~{~d~^, ~}."
            (length start-points)
            start-points)))
    
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table start-point)
  "Returns the obverse position associated in the JUMP-TABLE with the
   START-POINT, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (the fixnum
    (or (gethash start-point jump-table)
        (error "No jump destination associated with the position ~d."
          start-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns a new ``memory'' instance."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun memory-cell-at (memory index)
  "Returns the value of the MEMORY cell located amenable to the INDEX."
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the INDEX and
   returns the NEW-VALUE."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (setf (gethash index memory 0)
          (mod new-value 256))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (program)
  "Executes the .:iI1l!¡ PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip           0)
        (jump-table   (build-jump-table program))
        (memory       (make-memory))
        (cell-pointer 0))
    (declare (type fixnum ip))
    (declare (type jump-table jump-table))
    (declare (type memory     memory))
    (declare (type integer    cell-pointer))
    (symbol-macrolet
        ((execution-completed-p
          (the boolean
            (not (null
              (>= ip (length program))))))
         (current-command
          (the command
            (aref program ip)))
         (current-cell
          (the (or octet integer)
            (memory-cell-at memory cell-pointer))))
      (declare (type boolean            execution-completed-p))
      (declare (type command            current-command))
      (declare (type (or octet integer) current-cell))
      (loop until execution-completed-p do
        (case current-command
          (:move-right
            (incf cell-pointer))
          (:move-left
            (decf cell-pointer))
          (:increment
            (incf current-cell))
          (:decrement
            (decf current-cell))
          (:output
            (write-char
              (code-char current-cell)))
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf current-cell
              (char-code
                (read-char)))
            (clear-input))
          (:jump-forward
            (when (zerop current-cell)
              (setf ip
                (get-jump-destination jump-table ip))))
          (:jump-back
            (unless (zerop current-cell)
              (setf ip
                (get-jump-destination jump-table ip))))
          (otherwise
            (error "Invalid command ~s at position ~d."
              current-command ip)))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-|.:iI1l!¡| (code)
  "Interprets the piece of .:iI1l!¡ source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (extract-commands code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-.:iI1l!¡ converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-|.:iI1l!¡| (brainfuck-code
                                        &key (destination NIL))
  "Convert the BRAINFUCK-CODE to an equivalent .:iI1l!¡ program, writes
   the result to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string comprehending the program."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet
          ((collect-command (|.:iI1l!¡|-command)
            "Writes the |.:iI1l!¡|-COMMAND token to the DESTINATION and
             returns no value."
            (declare (type character |.:iI1l!¡|-command))
            (write-char |.:iI1l!¡|-command destination)
            (values)))
        (loop
          for brainfuck-token of-type character across brainfuck-code
          do  (case brainfuck-token
                (#\>       (collect-command #\.))
                (#\<       (collect-command #\:))
                (#\+       (collect-command #\i))
                (#\-       (collect-command #\I))
                (#\[       (collect-command #\l))
                (#\]       (collect-command #\1))
                (#\.       (collect-command #\|))
                (#\,       (collect-command #\!))
                (otherwise NIL))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-|.:iI1l!¡| brainfuck-code
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of .:iI1l!¡-to-brainfuck converter.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-|.:iI1l!¡|-to-brainfuck (|.:iI1l!¡|-code
                                        &key (destination NIL))
  "Converts the piece of |.:iI1l!¡|-CODE to brainfuck and writes the
   resulting program to the DESTINATION, returning for a non-``NIL''
   value ``NIL'', otherwise responding with a fresh string comprehending
   the output."
  (declare (type string      |.:iI1l!¡|-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet
          ((write-brainfuck-command (brainfuck-command)
            "Writes the BRAINFUCK-COMMAND to the DESTINATION and returns
             no value."
            (declare (type character brainfuck-command))
            (write-char brainfuck-command destination)
            (values)))
        (let ((|.:iI1l!¡|-program (extract-commands |.:iI1l!¡|-code)))
          (declare (type program |.:iI1l!¡|-program))
          (loop
            for |.:iI1l!¡|-command
              of-type command
              across  |.:iI1l!¡|-program
            for position
              of-type fixnum
              from    0
              by      1
            do
              (case |.:iI1l!¡|-command
                (:move-right   (write-brainfuck-command #\>))
                (:move-left    (write-brainfuck-command #\<))
                (:increment    (write-brainfuck-command #\+))
                (:decrement    (write-brainfuck-command #\-))
                (:output       (write-brainfuck-command #\.))
                (:input        (write-brainfuck-command #\,))
                (:jump-forward (write-brainfuck-command #\[))
                (:jump-back    (write-brainfuck-command #\]))
                (otherwise
                  (error "Invalid .:iI1l!¡ command ~s at position ~d."
                    |.:iI1l!¡|-command position))))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-|.:iI1l!¡|-to-brainfuck |.:iI1l!¡|-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-|.:iI1l!¡| "ilII.Il..i.IIIII::1:II:III1.I|...i|..||iiil|.1::::|iii|IIIIII|::I|....i|")

;;; -------------------------------------------------------

;; Move value.
(interpret-|.:iI1l!¡| "..lI1::lI..i::1")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-|.:iI1l!¡| "!l|!1")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-|.:iI1l!¡| "!|lII.il..1:l|1::1")

;;; -------------------------------------------------------

;; Convert a truth-machine from brainfuck to .:iI1l!¡ and return the
;; program as a string.
(convert-brainfuck-to-|.:iI1l!¡| ",.[-->+[>>]<[.]<<]")

;;; -------------------------------------------------------

;; Convert a truth-machine from .:iI1l!¡ to brainfuck and return the
;; program as a string.
(convert-|.:iI1l!¡|-to-brainfuck "!|lII.il..1:l|1::1")
