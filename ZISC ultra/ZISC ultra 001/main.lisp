;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ZISC ultra", invented by the Esolang user "Yourusername"
;; and presented on February 24th, 2024, endowed with such diorism as to
;; encode Urban Mueller's brainfuck in a sequence of percentage sign
;; ("%") and space (" ") occurrencies, the former symbols' acquisition
;; the procession through the entheus' octuple instruction set, the
;; latter the selection's activation.
;; 
;; 
;; Concept
;; =======
;; The ZISC ultra programming language's proprium is defined in terms of
;; two symbols champarty, the percentage sign ("%") and the space (" "),
;; which manipulate an "accumulator" by switching its state in a
;; wrapping fashion along the integral range of [0, 7] and selecting the
;; respective value, the same corresponds to one of the octuple
;; brainfuck instructions.
;; 
;; == THE ACCUMULATOR: A BRAINFUCK OPERATION SELECTOR ==
;; The ZISC ultra's central aspects is realized in the "accumulator", an
;; integral salvatory nuncupated to a scalar value's castaldy, and
;; confined to the closed interval [0, 7].
;; 
;; At the program's inchoation commorant on the minimum of zero (0),
;; each invocation of the command "%" increments the state by one; upon
;; its upper bourne's transgression, the same relapses to the lower
;; extremum.
;; 
;; Its confluency with brainfuck's octuple instruction set cardinality
;; does not impose a haphazardous fact, as the equinumerant contingency
;; in ZISC ultra corresponds in an unambiguous mode with the entheus'
;; capabilities. In a concrete diction, the following equiparation
;; governs the accumulator value and the encoded brainfuck operation:
;; 
;;   -----------------------------------------
;;   Accumulator state | brainfuck instruction
;;   ------------------+----------------------
;;   0                 | >
;;   .........................................
;;   1                 | <
;;   .........................................
;;   2                 | +
;;   .........................................
;;   3                 | -
;;   .........................................
;;   4                 | .
;;   .........................................
;;   5                 | ,
;;   .........................................
;;   6                 | [
;;   .........................................
;;   7                 | ]
;;   -----------------------------------------
;; 
;; An actual causatum's involvement proceeds, with respect to the
;; accumulator state, upon a space (" ") character's occurrency, whence
;; is the selected brainfuck command appended to the ZISC ultra command
;; sequence.
;; 
;; This catena of accumulator traversal and brainfuck instruction
;; acquisition serves in a stillatim generation of the encoded brainfuck
;; program's entirety.
;; 
;; 
;; Instructions
;; ============
;; An aspirant to its stock-father's competences, ZISC ultra's
;; instruction set enumerates the same octuple membership --- however,
;; expressed in a sequence of accumulator modulations which switch
;; through the available roster, ere selecting the desiderated specimen,
;; both being causata of a mere twissel of instructions.
;; 
;; == OVERVIEW: ZISC ULTRA OPERATIONS ==
;; The autochthonous aspect inherent to ZISC ultra tallies two
;; instructions only, the meager circumference shall be produced alow:
;; 
;;   ------------------------------------------------------------------
;;   ZISC command | Effect
;;   -------------+----------------------------------------------------
;;   %            | Increments the accumulator by one (1). If the new
;;                | state exceeds the upper bourne of eight (8), the
;;                | value relapses to the minimum of zero (0).
;;   ..................................................................
;;   (space)      | Invokes the brainfuck instruction corresponding to
;;                | the current accumulator state.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: BRAINFUCK OPERATIONS ==
;; The following purlicue's impartation shall be the equiparation and
;; elucidation of the eight available brainfuck instructions as a
;; corollary of the affiliated accumulator states.
;; 
;;   ------------------------------------------------------------------
;;   Accumulator | brainfuck | Effect
;;      state    |  command  | 
;;   ------------+-----------+-----------------------------------------
;;   0           | >         | Translates the cell pointer one step to
;;               |           | the right.
;;   ..................................................................
;;   1           | <         | Translates the cell pointer one step to
;;               |           | the left.
;;   ..................................................................
;;   2           | +         | Increments the current cell value by
;;               |           | one (1). If the new cell state exceeds
;;               |           | the upper bourne of 255, the value wraps
;;               |           | around to the minimum of zero (0).
;;   ..................................................................
;;   3           | -         | Decrements the current cell value by
;;               |           | one (1). If the new cell state descends
;;               |           | below the lower bourne of zero (0), the
;;               |           | value wraps around to the maximum of
;;               |           | 255.
;;   ..................................................................
;;   4           | .         | Prints the character whose ASCII code
;;               |           | corresponds to the current cell value to
;;               |           | the standard output.
;;   ..................................................................
;;   5           | ,         | Queries the standard input for a
;;               |           | character and stores its ASCII code in
;;               |           | the current cell.
;;   ..................................................................
;;   6           | [         | If the current cell value equals zero
;;               |           | (0), moves the instruction pointer (IP)
;;               |           | forward to the position immediately
;;               |           | succeeding the matching "]" instruction.
;;               |           | Otherwise proceeds as usual.
;;   ..................................................................
;;   7           | ]         | If the current cell value does not equal
;;               |           | zero (0), moves the instruction pointer
;;               |           | (IP) back to the position immediately
;;               |           | succeeding the matching "[" instruction.
;;               |           | Otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-07
;; 
;; Sources:
;;   [esolang2024ZISCultra]
;;   The Esolang contributors, "ZISC ultra", May 7th, 2024
;;   URL: "https://esolangs.org/wiki/ZISC_ultra"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among these complies to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, for both holds the
   comprehensive ``T'' as the default."
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

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member among which conforms to the ELEMENT-TYPE, for
   which holds the comprehensive ``T'' as the default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop for element of-type T in (the list candidate) always
              (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional correspondence of
   brainfuck jump forward and back instructions, mediated by adminiculum
   of their locations inside of the respective program and realized in a
   hash table of fixnum keys and fixnum values."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, and thus a commorant in the closed integral interval
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype byte-vector ()
  "The ``byte-vector'' type defines a sparse vector of unsigned byte
   cells, amenable to arbitrary signed integer subscripts and realized
   by mediation of a hash table whose integer keys answer to ``octet''
   values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype accumulator-state ()
  "The ``accumulator-state'' type defines a numeric value conformant
   with the ZISC ultra accumulator range, which resolves to an integer
   number in the closed interval [0, 7]."
  '(integer 0 7))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which enhalses, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of coding table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 8) +CODE-TABLE+))

;;; -------------------------------------------------------

(defparameter +CODE-TABLE+
  "><+-.,[]"
  "Associates the ZISC ultra and brainfuck instructions in a
   bidirectional mode by mediation of the latters' zero-based location
   inside of this string.")

;;; -------------------------------------------------------

(defun get-brainfuck-instruction (accumulator)
  "Returns the brainfuck instruction token associated with the ZISC
   ultra ACCUMULATOR state."
  (declare (type accumulator-state accumulator))
  (the character
    (schar +CODE-TABLE+ accumulator)))

;;; -------------------------------------------------------

(defun brainfuck-instruction-p (candidate)
  "Determines whether the CANDIDATE represents a member of the octuple
   brainfuck instruction set, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate +CODE-TABLE+ :test #'char=)))))

;;; -------------------------------------------------------

(defun get-ZISC-ultra-accumulator (brainfuck-instruction)
  "Returns the ZISC ultra accumulator state corresponding to the
   BRAINFUCK-INSTRUCTION token, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type character brainfuck-instruction))
  (the accumulator-state
    (or (position brainfuck-instruction +CODE-TABLE+ :test #'char=)
        (error "No corresponding ZISC ultra accumulator state ~
                can be detected for the brainfuck instruction ~c."
          brainfuck-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of accumulator operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun increment-accumulator (accumulator)
  "Increments the ACCUMULATOR's state, contingently wrapping around the
   upper bourne of seven (7), and returns the resultant state."
  (declare (type accumulator-state))
  (the accumulator-state
    (mod (1+ accumulator) 8)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ZISC-ultra-to-brainfuck converter.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-ZISC-ultra-to-brainfuck (zisc-ultra-code
                                        &optional (destination NIL))
  "Generates for the piece of ZISC-ULTRA-CODE an equivalent brainfuck
   program, prints the same to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the output."
  (declare (type string      zisc-ultra-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((accumulator 0))
        (declare (type accumulator-state accumulator))
        (loop
          for zisc-ultra-token
            of-type character
            across  zisc-ultra-code
          do
            (case zisc-ultra-token
              (#\%
                (setf accumulator
                  (increment-accumulator accumulator)))
              (#\Space
                (format destination "~c"
                  (get-brainfuck-instruction accumulator)))
              (otherwise
                NIL))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (convert-ZISC-ultra-to-brainfuck
          zisc-ultra-code
          brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-ZISC-ultra converter.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tally-required-accumulator-incrementations (start-value
                                                   targeted-value)
  "Returns the number of incrementation steps requisite to attain,
   proceeding from the accumulator's START-VALUE, the TARGETED-VALUE."
  (declare (type accumulator-state start-value))
  (declare (type accumulator-state targeted-value))
  (the (integer 0 7)
    (cond
      ((= start-value targeted-value)
        0)
      ((< start-value targeted-value)
        (- targeted-value start-value))
      ((> start-value targeted-value)
        (+ (- 8 start-value) targeted-value))
      (T
        (error "Cannot calculate the required incrementations betwixt ~
                the accumulator start value ~d and the targeted value ~
                ~d."
          start-value targeted-value)))))

;;; -------------------------------------------------------

(defun print-ZISC-coding-sequence (number-of-incrementations
                                   destination)
  "Prints to the DESTINATION a tally of incrementation instructions
   (\"%\") equinumerant to the NUMBER-OF-INCREMENTATIONS, succeeded by a
   single space (\" \") in order to accompass utility for the new
   accumulator state, and returns no value."
  (declare (type (integer 0 7) number-of-incrementations))
  (declare (type destination   destination))
  (format destination "~v@{~c~:*~} " number-of-incrementations #\%)
  (values))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-ZISC-ultra (brainfuck-code
                                        &optional (destination NIL))
  "Generates for the piece of BRAINFUCK-CODE an equivalent ZISC ultra
   program, prints the same to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((current-accumulator 0))
        (declare (type accumulator-state current-accumulator))
        (loop
          for brainfuck-token
            of-type character
            across  brainfuck-code
          when (brainfuck-instruction-p brainfuck-token) do
            (let ((new-accumulator
                    (get-ZISC-ultra-accumulator brainfuck-token)))
              (declare (type accumulator-state new-accumulator))
              (print-ZISC-coding-sequence
                (tally-required-accumulator-incrementations
                  current-accumulator
                  new-accumulator)
                destination)
              (setf current-accumulator new-accumulator))))
      (with-output-to-string (zisc-ultra-code)
        (declare (type string-stream zisc-ultra-code))
        (convert-brainfuck-to-ZISC-ultra
          brainfuck-code
          zisc-ultra-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (brainfuck-program)
  "Supputates and returns a jump table for the BRAINFUCK-PROGRAM, the
   same connects its corresponding jump points in a bilateral fashion by
   their positions' adminiculum."
  (declare (type string brainfuck-program))
  (let ((jump-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for instruction of-type character across brainfuck-program
      and position    of-type fixnum    from   0 by 1
      if (char= instruction #\[) do
        (push position start-points)
      else if (char= instruction #\]) do
        (if start-points
          (let ((start-point (pop start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf (gethash start-point jump-table) end-point
                   (gethash end-point   jump-table) start-point))
          (error "Unmatched jump back point at position ~d." position))
      end
      finally
        (when start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table departure-point)
  "Returns the obverse in the JUMP-TABLE associated with the
   DEPATURE-POINT, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     departure-point))
  (the fixnum
    (or (gethash departure-point jump-table)
        (error "No jump destination associated with the ~
                depature point ~d."
          departure-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cell-value-at (memory index)
  "Returns the octet value stored in the MEMORY cell amenable to the
   INDEX."
  (declare (type byte-vector memory))
  (declare (type integer     index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the INDEX,
   contingently preceded by a wrapping of its value in the pursuit to
   respect the valid byte range of [0, 255], and returns no value."
  (declare (type byte-vector memory))
  (declare (type integer     index))
  (setf (gethash index memory 0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainfuck (program)
  "Interprets the brainfuck PROGRAM and returns no value."
  (declare (type string program))
  (let ((ip           0)
        (jump-table   (build-jump-table program))
        (memory       (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type fixnum      ip))
    (declare (type jump-table  jump-table))
    (declare (type byte-vector memory))
    (declare (type integer     cell-pointer))
    (symbol-macrolet
        ((current-cell-value
          (the (or octet (eql -1))
            (cell-value-at memory cell-pointer)))
         (current-token
          (the character
            (char program ip)))
         (program-completed-p
          (the boolean
            (not (null
              (>= ip (length program)))))))
      (declare (type (or octet (eql -1)) current-cell-value))
      (declare (type character           current-token))
      (declare (type boolean             program-completed-p))
      (loop until program-completed-p do
        (case current-token
          (#\+
            (incf current-cell-value))
          (#\-
            (decf current-cell-value))
          (#\>
            (incf cell-pointer))
          (#\<
            (decf cell-pointer))
          (#\.
            (write-char
              (code-char current-cell-value)))
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf current-cell-value
              (char-code
                (read-char NIL NIL #\Null)))
            (clear-input))
          (#\[
            (when (zerop current-cell-value)
              (setf ip
                (get-jump-destination jump-table ip))))
          (#\]
            (unless (zerop current-cell-value)
              (setf ip
                (get-jump-destination jump-table ip))))
          (otherwise
            NIL))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-ZISC-ultra (program)
  "Interprets the ZISC ultra PROGRAM and returns no value."
  (declare (type string program))
  (interpret-brainfuck
    (convert-ZISC-ultra-to-brainfuck program))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-ZISC-ultra "%%%%% % %%%%%% % %% ")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-ZISC-ultra
  "%% %%%% %%%%%  %%%%% %%% %%% %%  %% %%%%%% %%%     %%%%%%  %%%%%% %% %%  %%%%%% %%   %%%% % %%% % %%%%   %% %% %%%%  %%%%  %%%%%%   %%%% %%%%%% %%%% %%%%%%% %%    %%% %%%%%%   %% %%%%%%%      % %%%%%  %% % %%%%    %% %% ")

;;; -------------------------------------------------------

;; Convert the cat program from ZISC ultra to its brainfuck paregal
;;   ,[.,]
(convert-ZISC-ultra-to-brainfuck "%%%%% % %%%%%% % %% ")

;;; -------------------------------------------------------

;; Convert the cat program from brainfuck to ZISC ultra, yielding
;;   %%%%% % %%%%%% % %% 
(convert-brainfuck-to-ZISC-ultra ",[.,]")
