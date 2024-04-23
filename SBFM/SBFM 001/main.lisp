;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language family "SBFM", abbreviating the complete name
;; "Strange brainfuck minimalizations", invented by the Esolang user
;; "ChuckEsoteric08" and presented on November 18th, 2033, being an
;; aggregate of a twain of programming languages, both variations on
;; Urban Mueller's brainfuck whose instruction set's curtailment from
;; eight to three operations maintains the company of a reduction
;; administered to the stock-father program memory's byte-valued cells
;; to such with binary capacity only.
;; 
;; 
;; Concept
;; =======
;; The SBFM family of programming languages, stevened by abbreviation of
;; the expanded agnomination "Strange brainfuck minimalizations",
;; provides a variation on the brainfuck language founded upon a
;; curtailment of the entheus octuple instruction set to a triad
;; membership, and its memory's reduction to bit-valued cells only.
;; 
;; == SBFM: A FAMILY OF TWO ==
;; The family's composition enumerates a twissel of relatives, norned
;; SBFM-1 and SBFM-2 respectively, to whom an equiparation in terms of
;; quantity in operations and any other bailiwick, except for the
;; command's concrete causata, is apportioned.
;; 
;; == THREE INSTRUCTIONS EXHAUST THE OPERATIONAL ASPECT ==
;; Both members embrace a quantity of three token admitted to
;; operational effects, while any other content's attendance does not
;; account for more than mere commentary purposes.
;; 
;; == THE MEMORY: A BILATERAL INFINITE TAPE OF BITS ==
;; The program memory, appropriated from brainfuck and modulated to the
;; particular encheson, constitutes a bilaterally infinite dispansion of
;; cells, each such a scalar bit's salvatory.
;; 
;; A cell pointer, initially empight on the first unit, operates on the
;; tape, selecting at any instant the currently active cell, such is
;; responsive to perquisitions and modifications. The fact involving the
;; pointer's mobile nature homologates stillatim translations along both
;; the tape's sinistral and dextral axis.
;; 
;; 
;; Instructions
;; ============
;; The SBFM language family wists of a membership's twain; a paregal in
;; their enumeration and potentials, a modesty of modulations governs
;; their divergence, however, always entrusted with the program memory's
;; castaldy and a conditional iterance construct's deployment.
;; 
;; == OVERVIEW: SBFM-1 ==
;; The following apercu shall be a medium to a cursory gnarity's
;; administration anenst the SBFM-1 language variation's competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   {       | Inverts the current cell's bit value. If the new state
;;           | equals zero (0), moves the instruction pointer (IP)
;;           | forward to the matching "}" instruction; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | If the code segment ensconced by the jumelle of "{" and
;;           | "}" tokens does not comprehend any commands, no looping
;;           | capacity is exercised, akin to a zero-valued probed bit
;;           | predicate.
;;   ..................................................................
;;   }       | If the current cell's bit value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "{" instruction;
;;           | otherwise translates the cell pointer one step to the
;;           | left and proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == OVERVIEW: SBFM-2 ==
;; A second overview shall be produced for the SBFM-2 variant, the
;; equipollence of which derives from an equinumerant but slightly
;; altered foundry of the SBFM-1 original:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   {       | Inverts the current cell's bit value and translates the
;;           | cell pointer one step to the left. If the new state
;;           | equals zero (0), moves the instruction pointer (IP)
;;           | forward to the matching "]" instruction; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | If the code segment ensconced by the jumelle of "{" and
;;           | "]" tokens does not comprehend any commands, no looping
;;           | capacity is exercised, akin to a zero-valued probed bit
;;           | predicate.
;;   ..................................................................
;;   ]       | If the current cell's bit value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "{" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-22
;; 
;; Sources:
;;   [esolang2023SBFM]
;;   The Esolang contributors, "SBFM", November 22nd, 2023
;;   URL: "https://esolangs.org/wiki/SBFM"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose keys assumes
   the KEY-TYPE and answer to values of the VALUE-TYPE, both defaulting
   to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and (hash-table-p candidate)
               (loop for key of-type T being the hash-keys in candidate
                     using (hash-value value)
                     always (and (typep key   key-type)
                                 (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of elements of the
   ELEMENT-TYPE, imposing as its default the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and (listp candidate)
               (loop for element of-type T in candidate
                     always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype stack-of (&optional (element-type T))
  "The ``stack-of'' type defines a list-based stack composed of elements
   of the ELEMENT-TYPE, imposing as its default the comprehensive
   ``T''."
  `(list-of ,element-type))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping which associates the loop
   end points in a bidirectional manner by mediation of their locations
   in the program."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype skip-table ()
  "The ``skip-table'' defines a mapping which associates the loop start
   andpoints with a Boolean assessment of their body's vacancy."
  '(hash-table-of fixnum boolean))

;;; -------------------------------------------------------

(deftype tape ()
  "Defines the program memory as a sparse vector of bits, the keys
   representing the cell indices, the values the cell states."
  '(hash-table-of integer bit))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized causata exercised by
   any of the SBFM variants."
  '(member
    :move-pointer-right
    :flip-bit-and-start-loop
    :flip-bit-move-pointer-left-and-start-loop
    :end-loop
    :end-loop-and-move-pointer-left))

;;; -------------------------------------------------------

(deftype command-table ()
  "The ``command-table'' type defines a unilateral association betwixt
   a token and its operation representation, realized in an association
   list, or alist, the keys of which impose characters, while the
   values assume the amenable ``command'' objects."
  '(list-of (cons character command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of command tables.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-table +SBFM-1-COMMANDS+))
(declaim (type command-table +SBFM-2-COMMANDS+))

;;; -------------------------------------------------------

(defparameter +SBFM-1-COMMANDS+
  '((#\> . :move-pointer-right)
    (#\{ . :flip-bit-and-start-loop)
    (#\} . :end-loop-and-move-pointer-left))
  "Associates the SBFM-1 operation symbols with the represented
   commands.")

;;; -------------------------------------------------------

(defparameter +SBFM-2-COMMANDS+
  '((#\> . :move-pointer-right)
    (#\{ . :flip-bit-move-pointer-left-and-start-loop)
    (#\] . :end-loop))
  "Associates the SBFM-2 operation symbols with the represented
   commands.")

;;; -------------------------------------------------------

(defun command-token-p (command-table candidate)
  "Determines whether the CANDIDATE represents an opertive token, as
   imposed by the COMMAND-TABLE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type command-table command-table))
  (declare (type character     candidate))
  (the boolean
    (not (null
      (assoc candidate command-table :test #'char=)))))

;;; -------------------------------------------------------

(defun loop-start-command-token-p (command-table candidate)
  "Determines whether the CANDIDATE represents a loop start instruction
   according to the COMMAND-TABLE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type command-table command-table))
  (declare (type character     candidate))
  (let ((command (cdr (assoc candidate command-table :test #'char=))))
    (declare (type (or null command) command))
    (the boolean
      (not (null
        (and command
          (member command
            '(:flip-bit-and-start-loop
              :flip-bit-move-pointer-left-and-start-loop)
            :test #'eq)))))))

;;; -------------------------------------------------------

(defun loop-end-command-token-p (command-table candidate)
  "Determines whether the CANDIDATE represents a loop end instruction
   according to the COMMAND-TABLE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type command-table command-table))
  (declare (type character     candidate))
  (let ((command (cdr (assoc candidate command-table :test #'char=))))
    (declare (type (or null command) command))
    (the boolean
      (not (null
        (and command
          (member command
            '(:end-loop
              :end-loop-and-move-pointer-left)
            :test #'eq)))))))

;;; -------------------------------------------------------

(defun get-command (command-table token)
  "Returns for the TOKEN the associated command from the COMMAND-TABLE,
   or signals an error of an unspecified type upon its disrespondency."
  (declare (type command-table command-table))
  (declare (type character     token))
  (the command
    (or (cdr (assoc token command-table :test #'char=))
        (error "No recognized command token: ~s." token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-jump-table (code command-table)
  "Returns a jump table connecting the SBFM source CODE's loop end
   points by the locations of their loop start and end points, as
   determined by the COMMAND-TABLE."
  (declare (type string        code))
  (declare (type command-table command-table))
  (let ((jump-table        (make-hash-table :test #'eql))
        (loop-start-points NIL))
    (declare (type jump-table        jump-table)
             (type (stack-of fixnum) loop-start-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from   0 by 1
      
      if (loop-start-command-token-p command-table token) do
        (push position loop-start-points)
      else if (loop-end-command-token-p command-table token) do
        (if loop-start-points
          (let ((start-point (pop loop-start-points)))
            (declare (type fixnum start-point))
            (psetf (gethash start-point jump-table) position
                   (gethash position    jump-table) start-point))
          (error "Unmatched loop end command at position ~d." position))
      
      finally
        (when loop-start-points
          (error "Unmatched loop start command~p at position~:p ~
                  ~{~d~^, ~}."
            (length loop-start-points) loop-start-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of skip table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nop-segment-betwixt-p (code command-table start-point end-point)
  "Determines whether the SBFM CODE entails ineffective tokens in the
   segment spanned by the inclusive START-POINT and extending towards
   the exclusive END-POINT, as defined by the COMMAND-TABLE, returning
   for a no-operation segment a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string        code))
  (declare (type command-table command-table))
  (declare (type fixnum        start-point))
  (declare (type fixnum        end-point))
  (the boolean
    (null
      (find-if
        #'(lambda (probed-token)
            (declare (type character probed-token))
            (command-token-p command-table probed-token))
        code
        :start start-point
        :end   end-point))))

;;; -------------------------------------------------------

(defun compute-skip-table (code command-table jump-table)
  "Returns a loop skip table for the piece of SBFM source CODE,
   employing the COMMAND-TABLE for the vacant code segments'
   recognition, and the JUMP-TABLE for the loop bournes' inquisition.
   ---
   Every entry in the thus generated skip table represents a loop start
   or end point desumed from the JUMP-TABLE, the affiliated object being
   a Boolean flag which determines whether the iterance body is empty."
  (declare (type string        code))
  (declare (type jump-table    jump-table))
  (declare (type command-table command-table))
  (let ((skip-table (make-hash-table :test #'eql)))
    (declare (type skip-table skip-table))
    (loop
      for   start-point of-type fixnum being the hash-keys in jump-table
      using (hash-value end-point)
      when (< start-point end-point) do
        (let ((empty-loop-p
                (nop-segment-betwixt-p code command-table
                  (1+ start-point) end-point)))
          (declare (type boolean empty-loop-p))
          (psetf (gethash end-point   skip-table) empty-loop-p
                 (gethash start-point skip-table) empty-loop-p)))
    (the skip-table skip-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flip-bit (tape index)
  "Flips the bit value in the TAPE cell at the INDEX and returns no
   value."
  (declare (type tape    tape))
  (declare (type integer index))
  (setf (gethash index tape 0) (logxor (gethash index tape 0) 1))
  (values))

;;; -------------------------------------------------------

(defun zero-valued-cell-p (tape index)
  "Determines whether the TAPE cell at the INDEX contains a zero bit,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type tape    tape))
  (declare (type integer index))
  (the boolean
    (not (null
      (zerop
        (gethash index tape 0))))))

;;; -------------------------------------------------------

(defun get-bounding-tape-indices (tape)
  "Determines the minimum and maximum TAPE cell index and returns two
   values:
     (1) The smallest inclusive TAPE cell index.
     (2) The largest  inclusive TAPE cell index."
  (declare (type tape tape))
  (the (values integer integer)
    (loop
      for      cell-index of-type integer being the hash-keys in tape
      minimize cell-index into smallest-cell-index of-type integer
      maximize cell-index into largest-cell-index  of-type integer
      finally
        (return
          (values smallest-cell-index largest-cell-index)))))

;;; -------------------------------------------------------

(defun print-tape (tape)
  "Prints the TAPE's cell values, commencing from the smalelst index and
   terminating in the largest one, to the standard output and returns no
   value.
   ---
   This function merely displays those TAPE cells explicitly modified."
  (declare (type tape tape))
  (multiple-value-bind (smallest-cell-index largest-cell-index)
      (get-bounding-tape-indices tape)
    (declare (type integer smallest-cell-index))
    (declare (type integer largest-cell-index))
    (loop
      for cell-index
        of-type integer
        from    smallest-cell-index
        to      largest-cell-index
      do
        (format T "~&Memory[~d] = ~d"
          cell-index
          (gethash cell-index tape 0))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-program-state (tape)
  "Prints the SBFM program state, represented by its TAPE's content, to
   the standard output and returns no value."
  (print-tape tape)
  (finish-output)
  (sleep 0.5)
  (values))

;;; -------------------------------------------------------

(defun interpret-SBFM (code command-table)
  "Interprets the piece of SBFM source CODE, its operative contents
   being specified by the COMMAND-TABLE, prints upon each cycle's
   patration the tape, and returns no value."
  (declare (type string        code))
  (declare (type command-table command-table))
  (let ((ip           0)
        (jump-table   (compute-jump-table code command-table))
        (tape         (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type tape       tape))
    (declare (type integer    cell-pointer))
    (let ((skip-table (compute-skip-table
                        code command-table jump-table)))
      (declare (type skip-table skip-table))
      (loop
        while (< ip (length code))
        for   current-token of-type character = (char code ip)
        when  (command-token-p command-table current-token) do
          (case (get-command command-table current-token)
            (:move-pointer-right
              (incf cell-pointer))
            
            (:flip-bit-and-start-loop
              (flip-bit tape cell-pointer)
              (when (or (zero-valued-cell-p tape cell-pointer)
                        (gethash ip skip-table))
                (setf ip
                  (1- (gethash ip jump-table)))))
            
            (:flip-bit-move-pointer-left-and-start-loop
              (flip-bit tape cell-pointer)
              (decf cell-pointer)
              (when (or (zero-valued-cell-p tape cell-pointer)
                        (gethash ip skip-table))
                (setf ip
                  (1- (gethash ip jump-table)))))
            
            (:end-loop
              (unless (or (zero-valued-cell-p tape cell-pointer)
                          (gethash ip skip-table))
                (setf ip (gethash ip jump-table))))
            
            (:end-loop-and-move-pointer-left
              (if (or (zero-valued-cell-p tape cell-pointer)
                      (gethash ip skip-table))
                (decf cell-pointer)
                (setf ip (gethash ip jump-table))))
            
            (otherwise
              (error "Unrecognized command ~s associated with the ~
                      token \"~c\" at position ~d."
                (get-command command-table current-token)
                current-token ip)))
        end
        do
          (print-program-state tape)
          (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-SBFM-1 (code)
  "Interprets the piece of SBFM-1 source CODE, prints upon each cycle's
   patration the tape, and returns no value."
  (declare (type string code))
  (interpret-SBFM code +SBFM-1-COMMANDS+)
  (values))

;;; -------------------------------------------------------

(defun interpret-SBFM-2 (code)
  "Interprets the piece of SBFM-2 source CODE, prints upon each cycle's
   patration the tape, and returns no value."
  (declare (type string code))
  (interpret-SBFM code +SBFM-2-COMMANDS+)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the first bit to one (1).
(interpret-SBFM-1 "{}")

;;; -------------------------------------------------------

;; Infinite loop.
(interpret-SBFM-2 ">{]>>{>{]]")
