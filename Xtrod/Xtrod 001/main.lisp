;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Xtrod", invented by the Esolang user "Ractangle" and
;; presented on August 27th, 2024, its conception that of a minimization
;; applied to Urban Mueller's language "brainfuck", the cambistry's
;; adhibition a curtailment from the octuple operation contingency to a
;; quadruple by conflations in the cell pointer motion and arithmetic
;; capabilities, the input and output intercourse, as well as the
;; jump-based control duction mechanism's poles.
;; 
;; 
;; Concept
;; =======
;; The Xtrod programming language's dioristic contribution wones in the
;; reduction of brainfuck's octuple instruction set to a cardinality of
;; four members; a chevisance whose obtention's gendrure emerges from
;; the conflation of several originally separate faculties into a
;; single symbol.
;; 
;; == XTROD: A BRAINFUCK MINIMALIZATION ==
;; The cynosure of Xtrod's purpose and telos appertains to an expression
;; of its brainfuck entheus' octuple instruction set in a more
;; compendious format, the same recludes to its tolerance a quadruple
;; contingency.
;; 
;; The warklume deployed to this accomplishment is located in the
;; conflation of several, partially cognate, partially forinsecal,
;; facilities in the provenance to a conjoined epiphenomenon.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of Xtrod's recipiency does not elude brainfuck's
;; architecture, appropriating in an ipsissima verba fashion a
;; bilaterally bourneless dispansion of unsigned byte-valued cells.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; The retention of brainfuck's capabilities in Xtrod ensues from the
;; conflation of the provenance's eight instructions to a quadruple of
;; aggregates.
;; 
;; == OVERVIEW ==
;; The following apercu's telos shall be satisfied in a requisite mete
;; of nortelry's dation concerning the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   (       | If the current cell value contains zero (0), relocates
;;           | the cell pointer one step in the dextral airt;
;;           | otherwise, for a non-zero cell state, increments the
;;           | current cell value by one (1). If the modified cell's
;;           | new state transcends the upper bourne of 255, its value
;;           | wraps around to the minimum of zero (0).
;;   ..................................................................
;;   )       | If the current cell value contains zero (0), relocates
;;           | the cell pointer one step in the sinistral airt;
;;           | otherwise, for a non-zero cell state, decrements the
;;           | current cell value by one (1). If the modified cell's
;;           | new state transcends the lower bourne of zero (0), the
;;           | value wraps around to the maximum of 255.
;;   ..................................................................
;;   !       | If the current cell contains zero (0), queries the
;;           | standard input conduit for a character and stores its
;;           | ASCII code in the current cell; otherwise, if the cell's
;;           | value does not equal zero (0), prints the character
;;           | whose ASCII code corresponds to the cell vaue to the
;;           | standard output conduit.
;;   ..................................................................
;;   |       | If the current cell contains zero (0), skips the next
;;           | instruction; otherwise, if the value does not equal
;;           | zero (0), moves the instruction pointer (IP) either
;;           | forward or back to the position immediately succeeding
;;           | the nearest "|" instruction.
;;           |---------------------------------------------------------
;;           | Please heed that only operation identifiers contribute
;;           | to the distance betwixt two "|" tokens; other content,
;;           | construed as commentary, will be neglected.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A few inroads of ambiguity beleaguer the Xrod programming language's
;; protolog, among the same a subset shall be extracted for further
;; perquisition.
;; 
;; == HOW DOES THE JUMP MECHANISM OPERATE? ==
;; The conflation of brainfuck's "[" and "]" jumelle, the former airted
;; dextrally, the latter backwards, into an aefauld "|" demarcation in
;; Xtrod, recludes an inroad for two ambivalencies in the concrete
;; selection of the compernage.
;; 
;; The protolog relates of a tendency to seek the "nearest |" upon
;; the current cell value's equality to zero, which begets the
;; questions:
;; 
;;   (1) ARE BOTH DIRECTIONS NAVIGABLE?
;;       If the nearest "|" resides to the current jump point's
;;       sinistral side, shall a backwards relocation be actuated?
;;       As a special case, upon a perfect equality in the candidates
;;       for destinations along both lateralities, which side --- left
;;       or right --- will be entalented with superior prevalence?
;;   
;;   (2) HOW IS DISTANCE METED?
;;       The dependency upon the distance to the "nearest" point
;;       establishes a predicament in the actual measurement; in a
;;       concrete diction: Do only operative symbols contribute to this
;;       distance, or shall commentary content be included in this
;;       tally?
;; 
;; Concerning the instruction pointer (IP) relocation to neighboring
;; "|" tokens' it has been adjudged to homologate the navigation to both
;; prevenient and succeeding targets.
;; 
;; Anent the distance metric, only characters affiliated with an
;; operative potential shall be subjected to the consideration; other
;; elements remain without epiphenomenal impact.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, its operations exercising immediately upon the Xtrod
;; source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-06-14
;; 
;; Sources:
;;   [esolang2024Xtrod]
;;   The Esolang contributors, "Xtrod", December 13th, 2024
;;   URL: "https://esolangs.org/wiki/Xtrod"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\" and
   produces a veridicous Boolean tantamount thereof, returning for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instruction-symbol-p (candidate)
  "Determines whether the CANDIDATE represents a symbol affiliated with
   an Xtrod instruction, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "()!|" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-simple-base-string (source)
  "Returns a simple base string representation of the SOURCE."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))

;;; -------------------------------------------------------

(defun remove-commentary-content (source)
  "Returns a simple base string representation of the SOURCE, purged
   from any non-operative characters."
  (declare (type string source))
  (the simple-base-string
    (convert-into-simple-base-string
      (remove-if-not #'instruction-symbol-p source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-pristine-tape ()))
  "The ``Tape'' class furnishes the implementation of the program tape,
   defined in terms of a bilaterally infinite dispansion of unsigned
   byte-valued cells, upon operates a cell pointer that any instant
   selects the currently active unit."
  (bits                         #b00000000
                                :type      unsigned-byte
                                :read-only NIL)
  (pointer                      0
                                :type      integer
                                :read-only NIL)
  (smallest-accessed-cell-index 0
                                :type      integer
                                :read-only NIL))

;;; -------------------------------------------------------

(defun translate-current-cell-index-into-bit-offset (tape)
  "Returns the unsigned integer offset into the TAPE's bits
   corresponding to its cell pointer's currently selected cell index
   and designating the start position inside of the binary sequence of
   this particular cell."
  (declare (type Tape tape))
  (the (integer 0 *)
    (* (- (tape-pointer                      tape)
          (tape-smallest-accessed-cell-index tape))
       8)))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the (unsigned-byte 8)
    (ldb
      (byte 8
        (translate-current-cell-index-into-bit-offset tape))
      (tape-bits tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by a wrapping of its state in order to respect
   the admissible bournes of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (byte 8
        (translate-current-cell-index-into-bit-offset tape))
      (tape-bits tape))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (decf (tape-pointer tape))
  (when (< (tape-pointer                      tape)
           (tape-smallest-accessed-cell-index tape))
    (psetf
      (tape-smallest-accessed-cell-index tape)
        (tape-pointer tape)
      (tape-bits tape)
        (ash (tape-bits tape) 8)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump point location operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-previous-jump-point (source start-point)
  "Proceeding from the exclusive START-POINT and advancing in a
   dextrosinistral airt, locates the position of the nearest jump
   instruction encountered along the sinistral laterality and signified
   by a vertical bar (\"|\"); or, upon its disrespondency, responds with
   ``NIL''."
  (declare (type simple-base-string source))
  (declare (type fixnum             start-point))
  (the (or null fixnum)
    (when (plusp start-point)
      (position #\| source
        :start    0
        :end      (1- start-point)
        :from-end T
        :test     #'char=))))

;;; -------------------------------------------------------

(defun locate-next-jump-point (source start-point)
  "Proceeding from the exclusive START-POINT and advancing in a
   sinistrodextral airt, locates the position of the nearest jump
   instruction encountered along the dextral laterality and signified by
   a vertical bar (\"|\"); or, upon its disrespondency, responds with
   ``NIL''."
  (declare (type simple-base-string source))
  (declare (type fixnum             start-point))
  (the (or null fixnum)
    (position #\| source
      :start (1+ start-point)
      :test  #'char=)))

;;; -------------------------------------------------------

(defun select-jump-point-with-minimum-distance (start-point
                                                left-jump-point
                                                right-jump-point)
  "Returns among the LEFT-JUMP-POINT and the RIGHT-JUMP-POINT that
   candidate whose distance metes the smallest from the START-POINT,
   preferring in the case of a paregal measurement the sinistral
   choice."
  (declare (type fixnum start-point))
  (declare (type fixnum left-jump-point))
  (declare (type fixnum right-jump-point))
  (the fixnum
    (or (and (<= (abs (- start-point left-jump-point))
                 (abs (- start-point right-jump-point)))
             left-jump-point)
        right-jump-point)))

;;; -------------------------------------------------------

(defun select-nearest-jump-point (start-point
                                  left-jump-point
                                  right-jump-point)
  "Given the required START-POINT in conjunction the LEFT-JUMP-POINT and
   the RIGHT-START-POINT, both of which may assume the ``NIL'' sentinel
   upon their absence, either returns the one nearest to the
   START-POINT, if both exist, the aefauld specified among these, or,
   upon the twissel's lacuna, the ``NIL'' value."
  (declare (type fixnum           start-point))
  (declare (type (or null fixnum) left-jump-point))
  (declare (type (or null fixnum) right-jump-point))
  (the (or null fixnum)
    (or (and left-jump-point
             right-jump-point
             (select-jump-point-with-minimum-distance
               start-point
               left-jump-point
               right-jump-point))
        left-jump-point
        right-jump-point)))

;;; -------------------------------------------------------

(defun locate-nearest-jump-point (source start-point)
  "Proceeding from the exclusive START-POINT and advancing in a
   sinistrodextral airt, locates the position of the nearest jump
   instruction, signified by a vertical bar (\"|\"); or, upon its
   disrespondency, signals an error of an unspecified type."
  (declare (type simple-base-string source))
  (declare (type fixnum             start-point))
  (the fixnum
    (or (select-nearest-jump-point start-point
          (locate-previous-jump-point source start-point)
          (locate-next-jump-point     source start-point))
        (error "No jump point associated with the position ~d."
          start-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun request-input (tape &key (displays-prompt-p T))
  "Queries the standard input conduit for a character, stores its ASCII
   code in the TAPE's current cell, and returns no value."
  (declare (type Tape    tape))
  (declare (type boolean displays-prompt-p))
  (when displays-prompt-p
    (format *standard-output* "~&>> "))
  (finish-output *standard-output*)
  (setf (current-cell-value tape)
    (char-code
      (read-char *standard-input* NIL
        (code-char 0))))
  (clear-input *standard-input*)
  (values))

;;; -------------------------------------------------------

(defun issue-output (tape)
  "Prints the character whose ASCII code corresponds to the TAPE's
   current cell value to the standard output conduit and returns no
   value."
  (declare (type Tape tape))
  (format *standard-output* "~c"
    (code-char
      (current-cell-value tape)))
  (finish-output *standard-output*)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Xtrod (code &key (displays-prompt-p T))
  "Interprets the piece of Xtrod source CODE and returns no value."
  (declare (type string  code))
  (declare (type boolean displays-prompt-p))
  (let ((optimized-code (remove-commentary-content code))
        (ip             0)
        (tape           (make-pristine-tape)))
    (declare (type simple-base-string optimized-code))
    (declare (type fixnum             ip))
    (declare (type Tape               tape))
    (loop while (< ip (length optimized-code)) do
      (case (schar code ip)
        (#\(
          (if (zerop (current-cell-value tape))
            (move-cell-pointer-right tape)
            (incf (current-cell-value tape))))
        (#\)
          (if (zerop (current-cell-value tape))
            (move-cell-pointer-left tape)
            (decf (current-cell-value tape))))
        (#\!
          (if (zerop (current-cell-value tape))
            (request-input tape :displays-prompt-p displays-prompt-p)
            (issue-output  tape)))
        (#\|
          (if (zerop (current-cell-value tape))
            (incf ip 1)
            (setf ip (locate-nearest-jump-point optimized-code ip))))
        (otherwise
          NIL))
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Xtrod "!!")
