;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter of the esoteric programming
;; lanugage "BF", invented by the Esolang user "BestCode" and presented
;; on June 24th, 2024, conceived as a derivation of Urban Mueller's
;; "brainfuck", while, maugre its patration in the quantitative and
;; syntactical congruency with the entheus, applies a transposition in
;; the octuple identifier set with regard to the affiliated causata,
;; whence ensues an interesting confounding aspect.
;; 
;; 
;; Concept
;; =======
;; The BF programming language constitutes an equipollent to its
;; brainfuck entheus, the lealty's incarnation neither of the wite's
;; tholance to deviate from the basic tenets in the program execution,
;; neither from the data castaldy's mode and epiphenomena; however, the
;; diorism's contribution constitutes a tenant in the novel covin
;; inwith which the syntactical and semantic aspects engage, desuming
;; the octuple operation identifiers, yet distributing their devers in
;; a widdershins conception.
;; 
;; == BF: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; BF's proprium appertains to its deployment of brainfuck's eight
;; recognized instruction agnominations, everichon in this accompt an
;; aefauld character, confounding in its nature by a reassignment of
;; the resulting causata.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of BF's recipiency does not elude brainfuck's
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
;; Begotten from a status of paregal conception in all but its
;; identifiers' semantical aspects to its brainfuck heritage, BF's
;; compass does neither actuate an ostention's commission designed with
;; supererogation with respect to its provenance, nor a curtailment in
;; the competences; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the BF programming language's facilities,
;; thilk concomitantly concur with the offerings of its brainfuck
;; stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   <       | Increments the current cell value by one (1). If the new
;;           | state transgresses the upper march of 255, the value
;;           | wraps around to the lower extremum of zero (0).
;;   ..................................................................
;;   >       | Decrements the current cell value by one (1). If the new
;;           | state transgresses the lower march of zero (0), the
;;           | value wraps around to the upper extremum of 255.
;;   ..................................................................
;;   -       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   +       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   [       | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   ]       | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ..................................................................
;;   .       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   ,       | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BF AND BRAINFUCK ==
;; The deliberately kensback reconfiguration of the expected vincula
;; atwixen identifiers and epiphenomena accommodated a woning in BF
;; shall be an elucidatory purlicue's subject, furnishing an
;; equiparation from the descendant towards the provenance:
;; 
;;   ----------------------------------------------
;;   BF | brainfuck | Causatum
;;   ---+-----------+------------------------------
;;   <  | +         | Increment the current cell.
;;   ..............................................
;;   >  | -         | Decrement the current cell.
;;   ..............................................
;;   +  | <         | Move the cell pointer left.
;;   ..............................................
;;   -  | >         | Move the cell pointer right.
;;   ..............................................
;;   [  | .         | Print the current cell.
;;   ..............................................
;;   ]  | ,         | Input into the current cell.
;;   ..............................................
;;   .  | [         | Jump forward if zero.
;;   ..............................................
;;   ,  | ]         | Jump back if not zero.
;;   ----------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, limns the immediacy of the
;; execution on the source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-18
;; 
;; Sources:
;;   [esolang2025:BF:BestCode]
;;   The Esolang contributors,
;;     "https://esolangs.org/wiki/BF_(BestCoder)",
;;     September 22nd, 2025
;;   URL: "https://esolangs.org/wiki/BF_(BestCoder)"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the types.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   (8) attiguous bits, thus specifying an incolant of the closed
   integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the global variables and constants.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *program*))
(declaim (type fixnum        *ip*))
(declaim (type character     *current-token*))
(declaim (type boolean       *program-is-exhausted-p*))

(declaim (type unsigned-byte *tape*))
(declaim (type integer       *cell-pointer*))
(declaim (type integer       *lowest-accessed-cell-index*))
(declaim (type T             *byte-selector-the-current-cell*))
(declaim (type boolean       *current-cell-contains-zero-p*))

;;; -------------------------------------------------------

(defparameter *program* ""
  "The BF source code to execute.")

(defparameter *ip* 0
  "The zero-based current instruction pointer (IP) position into the
   ``*program*'' string.")

;; The character at the position ``*ip*'' into the ``*program*'' string.
(define-symbol-macro *current-token*
  (the character
    (schar *program* *ip*)))

(define-symbol-macro *program-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *program* *ip*))))

(defparameter *tape* #b00000000
  "The memory tape as an integer-encoded bit sequence, each attiguous
   catena of eight bits forming a cell's unsigned byte representation,
   with the lowest bits mapping to the cell amenable to the smallest
   address, increasing in the subscripts towards the most significant
   bit (MSB) positions.")

(defparameter *cell-pointer* 0
  "The current cell pointer position, designating the index of the
   cell contemporaneously entalented with an amenability for
   perquisitions and modulations.
   ---
   As a parasceve to the obtention or modification of the currently
   selected cell, the cell pointer ought to experience a transcription
   into a non-negative bit offset into the ``*TAPE*'', the same defines
   the cell states as an attiguous sequence of bits.")

(defparameter *lowest-accessed-cell-index* 0
  "The bit offset into the ``*tape*'''s binary sequence which
   corresponds to the unsigned byte's lowest significant bit (LSB) in
   the cell designated by the ``*cell-pointer*''.")

(define-symbol-macro *byte-selector-for-the-current-cell*
  (the T
    (byte 8
      (* (- *cell-pointer* *lowest-accessed-cell-index*) 8))))

(define-symbol-macro *current-cell-contains-zero-p*
  (the boolean
    (not (null
      (zerop
        (current-cell-value))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the tape operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-the-cell-pointer-left ()
  "Translates the tape's cell pointer one step to the left and returns
   no value."
  (decf *cell-pointer*)
  (when (< *cell-pointer* *lowest-accessed-cell-index*)
    (psetf
      *lowest-accessed-cell-index* *cell-pointer*
      *tape*                       (ash *tape* 8)))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right ()
  "Translates the tape's cell pointer one step to the right and returns
   no value."
  (incf *cell-pointer*)
  (values))

;;; -------------------------------------------------------

(defun current-cell-value ()
  "Returns the unsigned byte value stored in the tape's current cell."
  (the octet
    (ldb *byte-selector-for-the-current-cell* *tape*)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value)
  "Stores the NEW-VALUE in the tape's current cell, contingently
   preceded by a wrapping of the state into the admissible unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (setf (ldb *byte-selector-for-the-current-cell* *tape*)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-matching-back-jump-point (&aux (start-point *ip*))
  "Proceeding from the position immediately succeeding the current
   instruction pointer location, ``*ip*'', searches in a sinistrodextral
   airt for a matching back jump instruction, designated by the symbol
   \",\", relocates the instruction pointer ``*ip*'' to the same, and
   returns no value."
  (declare (type fixnum start-point))
  (incf *ip*)
  (loop
    with nesting-level of-type fixnum = 0
    
    if *program-is-exhausted-p* do
      (error "Unmatched forward jump point at position ~d." start-point)
    else if (char= *current-token* #\,) do
      (if (zerop nesting-level)
        (loop-finish)
        (decf nesting-level))
    else if (char= *current-token* #\.) do
      (incf nesting-level)
    end
    
    do (incf *ip*))
  (values))

;;; -------------------------------------------------------

(defun locate-the-matching-forward-jump-point (&aux (start-point *ip*))
  "Proceeding from the position immediately preceding the current
   instruction pointer location, ``*ip*'', searches in a dextrasinistral
   airt for a matching forward jump instruction, designated by the
   symbol \".\", relocates the instruction pointer ``*ip*'' to the same,
   and returns no value."
  (declare (type fixnum start-point))
  (decf *ip*)
  (loop
    with nesting-level of-type fixnum = 0
    
    if *program-is-exhausted-p* do
      (error "Unmatched back jump point at position ~d." start-point)
    else if (char= *current-token* #\.) do
      (if (zerop nesting-level)
        (loop-finish)
        (decf nesting-level))
    else if (char= *current-token* #\,) do
      (incf nesting-level)
    end
    
    do (decf *ip*))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type character +NULL-CHARACTER+))

;;; -------------------------------------------------------

(defconstant +NULL-CHARACTER+
  (code-char 0)
  "Represents the \"null character\", amenable to the ASCII code zero
   (0), in an implementation-independent manner.")

;;; -------------------------------------------------------

(defun interpret-BF (code)
  "Interprets the piece of BF source CODE and returns no value."
  (declare (type string code))
  (psetf
    *program*                    (coerce code 'simple-string)
    *ip*                         0
    *tape*                       #b00000000
    *cell-pointer*               0
    *lowest-accessed-cell-index* 0)
  (loop until *program-is-exhausted-p* do
    (case *current-token*
      (#\<
        (incf (current-cell-value)))
      (#\>
        (decf (current-cell-value)))
      (#\+
        (move-the-cell-pointer-left))
      (#\-
        (move-the-cell-pointer-right))
      (#\[
        (format T "~c"
          (code-char
            (current-cell-value))))
      (#\]
        (format T "~&>> ")
        (finish-output)
        (setf (current-cell-value)
          (char-code
            (read-char NIL NIL +NULL-CHARACTER+)))
        (clear-input))
      (#\.
        (when *current-cell-contains-zero-p*
          (locate-the-matching-back-jump-point)))
      (#\,
        (unless *current-cell-contains-zero-p*
          (locate-the-matching-forward-jump-point)))
      (otherwise
        NIL))
    (incf *ip*))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-BF "<.>>->.--<->>>>>++,+>>+>>>,->[---<[--[[<<<.[-,++++[<<<[>>>>>>[++>[----<[")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-BF "].[],")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-BF "<<<<<<<<<--]++.>-<<<<<+,-<<<.->+>,--<<<<<.-<<<<<<<<<<+>,->++.--[++,-->[")
