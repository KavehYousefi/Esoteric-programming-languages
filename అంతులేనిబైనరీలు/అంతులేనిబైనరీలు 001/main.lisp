;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "అంతులేనిబైనరీలు", invented by the Esolang user "Cinnamony"
;; and presented on June 20th, 2023, its haecceity's most kenspeckle
;; expression the deployment of a bidirectionally infinite catena of
;; bits, operated upon by one-symbol instructions which, a fortiori,
;; encompass an infinite iterance construct.
;; 
;; 
;; Concept
;; =======
;; The అంతులేనిబైనరీలు programming language represents a specimen to
;; whom a dioristic reliance upon a bilaterally infinite dispansion of
;; bit-valued cells is assigned, the warklooms of its efficacy
;; one-symbol instructions nuncupated to the provision of bit
;; manipulations, control flow duction, and communications with a
;; boustrophedon's liberality.
;; 
;; == అంతులేనిబైనరీలు = /antulenibaɪnaɾilu/ ==
;; Maugre its ostention of characters desumed from the realm of Indian
;; diction, the అంతులేనిబైనరీలు, officially assigned, by its protolog's
;; averment, the pronunciation "/antulenibaɪnaɾilu/", deploys merely the
;; restrictive set commorant in the ASCII repertoire.
;; 
;; == INSTRUCTIONS ARE REPRESENTED BY SINGLE SYMBOLS ==
;; Each instruction's designment ensues from an aefauld character's
;; imposition, deprived of the necessity for adherent arguments.
;; 
;; Symbols in a carency of a causatum's apportionment experience a
;; tolerance's adhibition which is meted with an equipollence to their
;; neglect, ultimately ordained to a commentary purpose as no-operations
;; (NOPs).
;; 
;; == THE MEMORY: A BILATERALLY INFINITE TAPE OF BITS ==
;; The data department consigns its requisites to a bilaterally infinite
;; tape's onus, the conformation that of bit-valued cells, each such a
;; salvatory to a single integer value from the set {0, 1}.
;; 
;; Operating upon this binary catena, a cell pointer's governance
;; permits the handling of the currently selected tape cell, the same
;; represents at any instant that unit endowed with exclusive
;; responsiveness to perquisitions and modifications. The cursor's
;; motile nature capacitates stillatim translations along both of the
;; tape's axes in order to influence the cell selection.
;; 
;; 
;; Instructions
;; ============
;; An undecimal cardinality serves to exhaust the అంతులేనిబైనరీలు
;; programming language's instruction set, the bailiwicks enjoying a
;; parcery distributed across rudimentary bit manipulations, conditional
;; jumping facilities, a perpetual iterance construct, as well as input
;; and output intercourse warklumes.
;; 
;; == OVERVIEW ==
;; The following tabulation's cynosure shall be a requisite gnarity's
;; dation concerning the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   +       | Flips the current cell value.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   .       | Prints the current cell value in its verbatim numeric
;;           | form to the standard output conduit, neither preceded
;;           | nor succeeded by any adscititious content.
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a bit, that is,
;;           | an integral number from the set {0, 1}, and stores thilk
;;           | in the current cell.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token;
;;           | otherwise, for a one-valued (1) current cell,
;;           | accompasses no causatum.
;;   ..................................................................
;;   ]       | If the current cell value equals one (1), moves the
;;           | instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" token;
;;           | otherwise, for a zero-valued (0) current cell,
;;           | accompasses no causatum.
;;   ..................................................................
;;   (       | If the current cell value equals one (1), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching ")" token;
;;           | otherwise, for a zero-valued (0) current cell,
;;           | accompasses no causatum.
;;   ..................................................................
;;   )       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "(" token;
;;           | otherwise, for a one-valued (1) current cell,
;;           | accompasses no causatum.
;;   ..................................................................
;;   {       | Commences an infinite loop which extends to the
;;           | program's desinence, repeating after the conclusion, if
;;           | not terminated by a matching "}" operation in the
;;           | institial course.
;;   .................................................................
;;   }       | Immediately deactivates the matching infinite loop
;;           | instigator "{".
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization has been accomplished in the
;; programming language Common Lisp, the object of its efforts the
;; furnished source code string itself in its immediate form.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-10-27
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023అంతులేనిబైనరీలు]
;;   The Esolang contributors, "అంతులేనిబైనరీలు", August 19th, 2023
;;   URL: "https://esolangs.org/wiki/%E0%B0%85%E0%B0%82%E0%B0%A4%E0%B1%81%E0%B0%B2%E0%B1%87%E0%B0%A8%E0%B0%BF%E0%B0%AC%E0%B1%88%E0%B0%A8%E0%B0%B0%E0%B1%80%E0%B0%B2%E0%B1%81"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list serving as a salvatory to zero or
   more elements commorant in the realm of the ELEMENT-TYPE, thilk in
   its default configuration submits to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    current-element of-type T in (the list candidate)
              always (typep current-element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   is such to enumerate an arbitrary accompt of entries, their keys
   complying with the KEY-TYPE and mapping to values of the VALUE-TYPE,
   for both is imposed the default configuration of the generic sentinel
   ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   the jumelles of jump points in a అంతులేనిబైనరీలు program, mediated
   by adminiculum of their zero-based positions into the underlying
   source code and manifesting in a hash table whose keys and values
   both assume fixnum subscripts."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, the
   diorism of which lays its amplection around such functions as
   ``format'' and ``write-char'', a limited set of forbisens adduced."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its agency as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-`NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun supputate-the-jump-table-for (code)
  "Creates and returns a fresh ``jump-table'' dedicated to the castaldy
   of the jump points' vincula in the piece of అంతులేనిబైనరీలు source
   code by adminiculum of their zero-based positions inside this
   string."
  (declare (type string code))
  (let ((jump-table   (make-hash-table :test #'eql))
        ([-positions  NIL)
        (\(-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) [-positions))
    (declare (type (list-of fixnum) \(-positions))
    (dotimes (current-position (length code))
      (declare (type fixnum current-position))
      (case (char code current-position)
        (#\[
          (push current-position [-positions))
        (#\]
          (if [-positions
            (let (([-point (pop [-positions))
                  (]-point current-position))
              (declare (type fixnum [-point))
              (declare (type fixnum ]-point))
              (psetf
                (gethash [-point jump-table) ]-point
                (gethash ]-point jump-table) [-point))
            (error "Unmatched \"]\" instruction at position ~d."
              current-position)))
        (#\(
          (push current-position \(-positions))
        (#\)
          (if \(-positions
            (let ((\(-point (pop \(-positions))
                  (\)-point current-position))
              (declare (type fixnum \(-point))
              (declare (type fixnum \)-point))
              (psetf
                (gethash \(-point jump-table) \)-point
                (gethash \)-point jump-table) \(-point))
            (error "Unmatched \")\" instruction at position ~d."
              current-position)))
        (otherwise
          NIL)))
    
    (the jump-table
      (cond
        ([-positions
          (error "Unmatched \"[\" instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length [-positions)
            [-positions))
        (\(-positions
          (error "Unmatched \"(\" instruction~p at position~:p ~
                  ~{~d~^, ~}."
            (length \(-positions)
            \(-positions))
        (T
          jump-table)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory's bit tape.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Bit-Tape
  (:constructor prepare-a-pristine-bit-tape ()))
  "The ``Bit-Tape'' class applies itself to the program memory's
   furnishment, the bailiwick of which is realized in the castaldy of
   a bilaterally infinite dispansion of bit-valued cells, concomitantly
   entalented with an amenability to a mobile cell pointer whose wike
   imposes the currently active unit's selection, thilk presents the
   aefauld member at any instant admitted for perquisitions and
   modulations."
  (bits                         #b0
                                :type      (unsigned-byte *)
                                :read-only NIL)
  (pointer                      0
                                :type      integer
                                :read-only NIL)
  (smallest-accessed-cell-index 0
                                :type      integer
                                :read-only NIL)
  (largest-accessed-cell-index  0
                                :type      integer
                                :read-only NIL))

;;; -------------------------------------------------------

(defun translate-the-cell-index-into-a-bit-offset (tape cell-index)
  "Returns the non-negative bit offset into the TAPE's integer-encoded
   binary sequence corresponding to the signed integer CELL-INDEX."
  (declare (type Bit-Tape tape))
  (declare (type integer  cell-index))
  (the (integer 0 *)
    (- cell-index
       (bit-tape-smallest-accessed-cell-index tape))))

;;; -------------------------------------------------------

(defun translate-the-pointer-into-a-bit-offset (tape)
  "Returns the non-negative bit offset into the TAPE's integer-encoded
   binary sequence corresponding to its signed cell pointer position."
  (declare (type Bit-Tape tape))
  (the (integer 0 *)
    (translate-the-cell-index-into-a-bit-offset tape
      (bit-tape-pointer tape))))

;;; -------------------------------------------------------

(defun bit-at-the-index-is-on-p (tape cell-index)
  "Determines whether the bit TAPE cell at the specified CELL-INDEX
   ostends the bit state one (1), returning on confirmation a
   ``boolean'' value of ``T''; otherwise, for a zero-valued (0) cell,
   responds with ``NIL''."
  (declare (type Bit-Tape tape))
  (declare (type integer  cell-index))
  (the boolean
    (resolve-to-a-boolean-value
      (logbitp
        (translate-the-cell-index-into-a-bit-offset tape cell-index)
        (bit-tape-bits                              tape)))))

;;; -------------------------------------------------------

(defun request-the-bit-at-the-index (tape cell-index)
  "Returns the bit value stored in the bit TAPE amenable to the
   CELL-INDEX."
  (declare (type Bit-Tape tape))
  (declare (type integer  cell-index))
  (the bit
    (if (bit-at-the-index-is-on-p tape cell-index)
      1
      0)))

;;; -------------------------------------------------------

(defun request-the-current-bit (tape)
  "Returns the bit value stored in the bit TAPE's currently active
   cell."
  (declare (type Bit-Tape tape))
  (the bit
    (request-the-bit-at-the-index tape
      (bit-tape-pointer tape))))

;;; -------------------------------------------------------

(defun set-the-current-bit (tape new-state)
  "Stores the NEW-STATE in the bit TAPE's currently active cell and
   returns no value."
  (declare (type Bit-Tape tape))
  (let ((bit-offset (translate-the-pointer-into-a-bit-offset tape)))
    (declare (type (integer 0 *) bit-offset))
    (setf (ldb (byte 1 bit-offset)
               (bit-tape-bits tape))
          new-state))
  (values))

;;; -------------------------------------------------------

(defun flip-the-current-bit (tape)
  "Flips, or negates, the value stored in the bit TAPE's currently
   active cell and returns no value."
  (declare (type Bit-Tape tape))
  (set-the-current-bit tape
    (- 1
      (request-the-current-bit tape)))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Translates the bit TAPE's cell pointer one step along the dextral
   airt and returns no value."
  (declare (type Bit-Tape tape))
  (incf (bit-tape-pointer tape))
  (setf (bit-tape-largest-accessed-cell-index tape)
    (max
      (bit-tape-largest-accessed-cell-index tape)
      (bit-tape-pointer                     tape)))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Translates the bit TAPE's cell pointer one step along the sinistral
   airt and returns no value."
  (declare (type Bit-Tape tape))
  (decf (bit-tape-pointer tape))
  (setf (bit-tape-smallest-accessed-cell-index tape)
    (min
      (bit-tape-smallest-accessed-cell-index tape)
      (bit-tape-pointer                     tape)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((tape Bit-Tape) (stream T))
  (declare (type Bit-Tape    tape))
  (declare (type destination stream))
  (loop
    initially
      (fresh-line)
    for current-cell-index
      of-type (integer 0 *)
      from    (bit-tape-smallest-accessed-cell-index tape)
      to      (bit-tape-largest-accessed-cell-index  tape)
    for current-cell-value
      of-type bit
      =       (request-the-bit-at-the-index tape current-cell-index)
    for processes-the-current-cell-p
      of-type boolean
      =       (resolve-to-a-boolean-value
                (= current-cell-index
                   (bit-tape-pointer tape)))
    do
      (format stream "~:[ ~d ~;[~d]~]"
        processes-the-current-cell-p
        current-cell-value))
  (the Bit-Tape tape))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-అంతులేనిబైనరీలు (code)
  "Interprets the piece of అంతులేనిబైనరీలు source CODE and returns
   no value."
  (declare (type string code))
  
  (let ((ip             0)
        (jump-points    (supputate-the-jump-table-for code))
        (infinite-loops NIL)
        (tape           (prepare-a-pristine-bit-tape)))
    (declare (type fixnum           ip))
    (declare (type jump-table       jump-points))
    (declare (type (list-of fixnum) infinite-loops))
    (declare (type Bit-Tape         tape))
    
    (symbol-macrolet
        ((program-is-exhausted-p
          (the boolean
            (resolve-to-a-boolean-value
              (>= ip (length code))))))
      (declare (type boolean program-is-exhausted-p))
      
      (flet
          ((jump-if-the-current-cell-contains (guard)
            "Determines whether the bit TAPE's current cell value equals
             the GUARD, on confirmation relocating the instruction
             pointer (IP) to the affiliated obverse jump point in the
             CODE; otherwise accompasses no causatum."
            (declare (type bit guard))
            (when (= (request-the-current-bit tape) guard)
              (setf ip
                (gethash ip jump-points)))
            (values)))
        
        (loop do
          (case (char code ip)
            (#\+
              (flip-the-current-bit tape))
            
            (#\>
              (move-the-cell-pointer-right tape))
            
            (#\<
              (move-the-cell-pointer-left tape))
            
            (#\.
              (format T "~d"
                (request-the-current-bit tape)))
            
            (#\,
              (format T "~&Please input 0 or 1: ")
              (finish-output)
              (set-the-current-bit tape
                (parse-integer
                  (read-line NIL NIL "0")
                  :radix 2))
              (clear-input))
            
            (#\[
              (jump-if-the-current-cell-contains 0))
            
            (#\]
              (jump-if-the-current-cell-contains 1))
            
            (#\(
              (jump-if-the-current-cell-contains 1))
            
            (#\)
              (jump-if-the-current-cell-contains 0))
            
            (#\{
              (push ip infinite-loops))
            
            (#\}
              (if infinite-loops
                (pop infinite-loops)
                (error "No infinite loop remains to be terminated with ~
                        the instruction \"}\" at position ~d."
                  ip)))
            
            (otherwise
              NIL))
          
          (incf ip)
          
          (cond
            ((and program-is-exhausted-p
                  infinite-loops)
              (setf ip
                (pop infinite-loops)))
            (program-is-exhausted-p
              (loop-finish))
            (T
              NIL))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating binary cat program.
(interpret-అంతులేనిబైనరీలు "{,.")

;;; -------------------------------------------------------

;; Print the bit sequence "11111011111".
(interpret-అంతులేనిబైనరీలు "+.....>.<.....")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-అంతులేనిబైనరీలు ",[.].")

;;; -------------------------------------------------------

;; Alternative truth-machine implementation, which relies on an infinite
;; loop's termination via the "}" instruction.
(interpret-అంతులేనిబైనరీలు ",{.(}+)")

;;; -------------------------------------------------------

;; Perpetually print the alternating bit sequence "0101...".
(interpret-అంతులేనిబైనరీలు "{.+")
