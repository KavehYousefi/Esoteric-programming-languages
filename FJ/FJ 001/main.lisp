;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; lanugage "FJ", invented by the Esolang user "None1" and presented
;; on August 7th, 2023, conceived as a derivation of Urban Mueller's
;; "brainfuck", while, maugre its patration in the quantitative and
;; syntactical congruency with the entheus, deploys a set of identifiers
;; whose establishment's choice ensues from an eather symbol typing
;; principle's accommodation, restricted to the keyboard keys "F" and
;; "J" --- in their minuscular forms --- if naited on a QWERTY layout.
;; 
;; 
;; Concept
;; =======
;; The FJ programming language constitutes an equipollent to its
;; brainfuck entheus, the lealty's incarnation neither of the wite's
;; tholance to deviate from the basic tenets in the program execution,
;; neither from the data castaldy's mode and epiphenomena; however, the
;; diorism's contribution establishes a variation on the octuple
;; instruction names, the default one-symbol identifiers experiencing
;; a supersession by combinations of the Latin minuscules "f" and "j",
;; whose spatial distribution --- empight immediately alow the index
;; fingers on a QWERTY keyboard --- accommodates a mode of access more
;; aligned with concinnity in a source string's assemblage.
;; 
;; == FJ: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; FJ's kenspeckle designment is edified upon the provision of
;; succedanea to brainfuck's single-character instruction identifiers
;; by combinations of the letters "f" and "j", desumed from the most
;; convenient positions on the QWERTY keyboard, immediately alow the
;; index fingers, and alligated into trisulc units.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of FJ's recipiency does not elude brainfuck's
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
;; identifiers' semantical aspects to its brainfuck heritage, FJ's
;; compass does neither actuate an ostention's commission designed with
;; supererogation with respect to its provenance, nor a curtailment in
;; the competences; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the FJ programming language's facilities,
;; thilk concomitantly concur with the offerings of its brainfuck
;; stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   fff     | Increments the current cell value by one (1). If the new
;;           | state transgresses the upper march of 255, the value
;;           | wraps around to the lower extremum of zero (0).
;;   ..................................................................
;;   ffj     | Decrements the current cell value by one (1). If the new
;;           | state transgresses the lower march of zero (0), the
;;           | value wraps around to the upper extremum of 255.
;;   ..................................................................
;;   fjj     | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   fjf     | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ..................................................................
;;   jff     | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   jfj     | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   jjf     | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "jjj" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   jjj     | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "jjf" instruction;
;;           | otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == FJ AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation, FJ's patration in language twissel's replication
;; homologates an equiparation applying to the operative bailiwick:
;; 
;;   -----------------------------------------------
;;   FJ  | brainfuck | Causatum
;;   ----+-----------+------------------------------
;;   fff | +         | Increment the current cell.
;;   ...............................................
;;   ffj | -         | Decrement the current cell.
;;   ...............................................
;;   fjf | ,         | Input into the current cell.
;;   ...............................................
;;   fjj | .         | Print the current cell.
;;   ...............................................
;;   jff | <         | Move the cell pointer left.
;;   ...............................................
;;   jfj | >         | Move the cell pointer right.
;;   ...............................................
;;   jjf | [         | Jump forward if zero.
;;   ...............................................
;;   jjj | ]         | Jump back if not zero.
;;   -----------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the FJ source code
;; string towards a transcription into the more queming brainfuck
;; foundry, whence ensues the actual execution process.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-31
;; 
;; Sources:
;;   [esolang2024:FJ]
;;   The Esolang contributors, "FJ", September 14th, 2024
;;   URL: "https://esolangs.org/wiki/FJ"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the bespoke types.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype positional-stack ()
  "The ``positional-stack'' type defines a stack dedicated to the
   castaldy of string indices, and realized in an adjustable vector's
   guise, the elements of which subsume into the fixnum type."
  '(vector fixnum *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping betwixt a
   brainfuck program's jump points, mediated by adminiculum of their
   zero-based position into the source code string, with a manifestation
   that founds upon a simple array of fixnum elements, equinumerant in
   their accompt to the program string's character count, every element
   at a jump instruction's position comprehending the index of its
   opposite destination point, while all other entries acquire the -1
   sentinel."
  '(simple-array fixnum (*)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value whose componency
   enumerates an octuple catena of accolent bits, thus rendering a
   dispansion across the closed integral interval [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either producing a fresh
   specimen of the optated type, or, upon an extant subsumption into
   this specialization, responds with the unmodified SOURCE itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun convert-into-a-simple-base-string (source)
  "Converts the SOURCE into a simple base string, either producing a
   fresh specimen of the optated type, or, upon an extant subsumption
   into this specialization, responds with the unmodified SOURCE
   itself."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sliding window.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Sliding-Window ()
  ((source
    :initarg       :source
    :initform      (error "No source string has been provided.")
    :type          simple-string
    :documentation "The piece of FJ source code to analyze.")
   (token-start-point
    :initform      0
    :type          fixnum
    :documentation "The index of the TOKEN's first character into the
                    SOURCE string.")
   (token-end-point
    :type          fixnum
    :documentation "A tmema desumed from the SOURCE string commencing
                    at the inclusive POSITION, and tallying at most
                    three (3) characters."))
  (:documentation
    "The ``Sliding-Window'' class is apportioned the dever of a
     motile string tmema iterator, each \"window\" size's constraint
     applying to a maximum of three characters, as an equinumerant in
     the pleroma's case with the uniform FJ identifiers' diorism."))

;;; -------------------------------------------------------

(defun update-the-sliding-window-token (window)
  "Updates the sliding WINDOW's token end point with respect to its
   start point's contemporaneous location in the underlying source and
   returns no value."
  (declare (type Sliding-Window window))
  (with-slots (source token-start-point token-end-point) window
    (declare (type simple-string source))
    (declare (type fixnum        token-start-point))
    (declare (type fixnum        token-end-point))
    (setf token-end-point
      (min
        (length source)
        (+ token-start-point 3))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((window Sliding-Window) &key)
  "Initializes the sliding WINDOW's token and returns no value."
  (declare (type Sliding-Window window))
  (update-the-sliding-window-token window)
  (values))

;;; -------------------------------------------------------

(defun prepare-a-sliding-window-for (source)
  "Creates and returns a fresh ``Sliding-Window'' dedicated to the
   traversal of the piece of FJ SOURCE code."
  (declare (type string source))
  (the Sliding-Window
    (make-instance 'Sliding-Window
      :source (convert-into-a-simple-string source))))

;;; -------------------------------------------------------

(defun advance-the-sliding-window (window &optional (step-size 1))
  "Translates the sliding WINDOW forward by the STEP-SIZE, thilk
   defaults to one (1), and returns no value."
  (declare (type Sliding-Window window))
  (declare (type (integer 0 3)  step-size))
  (with-slots (source token-start-point) window
    (declare (type simple-string source))
    (declare (type fixnum        token-start-point))
    (setf token-start-point
      (min
        (+ token-start-point step-size)
        (length source))))
  (update-the-sliding-window-token window)
  (values))

;;; -------------------------------------------------------

(defun measure-the-token-size (window)
  "Returns the tally of characters demarcated by the sliding WINDOW's
   current token, which is capacitated to be an incolant of the closed
   integral interval [0, 3]."
  (declare (type Sliding-Window window))
  (the (integer 0 3)
    (with-slots (token-start-point token-end-point) window
      (declare (type fixnum token-start-point))
      (declare (type fixnum token-end-point))
      (- token-end-point token-start-point))))

;;; -------------------------------------------------------

(defun more-tokens-follow-p (window)
  "Determines whether the sliding WINDOW's currently demarcated token
   range accommodates the minimum of a trisulk in characters, pursuing
   to comply with the FJ instruction identifier set's impositions,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Sliding-Window window))
  (the boolean
    (with-slots (token-start-point token-end-point) window
      (declare (type fixnum token-start-point))
      (declare (type fixnum token-end-point))
      (not (null
        (>= (measure-the-token-size window)
            3))))))

;;; -------------------------------------------------------

(defun sliding-window-matches-p (window expected-identifier)
  "Determines whether the sliding WINDOW's contemporaneously demarcated
   token replicates the EXPECTED-IDENTIFIER in its content, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Sliding-Window window))
  (declare (type string         expected-identifier))
  (the boolean
    (with-slots (source token-start-point token-end-point) window
      (declare (type simple-string source))
      (declare (type fixnum        token-start-point))
      (declare (type fixnum        token-end-point))
      (not (null
        (string= source expected-identifier
          :start1 token-start-point
          :end1   token-end-point))))))

;;; -------------------------------------------------------

(defun parse-the-current-brainfuck-token (window)
  "Returns the brainfuck instruction identifier corresponding to the
   sliding WINDOW's currently demarcated trisulk letter token; or, upon
   a carency in any owelty to such, responds with the ``NIL'' sentinel."
  (declare (type Sliding-Window window))
  (the (or null standard-char)
    (cond
      ((sliding-window-matches-p window "fff") #\+)
      ((sliding-window-matches-p window "ffj") #\-)
      ((sliding-window-matches-p window "fjf") #\,)
      ((sliding-window-matches-p window "fjj") #\.)
      ((sliding-window-matches-p window "jff") #\<)
      ((sliding-window-matches-p window "jfj") #\>)
      ((sliding-window-matches-p window "jjf") #\[)
      ((sliding-window-matches-p window "jjj") #\])
      (T                                       NIL))))

;;; -------------------------------------------------------

(defun translate-into-brainfuck (window)
  "Converts the FJ program represented by the sliding WINDOW into its
   brainfuck equivalent and returns a fresh ``simple-base-string''
   comprehending the decoded instruction symbols."
  (declare (type Sliding-Window window))
  (the simple-base-string
    (convert-into-a-simple-base-string
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (loop
          while (more-tokens-follow-p window)
          
          for brainfuck-token
            of-type (or null standard-char)
            =       (parse-the-current-brainfuck-token window)
          
          if brainfuck-token do
            (write-char brainfuck-token brainfuck-code)
            (advance-the-sliding-window window 3)
          else do
            (advance-the-sliding-window window 1)
          end)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the positional stack.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-positional-stack ()
  "Creates and returns an initially vacant ``positional-stack''."
  (the positional-stack
    (make-array 0
      :element-type    'fixnum
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))

;;; -------------------------------------------------------

(defun push-onto-the-stack (stack new-element)
  "Inserts the NEW-ELEMENT at the positional STACK's top position and
   returns no value."
  (declare (type positional-stack stack))
  (declare (type fixnum           new-element))
  (vector-push-extend new-element stack)
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-stack (stack)
  "Removes and returns the positional STACK's top element."
  (declare (type positional-stack stack))
  (the fixnum
    (prog1
      (aref stack
        (1- (fill-pointer stack)))
      (decf (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun stack-is-empty-p (stack)
  "Determines whether the positional STACK is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type positional-stack stack))
  (the boolean
    (not (null
      (zerop
        (fill-pointer stack))))))

;;; -------------------------------------------------------

(defun count-the-stack-elements (stack)
  "Returns the tally of elements partaking of the positional STACK."
  (declare (type positional-stack stack))
  (the fixnum
    (fill-pointer stack)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-jump-table (size)
  "Creates and returns a fresh ``jump-table'' endowed with a SIZE
   expected to be greater than or equal to the brainfuck code upon which
   it is edified."
  (declare (type fixnum size))
  (the jump-table
    (make-array size
      :element-type    'fixnum
      :initial-element -1
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (connections start-point end-point)
  "Connects the jump START-POINT and END-POINT in the CONNECTIONS table
   and returns no value."
  (declare (type jump-table connections))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (aref connections start-point) end-point
    (aref connections end-point)   start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (brainfuck-code)
  "Creates and returns a fresh ``jump-table'' dedicated to the castaldy
   of the vincula betwixt the piece of BRAINFUCK-CODE's jump
   instructions, mediated by adminiculum of their zero-based indices
   into the same."
  (declare (type simple-base-string brainfuck-code))
  (let ((connections
          (prepare-an-empty-jump-table
            (length brainfuck-code)))
        (forward-jump-points
          (prepare-an-empty-positional-stack)))
    (declare (type jump-table       connections))
    (declare (type positional-stack forward-jump-points))
    (loop
      for current-token    of-type character across brainfuck-code
      for current-position of-type fixnum    from   0 by 1
      
      if (char= current-token #\[) do
        (push-onto-the-stack forward-jump-points current-position)
      else if (char= current-token #\]) do
        (if (stack-is-empty-p forward-jump-points)
          (error "Unmatched back jump instruction.")
          (connect-the-jump-points connections
            (pop-from-the-stack forward-jump-points)
            current-position))
      end
      
      finally
        (unless (stack-is-empty-p forward-jump-points)
          (error "Unmatched forward jump instruction~p."
            (count-the-stack-elements forward-jump-points))))
    (the jump-table connections)))

;;; -------------------------------------------------------

(defun locate-the-opposite-jump-point (connections point-of-departure)
  "Expecting the POINT-OF-DEPARTURE to refer to a forward or back
   jump instruction's position in the CONNECTIONS table, returns the
   index of the opposite end point; or, upon its disrespondency, signals
   an error of an unspecified type."
  (declare (type jump-table connections))
  (declare (type fixnum     point-of-departure))
  (let ((destination-point (aref connections point-of-departure)))
    (declare (type fixnum destination-point))
    (the fixnum
      (if (minusp destination-point)
        (error "No destination associated with the jump point ~d."
          point-of-departure)
        destination-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :type          unsigned-byte
    :documentation "The cell data as an integer-encoded catena of bits,
                    each eight accolent bit elements representating one
                    cell's unsigned byte state, the cells proceeding
                    from the least significant bit (LSB) towards the
                    most significant one (MSB).")
   (minimum-cell-index
    :initform      0
    :type          integer
    :documentation "The smallest cell index traversed by the
                    CELL-POINTER in the course of an FJ program's
                    execution.")
   (cell-pointer
    :initform      0
    :type          integer
    :documentation "The current cell pointer position."))
  (:documentation
    "The ``Tape'' class serves in a bilaterally infinite tape's
     furnishment whose cells each mete a single unsigned byte in
     capacity."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape''."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun translate-the-cell-pointer-into-a-byte-specifier (tape)
  "Returns an implementation-dependent byte specifier which capacitates
   the selection, for both inquisitive and modulating purposes, of the
   eight (8) accolent bits corresponding to the TAPE cell pointer's
   currently active cell in the underlying integer-encoded bit
   sequence."
  (declare (type Tape tape))
  (the T
    (with-slots (cell-pointer minimum-cell-index) tape
      (declare (type integer cell-pointer))
      (declare (type integer minimum-cell-index))
      (byte 8
        (* (- cell-pointer minimum-cell-index) 8)))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's currently
   active cell."
  (declare (type Tape tape))
  (the octet
    (with-slots (bits) tape
      (declare (type unsigned-byte bits))
      (ldb
        (translate-the-cell-pointer-into-a-byte-specifier tape)
        bits))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently active cell,
   contingently preceded by a wrapping around in order to respect the
   unsigned byte interval of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-slots (bits) tape
    (declare (type unsigned-byte bits))
    (setf
      (ldb
        (translate-the-cell-pointer-into-a-byte-specifier tape)
        bits)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun move-left-along-the-tape (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (with-slots (bits cell-pointer minimum-cell-index) tape
    (declare (type unsigned-byte bits))
    (declare (type integer       cell-pointer))
    (declare (type integer       minimum-cell-index))
    (decf cell-pointer)
    (when (< cell-pointer minimum-cell-index)
      (psetf
        minimum-cell-index cell-pointer
        bits               (ash bits 8))))
  (values))

;;; -------------------------------------------------------

(defun move-right-along-the-tape (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (slot-value tape 'cell-pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck interpreter.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainfuck
    (program
     &aux (jump-table   (supputate-the-jump-table-for program))
          (ip           0)
          (program-size (length program))
          (tape         (prepare-a-pristine-tape)))
  "Interprets the brainfuck PROGRAM and returns no value."
  (declare (type simple-base-string program))
  (declare (type jump-table         jump-table))
  (declare (type fixnum             ip))
  (declare (type fixnum             program-size))
  (declare (type Tape               tape))
  (loop while (< ip program-size) do
    (case (schar program ip)
      (#\+
        (incf (current-cell-value tape)))
      (#\-
        (decf (current-cell-value tape)))
      (#\,
        (format        *query-io* "~&>> ")
        (finish-output *query-io*)
        (setf (current-cell-value tape)
          (char-code
            (read-char *query-io* NIL
              (code-char 0))))
        (clear-input *query-io*))
      (#\.
        (format *query-io* "~c"
          (code-char
            (current-cell-value tape))))
      (#\<
        (move-left-along-the-tape tape))
      (#\>
        (move-right-along-the-tape tape))
      (#\[
        (when (zerop (current-cell-value tape))
          (setf ip
            (locate-the-opposite-jump-point jump-table ip))))
      (#\]
        (unless (zerop (current-cell-value tape))
          (setf ip
            (locate-the-opposite-jump-point jump-table ip))))
      (otherwise
        NIL))
    (incf ip))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the FJ interpreter.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-FJ (program)
  "Interprets the FJ PROGRAM and returns no value."
  (interpret-brainfuck
    (translate-into-brainfuck
      (prepare-a-sliding-window-for program)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This encodes the brainfuck program:
;; 
;;   "+[,.]"
;; 
(interpret-FJ "fffjjffjffjjjjj")

;;; -------------------------------------------------------

;; Print the message "Hello World!" to the standard output conduit.
(interpret-FJ "ffffffffffffffffffffffffjjfjfjffffffffffffjjfjfjffffffjfjfffffffffjfjfffffffffjfjfffjffjffjffjffffjjjjjfjfffjfjfffjfjffjjfjjfjfffjjfjffjjjjffffjjjjjfjjfjfjjjfjffjffjffjfjjffffffffffffffffffffffjjfjjffffffffffjjjfjjfjfjjjffffjfjjjfffjjffffffffffjjffjffjffjffjffjffjfjjffjffjffjffjffjffjffjffjfjjjfjjfjffffjjjfjfffffffjj")
