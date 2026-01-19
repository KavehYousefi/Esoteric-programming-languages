;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tol", invented by the Esolang user "Tommyaweosme" and
;; presented on June 14th, 2024, conceived as a derivation of
;; Urban Mueller's "brainfuck" whose sole indicium, with all further
;; aspects ipsissima verba replications of the entheus' notions,
;; appertains to a syntactical deviation from the one-symbol instruction
;; identifiers towards variations on the term "Tol" molded into an
;; exhaustive permutation's installation in majuscular and minuscular
;; choices.
;; 
;; 
;; Concept
;; =======
;; The Tol programming language constitutes an equipollent to its
;; brainfuck entheus, the lealty's incarnation neither of the wite's
;; tholance to deviate from the basic tenets in the program execution,
;; neither from the data castaldy's mode and epiphenomena; however, the
;; diorism's contribution establishes a variation on the octuple
;; instruction names, the default one-symbol identifiers experiencing
;; a supersession by permutations of the majuscules and minuscules
;; forming variants of the term "Tol".
;; 
;; == TOL: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; Tol's kenspeckle physiognomy betokens its provenance in a cambistry's
;; application with the operation identifiers, transitioning from the
;; one-symbol originals to variations on the word "Tol" with the merists
;; defined in the letter's cases as the distinguishing formula.
;; 
;; == IDENTIFIERS DO NOT REQUIRE MERISTS ==
;; A further caract inherent to the language, the statement of operation
;; names does not tharf any sepiment's involvement commorant in the
;; interstices; albeit any content atwixen such will be subjected to
;; neglect.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of Tol's recipiency does not elude brainfuck's
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
;; identifiers' semantical aspects to its brainfuck heritage, Tol's
;; compass does neither actuate an ostention's commission designed with
;; supererogation with respect to its provenance, nor a curtailment in
;; the competences; in corollary, this cleronomy accounts for an octuple
;; contingency, amplecting in its compass the cell pointer movement,
;; basic arithmetics, input and output facilities, as well as an aefauld
;; construct for the control flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the Tol programming language's facilities,
;; thilk concomitantly concur with the offerings of its brainfuck
;; stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   tol     | Increments the current cell value by one (1). If the new
;;           | state transgresses the upper march of 255, the value
;;           | wraps around to the lower extremum of zero (0).
;;   ..................................................................
;;   Tol     | Decrements the current cell value by one (1). If the new
;;           | state transgresses the lower march of zero (0), the
;;           | value wraps around to the upper extremum of 255.
;;   ..................................................................
;;   tOl     | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   tOL     | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   toL     | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "TOL" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   TOL     | If the current cell value does not equal zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "toL" instruction;
;;           | otherwise proceeds as usual.
;;   ..................................................................
;;   TOl     | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   ToL     | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ------------------------------------------------------------------
;; 
;; == TOL AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation, Tol's patration in language twissel's replication
;; homologates an equiparation applying to the operative bailiwick:
;; 
;;   -----------------------------------------------
;;   Tol | brainfuck | Causatum
;;   ----+-----------+------------------------------
;;   tol | +         | Increment the current cell.
;;   ...............................................
;;   Tol | -         | Decrement the current cell.
;;   ...............................................
;;   tOl | <         | Move the cell pointer left.
;;   ...............................................
;;   tOL | >         | Move the cell pointer right.
;;   ...............................................
;;   toL | [         | Jump forward if zero.
;;   ...............................................
;;   TOL | ]         | Jump back if not zero.
;;   ...............................................
;;   TOl | .         | Print the current cell.
;;   ...............................................
;;   ToL | ,         | Input into the current cell.
;;   -----------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the Tol source code
;; string's transcription into dedicated instruction representations
;; inwith whose potential an enhaused capacity for eath evaluation
;; wones, ere these entities ultimate vouchsafement of actual efficacy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-12
;; 
;; Sources:
;;   [esolang2025:Tol]
;;   The Esolang contributors, "Tol", November 24th, 2025
;;   URL: "https://esolangs.org/wiki/Tol"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes the NAME's
   dation, and which acts as a pernor to the LAMBDA-LIST's ipsissima
   verba specifications as its personal formal parameters, concomitantly
   assigning the probed object to the CANDIDATE-NAME, evaluates the BODY
   forms, and construes the desinent form's primary return value as the
   docimasy's adjudgment, a \"generalized boolean\" truth value of
   \"true\" peracting a successful compatibility's assessment's
   signification, while a \"false\" response concludes in the
   candidate's rejection.
   ---
   The first BODY form, in the case of its resolution to a string
   object, is adhibited the role of a documentation string to the type
   definition, being, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
       ,(or (and (stringp (first body))
               (pop body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency tallying zero or more entries, everichon member among
   these dimidiated into a key compliant with the KEY-TYPE and an allied
   value of the VALUE-TYPE, both governed by a configuration which
   assigns the generic sentinel ``*'' as the default state."
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
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*))
  "The ``list-of'' type defines a linked list comprehending zero or more
   members, each element partaking of the same complying with the
   ELEMENT-TYPE, for thilk is specified the generic sentinel ``*'' as
   the default."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on Tol
   instructions."
  '(member
    :increment
    :decrement
    :move-left
    :move-right
    :jump-forward
    :jump-back
    :output
    :input))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Tol program as a
   one-dimensional simple array of ``instruction'' objects."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype position-map ()
  "The ``position-map'' type defines zero or more associations betwixt
   positions or indices, realized as a hash table whose keys and values
   both partake of a subsumption into the ``fixnum'' type."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   (8) attiguous bits, thus forming an incolant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations, the
   circumference appropriated by the dispands across, among others, the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-a-program-from (instructions)
  "Creates and returns a fresh ``program'' comprehending the
   INSTRUCTIONS in their specified order."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (source desideratum start-position)
  "Determines whether the DESIDERATUM commences at the inclusive
   START-POSITION in the SOURCE string, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (declare (type simple-string desideratum))
  (declare (type fixnum        start-position))
  (the boolean
    (convert-into-a-boolean-value
      (string= source desideratum
        :start1 start-position
        :end1   (min
                  (+ start-position
                     (length desideratum))
                  (length source))))))

;;; -------------------------------------------------------

(defun probe-the-substring (source desideratum start-position
                            return-value-on-success)
  "Determines whether the DESIDERATUM commences at the inclusive
   START-POSITION in the SOURCE and returns two values:
     (1) If a match could be attested, the RETURN-VALUE-ON-SUCCESS,
         otherwise the ``NIL'' sentinel.
     (2) If a match could be attested, the position into the SOURCE
         immediately succeeding the equiparated tmema; otherwise the
         position immediately following the START-POSITION."
  (declare (type simple-string source))
  (declare (type simple-string desideratum))
  (declare (type fixnum        start-position))
  (declare (type T             return-value-on-success))
  (the (values T fixnum)
    (if (substring-starts-at-p source desideratum start-position)
      (values
        return-value-on-success
        (+ start-position
           (length desideratum)))
      (values
        NIL
        (1+ start-position)))))

;;; -------------------------------------------------------

(defun convert-into-a-simple-string (source)
  "Converts the SOURCE into a simple string, either by returning a
   freshly created object of the optated type, upon the SOURCE's failure
   to comply with its stipulations, or, if the SOURCE already subsumes
   into the ``simple-string'' species, by delivering the SOURCE itself.
   ---
   The SOURCE will not be modulated in any case."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Tol code parser.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-the-instruction-at (source start-position)
  "Commencing with the inclusive START-POSITION in the SOURCE, attents
   to recognize and extract a Tol instruction and returns two values:
     (1) If the character treble commencing at the START-POSITION
         affiliates with a Tol instruction, a connable representation
         thereof; otherwise the ``NIL'' sentinel.
     (2) If the character treble commencing at the START-POSITION
         affiliates with a Tol instruction, the position into the SOURCE
         immediately succeeding the matched tmema; otherwise the index
         immediately succeeding the START-POSITION."
  (declare (type simple-string source))
  (declare (type fixnum        start-position))
  (the (values (or null instruction) fixnum)
    (let ((extracted-instruction NIL)
          (new-position          0))
      (declare (type (or null instruction) extracted-instruction))
      (declare (type fixnum                new-position))
      (flet
          ((probe-the-token (identifier resulting-instruction)
            "Determines whether the IDENTIFIER matches the SOURCE string
             tmema commencing at the inclusive START-POSITION,
             configures the EXTRACTED-INSTRUCTION and NEW-POSITION
             variables accordingly, and returns the
             EXTRACTED-INSTRUCTION's thus modulated value."
            (declare (type simple-string identifier))
            (declare (type instruction   resulting-instruction))
            (multiple-value-setq (extracted-instruction new-position)
              (probe-the-substring
                source
                identifier
                start-position
                resulting-instruction))
            (the (or null instruction) extracted-instruction)))
        (or (probe-the-token "tol" :increment)
            (probe-the-token "Tol" :decrement)
            (probe-the-token "tOl" :move-left)
            (probe-the-token "tOL" :move-right)
            (probe-the-token "toL" :jump-forward)
            (probe-the-token "TOL" :jump-back)
            (probe-the-token "TOl" :output)
            (probe-the-token "ToL" :input)))
      (values extracted-instruction new-position))))

;;; -------------------------------------------------------

(defun extract-the-tol-instructions (code)
  "Parses the piece of Tol source CODE and returns a one-dimensional
   simple array comprehending its extracted instructions."
  (declare (type simple-string code))
  (the program
    (assemble-a-program-from
      (let ((extracted-instruction NIL)
            (current-position      0))
        (declare (type (or null instruction) extracted-instruction))
        (declare (type fixnum                current-position))
        (loop
          until (>= current-position (length code)) do
            (multiple-value-setq (extracted-instruction
                                  current-position)
              (extract-the-instruction-at code current-position))
          when extracted-instruction
            collect extracted-instruction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :accessor      jump-table-connections
    :type          position-map
    :documentation "Affiliates the forward and back jump instructions
                    in a bidirectional fashion per procurationem of
                    their zero-based indices into the underlying
                    program's instruction vector."))
  (:documentation
    "The ``Jump-Table'' class applied itself to the castaldy of the
     bidirectional vincula betwixt the jump points in a Tol program by
     adminiculum of their zero-based positions into the program's
     instruction vector."))

;;; -------------------------------------------------------

(defun prepare-a-vacant-jump-table ()
  "Creates and returns a `Jump-Table'' whose state at this instant of
   inchoacy amounts to a plene vacancy."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Connects the jump points designated by the zero-based indices
   START-POINT and END-POINT in the jump TABLE and returns no value.
   ---
   Any extant entry whose key conflates with either the START-POINT, the
   END-POINT, or both, will be subjected to a tacit supersession by the
   new twain."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-slots (connections) table
    (declare (type position-map connections))
    (psetf
      (gethash start-point connections) end-point
      (gethash end-point   connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun locate-the-destination-jump-point (table point-of-departure)
  "Returns the zero-based position of the jump point coalized with the
   POINT-OF-DEPARTURE in the jump TABLE; or, upon its direspondency,
   signals an error of an unspecified type."
  (declare (type Jump-Table table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure
          (jump-table-connections table))
        (error "No destination exists for the jump point ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (program)
  "Builds and returns a fresh ``Jump-Table'' dedicated to the vincula's
   contexture betwixt the matching jump points in the Tol PROGRAM,
   mediated by adminiculum of their zero-based positions inwith its
   instruction vector."
  (declare (type program program))
  (the Jump-Table
    (let ((jump-points  (prepare-a-vacant-jump-table))
          (start-points NIL))
      (declare (type Jump-Table       jump-points))
      (declare (type (list-of fixnum) start-points))
      (dotimes (current-position (length program))
        (case (aref program current-position)
          (:jump-forward
            (push current-position start-points))
          (:jump-back
            (if start-points
              (connect-the-jump-points jump-points
                (pop start-points)
                current-position)
              (error "An unmatched back jump point has been ~
                      detected.")))
          (otherwise
            NIL)))
      (if start-points
        (error "One or more unmatched forwad jump points have been ~
                detected.")
        jump-points))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory tape.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((bits
    :initform      #b00000000
    :accessor      tape-bits
    :type          unsigned-byte
    :documentation "Maintains the explicitly sojourned cells' unsigned
                    byte states in an integer-encoded bit sequence,
                    each accolent octuple of positions allying with one
                    cell, the least significant bit (LSB) locations
                    chosen such as to answer to the lowest cell index.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          integer
    :documentation "The current cell pointer position.")
   (smallest-accessed-cell-index
    :initform      0
    :accessor      tape-smallest-accessed-cell-index
    :type          integer
    :documentation "The minimum cell index traversed by the cell
                    POINTER during a program's execution."))
  (:documentation
    "The ``Tape'' class serves in a bidirectionally infinite, unsigned
     byte-valued cell catena's furnishment, thilk is founded upon an
     integer-encoded bit sequence as the data castaldy's mechanism."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-tape ()
  "Creates and returns a fresh ``Tape'', empight at its inchoacy in the
   default state comprising merely zero-valued cells."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun locate-the-selected-tape-bits (tape)
  "Returns an implementation-dependent byte specifier which locates the
   eight (8) accolent bits comprising the TAPE's currently selected
   cell."
  (declare (type Tape tape))
  (the T
    (byte 8
      (* (- (tape-pointer                      tape)
            (tape-smallest-accessed-cell-index tape))
         8))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value commorant in the TAPE's currently
   selected cell."
  (declare (type Tape tape))
  (the octet
    (ldb
      (locate-the-selected-tape-bits tape)
      (tape-bits                     tape))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's currently selected cell,
   contingently preceded by its wrapping into the admissible unsigned
   byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf
    (ldb
      (locate-the-selected-tape-bits tape)
      (tape-bits                     tape))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step in a dextrosinistral
   airt and returns no value."
  (declare (type Tape tape))
  (with-slots (bits pointer smallest-accessed-cell-index) tape
    (declare (type unsigned-byte bits)
             (ignorable          bits))
    (declare (type integer       pointer))
    (declare (type integer       smallest-accessed-cell-index))
    (decf pointer)
    (when (< pointer smallest-accessed-cell-index)
      (psetf
        smallest-accessed-cell-index pointer
        bits                         (ash bits 8))))
  (values))

;;; -------------------------------------------------------

(defun move-the-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step in a sinistrodextral
   airt and returns no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-the-tol-program (program)
  "Executes the Tol PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (supputate-the-jump-table-for program))
        (tape       (prepare-a-pristine-tape)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    (declare (type Tape       tape))
    (loop while (< ip (length program)) do
      (case (aref program ip)
        (:increment
          (incf (current-cell-value tape)))
        (:decrement
          (decf (current-cell-value tape)))
        (:move-left
          (move-the-cell-pointer-left tape))
        (:move-right
          (move-the-cell-pointer-right tape))
        (:jump-forward
          (when (zerop (current-cell-value tape))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (:jump-back
          (unless (zerop (current-cell-value tape))
            (setf ip
              (locate-the-destination-jump-point jump-table ip))))
        (:output
          (format T "~c"
            (code-char
              (current-cell-value tape))))
        (:input
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value tape)
            (char-code
              (read-char NIL NIL #\null)))
          (clear-input))
        (otherwise
          (error "The instruction ~a cannot be processed."
            (aref program ip))))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-tol-code (code)
  "Interprets the piece of Tol source CODE and returns no value."
  (declare (type string code))
  (execute-the-tol-program
    (extract-the-tol-instructions
      (convert-into-a-simple-string code)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck code parser.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-the-brainfuck-instructions (code)
  "Parses the piece of brainfuck source CODE and returns a
   one-dimensional simple array ensconcing its instructions."
  (declare (type simple-string code))
  (the program
    (let ((optimized-code (convert-into-a-simple-string code)))
      (declare (type simple-string optimized-code))
      (assemble-a-program-from
        (loop for token of-type character across optimized-code append
          (case token
            (#\+       '(:increment))
            (#\-       '(:decrement))
            (#\<       '(:move-left))
            (#\>       '(:move-right))
            (#\,       '(:input))
            (#\.       '(:output))
            (#\[       '(:jump-forward))
            (#\]       '(:jump-back))
            (otherwise NIL)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck code generator.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-program-into-brainfuck (program
                                             &key (destination NIL))
  "Generates and returns for the PROGRAM the equivalent brainfuck code,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the output."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null simple-string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from   0 by 1
        do
          (format destination "~c"
            (case current-instruction
              (:increment    #\+)
              (:decrement    #\-)
              (:move-left    #\<)
              (:move-right   #\>)
              (:input        #\,)
              (:output       #\.)
              (:jump-forward #\[)
              (:jump-back    #\])
              (otherwise
                (error "The instruction ~a at the position ~d cannot ~
                        be translated into a brainfuck instruction."
                  current-instruction current-position)))))
      (convert-into-a-simple-string
        (with-output-to-string (brainfuck-code)
          (declare (type string-stream brainfuck-code))
          (translate-the-program-into-brainfuck program
            :destination brainfuck-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Tol code generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-program-into-tol (program
                                       &key (destination NIL))
  "Generates and returns for the PROGRAM the equivalent Tol code,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the output."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null simple-string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from   0 by 1
        do
          (format destination "~a"
            (case current-instruction
              (:increment    "tol")
              (:decrement    "Tol")
              (:move-left    "tOl")
              (:move-right   "tOL")
              (:input        "ToL")
              (:output       "TOl")
              (:jump-forward "toL")
              (:jump-back    "TOL")
              (otherwise
                (error "The instruction ~a at the position ~d cannot ~
                        be translated into a Tol instruction."
                  current-instruction current-position)))))
      (convert-into-a-simple-string
        (with-output-to-string (tol-code)
          (declare (type string-stream tol-code))
          (translate-the-program-into-tol program
            :destination tol-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the translation operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-tol-code-into-brainfuck (tol-code
                                              &key (destination NIL))
  "Translates the piece of TOL-CODE into its brainfuck equivalent,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION, the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the result."
  (declare (type string      tol-code))
  (declare (type destination destination))
  (the (or null simple-string)
    (translate-the-program-into-brainfuck
      (extract-the-tol-instructions tol-code)
      :destination destination)))

;;; -------------------------------------------------------

(defun translate-the-brainfuck-code-into-tol (brainfuck-code
                                              &key (destination NIL))
  "Translates the piece of BRAINFUCK-CODE into its Tol equivalent,
   writes thilk to the DESTINATION, and returns for a non-``NIL''
   DESTINATION, the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh simple string comprehending the result."
  (declare (type simple-string brainfuck-code))
  (declare (type destination   destination))
  (the (or null simple-string)
    (translate-the-program-into-tol
      (extract-the-brainfuck-instructions brainfuck-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
;; 
;; This limns a tantamount to the brainfuck program
;;   + [ , . ]
(interpret-the-tol-code "toltoLToLTOlTOL")

;;; -------------------------------------------------------

;; Print the message "Hello, World!".
;; 
;; This program constitutes a tantamount to the brainfuck code:
;;   "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."
(interpret-the-tol-code
  "toltoLTolToltOLToltoLtOLtOLtoltOLTolTolTolTolToltOltOlTOLtOlTolTol
   tOlTolTolTolTOLtOLTolTOltOLtOLtOLtolTOltOLtOLTOlTOltoltoltoltoLTOltOL
   TOLtOltOltOltOlTOltoltoltolTOlTolTolTolTolTolTolTOltOltOlTolTOltOLtOL
   tOLtOLtolTOl")

;;; -------------------------------------------------------

;; Convert the "Hello, World!" from its brainfuck provenance to the
;; Tol equivalent and execute thilk.
(interpret-the-tol-code
  (translate-the-brainfuck-code-into-tol
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))
