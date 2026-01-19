;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language 'A "real" esolang', invented by the Esolang user
;; "Tommyaweosme" and presented on August 13th, 2024, conceived as a
;; derivation of Urban Mueller's "brainfuck", the cleronomy appropriated
;; by thilk such of a nearly enker compass; merely abluding from its
;; designment's provenance in the bailiwick of its syntaxis, this
;; constituting a cambistry peracted on the original one-symbol
;; instruction identifiers for realia molded into an ogdoad of
;; vegetable agnominations.
;; 
;; 
;; Concept
;; =======
;; The 'A "real" esolang' programming language constitutes an
;; equipollent to its brainfuck entheus, the lealty's incarnation
;; neither of the wite's tholance to deviate from the basic tenets in
;; the program execution, neither from the data castaldy's mode and
;; epiphenomena; however, the diorism's contribution establishes a
;; variation on the octuple instruction names, the default one-symbol
;; identifiers experiencing a supersession by terms designating "real"
;; objects, these realia being desumed from the realm of vegetables.
;; 
;; == A REAL ESOLANG: BRAINFUCK'S INSTRUCTION IDENTIFIERS REASSIGNED ==
;; A program's assemblage ensues from a catena of specific vegetable
;; names, each such twissel's segregation one or more whitespaces'
;; dever.
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; The perimeter of 'A "real" esolang''s recipiency does not elude
;; brainfuck's architecture, appropriating in an ipsissima verba fashion
;; a bilaterally bourneless dispansion of unsigned byte-valued cells.
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
;; identifiers' semantical aspects to its brainfuck heritage,
;; 'A \"real\" esolang''s compass does neither actuate an ostention's
;; commission designed with supererogation with respect to its
;; provenance, nor a curtailment in the competences; in corollary, this
;; cleronomy accounts for an octuple contingency, amplecting in its
;; compass the cell pointer movement, basic arithmetics, input and
;; output facilities, as well as an aefauld construct for the control
;; flow duction.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be fulfilled in a cursory mete of
;; gnarity's adhibition anent the 'A "real" esolang' programming
;; language's facilities, thilk concomitantly concur with the offerings
;; of its brainfuck stock-father.
;; 
;;   ------------------------------------------------------------------
;;   Command     | Effect
;;   ------------+-----------------------------------------------------
;;   pea         | Increments the current cell value by one (1). If the
;;               | new state transgresses the upper march of 255, the
;;               | value wraps around to the lower extremum of
;;               | zero (0).
;;   ..................................................................
;;   corn        | Decrements the current cell value by one (1). If the
;;               | new state transgresses the lower march of zero (0),
;;               | the value wraps around to the upper extremum of 255.
;;   ..................................................................
;;   greenbean   | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   broccoli    | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   cauliflower | If the current cell value equals zero (0), moves the
;;               | instruction pointer (IP) forward to the position
;;               | immediately succeeding the matching "zucchini"
;;               | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   zucchini    | If the current cell value does not equal zero (0),
;;               | moves the instruction pointer (IP) back to the
;;               | position immediately succeeding the matching
;;               | "cauliflower" instruction; otherwise proceeds as
;;               | usual.
;;   ..................................................................
;;   lettuce     | Prints the character whose ASCII code corresponds to
;;               | the current cell value to the standard output
;;               | conduit.
;;   ..................................................................
;;   spinach     | Queries the standard input conduit for a character
;;               | and stores its ASCII code in the current cell.
;;   ------------------------------------------------------------------
;; 
;; == 'A "REAL" ESOLANG' AND BRAINFUCK ==
;; Establishing an apodosis from its status as a mere brainfuck
;; reformulation, 'A "real" esolang''s patration in language twissel's
;; replication homologates an equiparation applying to the operative
;; bailiwick:
;; 
;;   ------------------------------------------------------------
;;   A "real" esolang | brainfuck | Causatum
;;   -----------------+-----------+------------------------------
;;   pea              | +         | Increment the current cell.
;;   ............................................................
;;   corn             | -         | Decrement the current cell.
;;   ............................................................
;;   greenbean        | >         | Move the cell pointer right.
;;   ............................................................
;;   broccoli         | <         | Move the cell pointer left.
;;   ............................................................
;;   cauliflower      | [         | Jump forward if zero.
;;   ............................................................
;;   zucchini         | ]         | Jump back if not zero.
;;   ............................................................
;;   lettuce          | .         | Print the current cell.
;;   ............................................................
;;   spinach          | ,         | Input into the current cell.
;;   ------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's mode of operation, being implemented in the
;; programming language Common Lisp, avaunts from the 'A "real" esolang'
;; source code string's transcription into dedicated instruction
;; representations inwith whose potential an enhaused capacity for eath
;; evaluation wones, ere these entities ultimate vouchsafement of actual
;; efficacy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-01-11
;; 
;; Sources:
;;   [esolang2024:A "real" esolang]
;;   The Esolang contributors, 'A "real" esolang', August 15th, 2024
;;   URL: "https://esolangs.org/wiki/A_%22real%22_esolang"
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
  "The ``instruction'' type enumerates the recognized variation on
   'A \"real\" esolang' instructions."
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
  "The ``program'' type defines an executable 'A \"real\" esolang'
   program as a one-dimensional simple array of ``instruction''
   objects."
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
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun character-is-a-whitespace-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, a diorism
   inwith whose bournes are ensconced the horizontal tab, the newline,
   or line feed, the vertical tabulation, the form feed, the carriage
   return, as well as the traditional space, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string splitter.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Splitter ()
  ((source
    :initarg       :source
    :initform      (error "Missing the source for the splitter.")
    :type          simple-string
    :documentation "The string to segregate into its tokens.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE string."))
  (:documentation
    "The ``Splitter'' class furnishes an entity whose capacitation
     appertains to the extraction of whitespace-separated tokens from
     an underlying source string."))

;;; -------------------------------------------------------

(defun skip-accolent-whitespaces (splitter)
  "Proceeding from the current position into the SPLITTER's source,
   skips a catena enumerating zero or more whitespace characters, and
   returns no value."
  (declare (type Splitter splitter))
  (with-slots (source position) splitter
    (declare (type simple-string source))
    (declare (type fixnum        position))
    (setf position
      (or (position-if-not #'character-is-a-whitespace-p source
            :start position)
          (length source))))
  (values))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((splitter Splitter) &key)
  "Skips any leading whitespaces at the inchoacy of the SPLITTER's
   source string and returns no value."
  (declare (type Splitter splitter))
  (skip-accolent-whitespaces splitter)
  (values))

;;; -------------------------------------------------------

(defun prepare-a-splitter-for (source)
  "Creates and returns a fresh ``Splitter'' dedicated to the SOURCE
   string's segregation into its tokens."
  (declare (type string source))
  (the Splitter
    (make-instance 'Splitter :source
      (convert-into-a-simple-string source))))

;;; -------------------------------------------------------

(defun more-tokens-follow-p (splitter)
  "Determines whether one or more tokens remain to be requested from the
   SPLITTER, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Splitter splitter))
  (the boolean
    (with-slots (source position) splitter
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (convert-into-a-boolean-value
        (< position
           (length source))))))

;;; -------------------------------------------------------

(defun request-the-next-token (splitter)
  "Returns the next token from the SPLITTER.
   ---
   Upon its source's exhaustion, the SPLITTER responds to any request
   with a fresh empty string."
  (declare (type Splitter splitter))
  (the simple-string
    (with-slots (source position) splitter
      (declare (type simple-string source))
      (declare (type fixnum        position))
      (let ((end-position
              (or (position-if #'character-is-a-whitespace-p source
                    :start position)
                  (length source))))
        (declare (type fixnum end-position))
        (prog1
          (subseq source position end-position)
          (setf position end-position)
          (skip-accolent-whitespaces splitter))))))



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
;; -- Implementation of the 'A "real" esolang' code parser.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-the-next-instruction (splitter)
  "Requests the next token from the SPLITTER and attents to parse thilk
   as an 'A \"real\" esolang instruction, returning on confirmation a
   covenable instruction representation; otherwise responds with
   ``NIL''."
  (declare (type Splitter splitter))
  (let ((next-token (request-the-next-token splitter)))
    (declare (type simple-string next-token))
    (the (or null instruction)
      (flet ((probe-the-token (expected-content result-on-match)
              "Determines whether the NEXT-TOKEN equals the
               EXPECTED-CONTENT, returning on confirmation the
               RESULT-ON-MATCH, otherwise ``NIL''."
              (declare (type simple-string expected-content))
              (declare (type instruction   result-on-match))
              (the (or null instruction)
                (and
                  (string= next-token expected-content)
                  result-on-match))))
        (or (probe-the-token "pea"         :increment)
            (probe-the-token "corn"        :decrement)
            (probe-the-token "greenbean"   :move-right)
            (probe-the-token "broccoli"    :move-left)
            (probe-the-token "cauliflower" :jump-forward)
            (probe-the-token "zucchini"    :jump-back)
            (probe-the-token "lettuce"     :output)
            (probe-the-token "spinach"     :input))))))

;;; -------------------------------------------------------

(defun extract-the-a-real-esolang-instructions (code)
  "Parses the piece of 'A \"real\" esolang' source CODE and returns a
   one-dimensional simple array comprehending its extracted
   instructions."
  (declare (type simple-string code))
  (the program
    (let ((splitter (prepare-a-splitter-for code)))
      (declare (type Splitter splitter))
      (assemble-a-program-from
        (loop
          while
            (more-tokens-follow-p splitter)
          for next-instruction
            of-type (or null instruction)
            =       (extract-the-next-instruction splitter)
          when next-instruction
            collect next-instruction)))))



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
     bidirectional vincula betwixt the jump points in an
     'A \"real\" esolang' program by adminiculum of their zero-based
     positions into the program's instruction vector."))

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
   contexture betwixt the matching jump points in the
   'A \"real\" esolang' PROGRAM, mediated by adminiculum of their
   zero-based positions inwith its instruction vector."
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

(defun execute-the-a-real-esolang-program (program)
  "Executes the 'A \"real\" esolang' PROGRAM and returns no value."
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

(defun interpret-the-a-real-esolang-code (code)
  "Interprets the piece of 'A \"real\" esolang' source CODE and returns
   no value."
  (declare (type string code))
  (execute-the-a-real-esolang-program
    (extract-the-a-real-esolang-instructions
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
              (:move-right   #\>)
              (:move-left    #\<)
              (:jump-forward #\[)
              (:jump-back    #\])
              (:output       #\.)
              (:input        #\,)
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
;; -- Implementation of the 'A "real" esolang' code generator.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-program-into-a-real-esolang
    (program
     &key (destination NIL))
  "Generates and returns for the PROGRAM the equivalent
   'A \"real\" esolang' code, writes thilk to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh simple string
   comprehending the output."
  (declare (type program     program))
  (declare (type destination destination))
  (the (or null simple-string)
    (if destination
      (loop
        for current-instruction of-type instruction across program
        and current-position    of-type fixnum      from   0 by 1
        do
          (format destination "~&~a"
            (case current-instruction
              (:increment    "pea")
              (:decrement    "corn")
              (:move-right   "greenbean")
              (:move-left    "broccoli")
              (:jump-forward "cauliflower")
              (:jump-back    "zucchini")
              (:output       "lettuce")
              (:input        "spinach")
              (otherwise
                (error "The instruction ~a at the position ~d cannot ~
                        be translated into a 'A \"real\" esolang' ~
                        instruction."
                  current-instruction current-position)))))
      (convert-into-a-simple-string
        (with-output-to-string (a-real-esolang-code)
          (declare (type string-stream a-real-esolang-code))
          (translate-the-program-into-a-real-esolang program
            :destination a-real-esolang-code))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the translation operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-the-a-real-esolang-code-into-brainfuck
    (a-real-esolang-code
     &key (destination NIL))
  "Translates the piece of A-REAL-ESOLANG-CODE into its brainfuck
   equivalent, writes thilk to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION, the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh simple string comprehending the
   result."
  (declare (type string      a-real-esolang-code))
  (declare (type destination destination))
  (the (or null simple-string)
    (translate-the-program-into-brainfuck
      (extract-the-a-real-esolang-instructions a-real-esolang-code)
      :destination destination)))

;;; -------------------------------------------------------

(defun translate-the-brainfuck-code-into-a-real-esolang
    (brainfuck-code
     &key (destination NIL))
  "Translates the piece of BRAINFUCK-CODE into its 'A \"real\" esolang'
   equivalent, writes thilk to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION, the ``NIL'' value; otherwise, for a ``NIL''
   DESTINATION, responds with a fresh simple string comprehending the
   result."
  (declare (type simple-string brainfuck-code))
  (declare (type destination   destination))
  (the (or null simple-string)
    (translate-the-program-into-a-real-esolang
      (extract-the-brainfuck-instructions brainfuck-code)
      :destination destination)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ASCII loop: Print the characters with the ASCII code from inclusive
;; zero (0) to inclusive 255.
(interpret-the-a-real-esolang-code
  "pea cauliflower lettuce pea zucchini")

;;; -------------------------------------------------------

;; Print the message "Hello, World!".
;; 
;; This program constitutes a tantamount to the brainfuck code:
;;   "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."
(interpret-the-a-real-esolang-code
  "
  pea
  cauliflower
  corn
  corn
  greenbean
  corn
  cauliflower
  greenbean
  greenbean
  pea
  greenbean
  corn
  corn
  corn
  corn
  corn
  broccoli
  broccoli
  zucchini
  broccoli
  corn
  corn
  broccoli
  corn
  corn
  corn
  zucchini
  greenbean
  corn
  lettuce
  greenbean
  greenbean
  greenbean
  pea
  lettuce
  greenbean
  greenbean
  lettuce
  lettuce
  pea
  pea
  pea
  cauliflower
  lettuce
  greenbean
  zucchini
  broccoli
  broccoli
  broccoli
  broccoli
  lettuce
  pea
  pea
  pea
  lettuce
  corn
  corn
  corn
  corn
  corn
  corn
  lettuce
  broccoli
  broccoli
  corn
  lettuce
  greenbean
  greenbean
  greenbean
  greenbean
  pea
  lettuce
  ")

;;; -------------------------------------------------------

;; Convert the "Hello, World!" from its brainfuck provenance to the
;; 'A "real" esolang' equivalent and execute thilk.
(interpret-the-a-real-esolang-code
  (translate-the-brainfuck-code-into-a-real-esolang
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))
