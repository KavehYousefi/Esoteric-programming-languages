;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Deskin", invented by the Esolang user "Leothetechguy" and
;; presented on April 3rd, 2021, the dation of its haecceity amplecting
;; a single instruction which operates on a sequence of signed integer
;; number, restricted in its competences to their printing, as well as
;; the deducted value's memorization, the inherent perpetuality of this
;; operation governed by a skipping mechanism that, founded upon a
;; negative modification of the current argument, may cause the
;; program's cessation.
;; 
;; 
;; Concept
;; =======
;; The Deskin programming language operates on a sequence of integer
;; numbers, printing thilk, while probing their decremented form in
;; regards to their sign, contingently skipping subsequent members in
;; the negative case; its possede also encompassing a rather
;; ineffectuous memory that correlates signed integer addresses values
;; whose woning is realized in the same realm.
;; 
;; == DESKIN: ONE INSTRUCTION'S PURVIEW ==
;; The Deskin programming language's membership is recorded among the
;; "One Instruction Set Computer" (OISC) species, the proprium of which
;; ligates the operations in their tally to an aefauld specimen, the
;; multiplier of its potential commonly, but not mandatorily a causatum
;; begotten by the operands' account and design.
;; 
;; == DESKIN: [DE]CREMENT AND [SKI]P IF [N]EGNATIVE ==
;; In Deskin's case, the variation in deportment enjoys its elicitation
;; from the current operand's reduction, which, in the negative case,
;; conditions the subsequent argument's omission; otherwise accompassing
;; the standard handling routine.
;; 
;; This principle, deliberately, reverberates in the language's
;; agnomination which parlays of Deskin's wont to
;; "[de]crement and [ski]p if [n]egative".
;; 
;; == THE PROGRAM: A SEQUENCE OF INTEGER NUMBERS ==
;; Its syntactical stipulation renders a Deskin program a sequence of
;; signed or unsignd integer numbers, with any other content's
;; adhibition of tolerance establishing a tantamount to its neglect.
;; 
;; The extracted numeric components produce the tacitly operating
;; aefauld instructions' operands or arguments.
;; 
;; == PROGRAM EXECUTION: OUTPUT, DECREMENT, REPEAT IF NON-NEGATIVE ==
;; A program's execution proceeds from the following stipulations:
;; 
;;   (1) If the current argument constitutes the first of this cycle,
;;       no output is issued.
;;   
;;   (2) If the current argument constitutes the last of this cycle,
;;       and concomitantly during this cycle no argument had been
;;       printed yet, no output is issued.
;;   
;;   (3) If neither the rule (1) nor (2) applies, the current argument
;;       is printed, either in its ipsissima verba form, or, depending
;;       on the concrete implementation and its configurations, in the
;;       guise of the character whose ASCII code corresponds to the
;;       numeric value.
;;   
;;   (4) A decremented value, "reducedArgument", is obtained via
;;         
;;         reducedArgument <- currentArgument - 1
;;   
;;   (5) This reducedArgument is stored in the memory cell under the
;;       address equal to the current argument's value:
;;       
;;         memory[currentArgument] <- reducedArgument
;;   
;;   (6) Depending on the reducedArgument, a furcation is exercised on
;;       the subsequent behavior:
;;       
;;       (6a) If the reducedArgument is negative, and the current
;;            argument constitutes the last of this cycle, the program
;;            is immediately terminated.
;;       
;;       (6b) If the reducedArgument is negative, but the current
;;            argument does not constitute the desinent one in this
;;            cycle, the next argument is skipped.
;;       
;;       (6c) If the reducedArgument is greater than or equal to zero,
;;            and the current argument constitutes the cycle's last
;;            one, a new cycle with the first argument commences with
;;            the step (1).
;;       
;;       (6d) Otherwise, if the reducedArgument is greater than or
;;            equal to zero, and a further argument follows, this
;;            successor is selected for a handling in concord with the
;;            step (1).
;; 
;; == THE EXECUTION PRINCIPLES MOLDED IN PSEUDOCODE ==
;; The following pseudocode treatise shall be valorized by its superior
;; formality in the execution principles' description:
;; 
;;   procedure interpretDeskin (arguments)
;;     Input:
;;       arguments: An ordered sequence of integer numbers.
;;     
;;     Output:
;;       None.
;;     
;;     Procedure:
;;       let memory <- prepare empty random-access memory
;;       
;;       repeat do
;;         let ip                <- 1
;;         let hasAlreadyPrinted <- false
;;         let cycleIsComplete   <- (ip > length(arguments))
;;         
;;         repeat until cycleIsComplete do
;;           let currentArgument <- arguments[ip]
;;           let isFirstArgument <- (ip = 1)
;;           let isLastArgument  <- (ip = length(arguments))
;;           let reducedArgument <- currentArgument - 1
;;           
;;           if not(isFirstArgument or
;;                  (isLastArgument and hasAlreadyPrinted))
;;           then
;;             print currentArgument
;;             hasAlreadyPrinted <- true
;;           end if
;;           
;;           memory[currentArgument] <- reducedArgument
;;           
;;           if (reducedArgument < 0) and isLastArgument then
;;             terminate program
;;           else if (reducedArgument < 0) and (not isLastArgument) then
;;             ip <- ip + 1
;;           end if
;;           
;;           ip <- ip + 1
;;         end repeat
;;       end repeat
;;   end procedure
;; 
;; == THE MEMORY: A BILATERALLY INFINITE VECTOR OF INTEGERS ==
;; The Deskin program memory's conformation ensues from the notion of
;; a one-dimensional array, the capacity of which wists of no bournes.
;; 
;; The cells whose participation furnishes the entirety's edification
;; each admit an aefauld integer number, delivered to one's personal
;; deliberation in both aspects involving sign and mickleness.
;; 
;; The random-access nature endows this compound salvatory with its
;; units' amenability to signed integer indices, whence the bourneless
;; dispansion of its gamut enjoys its gendrure.
;; 
;; The memory's purpose is governed by the reference to its inclusion
;; of the program arguments in a reduced form, associating with the
;; original argument state the consequence of a deduction by the
;; quantity of one. Maugre this dever's parcery, the storage efforts
;; ultimately limn a mateotechny's simulacrum, in that neither the
;; subscript for the content in the cell ostend a specified telos'
;; entertainment.
;; 
;; 
;; Instructions
;; ============
;; Deskin's compliance with the notion of a "One Instruction Set
;; Computer" restricts its operative membership to a singular exponent,
;; admitting zero or more signed or unsigned integer numbers.
;; 
;; Commorant in a perpetual execution iterance, the arguments are
;; printed, either in their verbatim numeric or a character form, ere
;; a decremented variant's supputation. Founded upon this value's
;; sign, the subsequent argument is either processed or skipped, the
;; sign's negative nature accompassing the second case.
;; 
;; For a more detailed treatise on the subject of its execution, please
;; consult the "Concept" subsections "PROGRAM EXECUTION: OUTPUT,
;; DECREMENT, REPEAT IF NON-NEGATIVE" and "THE EXECUTION PRINCIPLES
;; MOLDED IN PSEUDOCODE".
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The chronicle applying to Deskin's renditions, in conjunction with
;; its curtailed treatise, serve to encumber the protolog with a rather
;; high mete of susceptibility to ambiguity's inroads. The following
;; tmemata shall be ordained to a peisant subset's elucidation.
;; 
;; == HOW DOES THE MEMORY AFFECT THE PROGRAM? ==
;; Maugre its involvement in the program by the affiliation of the
;; current argument as the index with its decremented form as the cell
;; value, the memory does not even entrepart in the execution's course
;; as a parhedral component, forecause no further perquisitions nor
;; modulations ensue from this sole aspect.
;; 
;; A ramification yielded by the assessment of this salvatory deems
;; its implementation a reded thing, in the obbligato of an acquiescence
;; regarding its contemporaneous otioseness.
;; 
;; == IN WHICH FORM SHALL OUTPUT BE ISSUED? ==
;; The amplectation of Deskin's examplary programs in the most recent
;; rendition does not rise above the accrementation of a membership's
;; twissel, exhausted by a "Hello world!" printer and a truth-machine.
;; A conflicting account of its behavior in the output mode's matter
;; governs these two forbisens, as the former is purported to replicate
;; the numeric arguments in the form of the decoded ASCII characters,
;; while the truth-machine per force ought to ostend their ipsissima
;; verba numeric states.
;; 
;; As a corollary, the question shall be posed which mode, numeric or
;; character-based, exercises the veridicous lealty to the covenant.
;; 
;; It has been adjudged, the provenance a lacuna of adminicula
;; concerning corroborative warklumes for any laterality, to consign
;; the ultimate decision to an implementation-dependent aspect. Whether
;; the modality remains an incorporation into the interpreter as a
;; stalwart fact, or homologates its adjustment to express a
;; malleability as a concomitant to an executed program, shall not be
;; arrogated by further adjudications in this place.
;; 
;; A consequence of this admitted acatalepsy, both extant examples may
;; be assessed in a status of synkatathesis.
;; 
;; == HOW SHALL THE RECEPTION OF INPUT PROCEED? ==
;; An intimation of Deskin's engagement in a bidirectional
;; communication, valorized beyond the issuance of output in the
;; reception and incorporation of user-supplied numeric values,
;; partakes of the language standard's expressed facilities, its
;; reification the appendage of these supplements to the program's
;; rear.
;; 
;; However, the modus and instant involving this transaction's actual
;; actutation is wanting in the script.
;; 
;; It has been adjudged to precede a Deskin program's execution by a
;; prompt which accepts a single line of string, the conformation of
;; whose analyzation is subjected to the same nomothesia as a piece of
;; regular source code, and whose extracted integer numbers are
;; inserted at the program argument list's rear.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's geniture proceeds as an adminiculum of the
;; programming language Common Lisp, the extraction of the integral
;; numeric objects from the source code serving as an accoucheuse in
;; prevenience to the resulting integer vector's evaluation as the
;; sole instruction's arguments.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-01
;; 
;; Sources:
;;   [esolang2021Deskin:oldid:81846]
;;   The Esolang contributors, "Deskin", April 4th, 2021,
;;     revision ID 81846
;;   URL: "https://esolangs.org/w/index.php?title=Deskin&oldid=81846"
;;   Notes:
;;     - Rendition with ID 81846 of the Deskin specification.
;;     - Supplements additional information partially superior in its
;;       circumference or lucidity when juxtaposed with the latest
;;       version.
;;   
;;   [esolang2021Deskin:oldid:81794]
;;   The Esolang contributors, "Deskin", April 4th, 2021,
;;     revision ID 81794
;;   URL: "https://esolangs.org/w/index.php?title=Deskin&oldid=81794"
;;   Notes:
;;     - Rendition with ID 81794 of the Deskin specification.
;;     - Supplements additional information partially superior in its
;;       circumference or lucidity when juxtaposed with the latest
;;       version.
;;   
;;   [esolang2021Deskin]
;;   The Esolang contributors, "Deskin", April 15th, 2021
;;   URL: "https://esolangs.org/wiki/Deskin"
;;   Notes:
;;     - The most recent version of the Deskin language standard during
;;       the period of this file's composition.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype integer-vector ()
  "The ``integer-vector'' type defines a one-dimensional simple array of
   signed or unsigned integer numbers without any impositions concerning
   the collection's cardinality."
  '(simple-array integer (*)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the
   same defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (every
                #'(lambda (current-element)
                    (declare (type T current-element))
                    (typep current-element element-type))
                (the list candidate))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among which complies with the KEY-TYPE and
   allies with a value of the VALUE-TYPE, for both holds the default of
   the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (or
              (and (eq key-type   '*)
                   (eq value-type '*))
              (loop
                for current-key
                  of-type T
                  being the hash-keys in (the hash-table candidate)
                using
                  (hash-value current-value)
                always
                  (and
                    (or (eq    key-type      '*)
                        (typep current-key   key-type))
                    (or (eq    value-type    '*)
                        (typep current-value value-type))))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Deskin program memory model as a
   one-dimensional sparse array of integer-valued cells, amenable to
   subscripts desumed from the selfsame vale, and realized in a hash
   table whose keys provide the addresses, and whose values impose the
   cell states."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype output-mode ()
  "The ``output-mode'' type enumerates the valid policies for the
   display of Deskin program arguments on the standard output."
  '(member :character :numeric))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Applies an interpretation of the OBJECT as a \"generalized boolean\"
   and produces a veridicous Boolean equivalency thereof, returning for
   a non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, a diorism
   into which are subsumed the space, horizontal tab, and newline
   specimens, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-next-word (source start)
  "Proceeding from the START position into the SOURCE, detects the
   location of the nearest word and returns thilk; otherwise, upon its
   disrespondency, reprodces the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, and expected to
   reside inside of a token's bournes, detects the end of the current
   word and returns thilk; otherwise, upon its disrespondecy, reproduces
   with the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun extract-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates the next
   word and returns two values:
     (1) A fresh string representation of the detected word, which may
         be empty.
     (2) The position into the SOURCE immediately succeeding the tmema
         under the detected word's occupancy."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let* ((start-position-of-word
          (locate-next-word source start))
         (end-position-of-word
          (locate-end-of-word source start-position-of-word)))
    (declare (type fixnum start-position-of-word))
    (declare (type fixnum end-position-of-word))
    (the (values simple-string fixnum)
      (values
        (subseq source start-position-of-word end-position-of-word)
        end-position-of-word))))

;;; -------------------------------------------------------

(defun extract-next-number (source start)
  "Proceeding from the START position into the SOURCE, locates and
   extracts the next signed or unsigned integer number and returns two
   values:
     (1) If an integer could be found, its parsed value; otherwise the
         ``NIL'' sentinel.
     (2) If an integer could be found, the position into the SOURCE
         immediately succeeding its occupied parcel; otherwise the
         length of the SOURCE."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let ((next-word    NIL)
        (new-position start))
    (declare (type (or null string) next-word))
    (declare (type fixnum           new-position))
    (flet ((probe-next-word-for-number ()
            "Proceeding from the NEW-POSITION, queries the next word
             from the SOURCE, updates both the NEXT-WORD and the
             NEW-POSITION to the request's result, and either returns
             the NEXT-WORD's parsed integral value, or, upon a failure
             to interpret such, responds with ``NIL''."
            (multiple-value-setq (next-word new-position)
              (extract-next-word source new-position))
            (the (or null integer)
              (ignore-errors
                (parse-integer next-word)))))
      (the (values (or null integer) fixnum)
        (loop
          for detected-number
            of-type (or null integer)
            =       (probe-next-word-for-number)
          until
            (or detected-number
                (>= new-position (length source)))
          finally
            (return
              (values detected-number new-position)))))))

;;; -------------------------------------------------------

(defun convert-to-simple-string (source)
  "Ascertains the subsumption of the SOURCE into the simple string
   species, either by a production and return of a fresh
   ``simple-string'' instance comprehending its content, or, upon its
   compliance to the stipulation, by responding with the SOURCE itself."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of integer vector operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-integer-vector-of (elements)
  "Creates and returns a fresh ``integer-vector'' whose content is
   desumed from the list of ELEMENTS."
  (declare (type (list-of integer) elements))
  (the integer-vector
    (coerce elements
      '(simple-array integer (*)))))

;;; -------------------------------------------------------

(defun concatenate-integer-vectors (first-vector second-vector)
  "Creates and returns a fresh ``integer-vector'' whose gendrure
   constitutes an ultimity of the SECOND-VECTOR's appendage to the
   FIRST-VECTOR."
  (declare (type integer-vector first-vector))
  (declare (type integer-vector second-vector))
  (the integer-vector
    (concatenate
      '(simple-array integer (*))
      first-vector second-vector)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-numbers (source)
  "Extracts from the piece of SOURCE the entailed integer numbers and
   returns these according to their encountered order in a
   one-dimensional simple array."
  (declare (type simple-string source))
  (let ((next-number      NIL)
        (current-position 0))
    (declare (type (or null integer) next-number))
    (declare (type fixnum            current-position))
    (flet ((has-found-a-further-number-p ()
            "Queries the next number from the SOURCE, updates the
             NEXT-NUMBER and CURRENT-POSITION variables accordingly, and
             determines whether the NEXT-NUMBER is a non-``NIL'' integer
             object, returning on confirmation a ``boolean'' value of
             ``T'', otherwise ``NIL''."
            (multiple-value-setq (next-number current-position)
              (extract-next-number source current-position))
            (the boolean
              (get-boolean-value-of next-number))))
      (the integer-vector
        (make-integer-vector-of
          (loop
            while   (has-found-a-further-number-p)
            collect next-number))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input/output operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun request-input ()
  "Queries the standard input for a sequence of zero or numbers and
   returns a fresh ``integer-vector'' representation thereof."
  (format T "~&>> ")
  (finish-output)
  (the integer-vector
    (extract-numbers
      (prog1
        (convert-to-simple-string
          (read-line NIL NIL ""))
        (clear-input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :accessor      program
    :type          integer-vector
    :documentation "The program as a vector of integer numbers.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          fixnum
    :documentation "The current instruction pointer (IP) position.")
   (has-already-issued-output-p
    :initform      NIL
    :accessor      has-already-issued-output-p
    :type          boolean
    :documentation "Determines whether, during the current cycle, at
                    least one printing behest has been committed.")
   (output-mode
    :initarg       :output-mode
    :initform      :character
    :type          output-mode
    :reader        get-output-mode
    :documentation "Determines whether arguments are printed in their
                    verbatim numeric form or as the corresponding ASCII
                    characters.")
   (memory
    :initform      (make-hash-table :test #'eql)
    :type          memory
    :documentation "The program memory as a sparse integer vector."))
  (:documentation
    "The ``Interpreter'' class applies itself to the accompassing of
     actual productive value to a Deskin program's induced argument
     sequence."))

;;; -------------------------------------------------------

(defun append-input-to-program (interpreter)
  "Queries the standard input for a sequence of numbers, extracts and
   appends these to the INTERPRETER's program, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (program interpreter)
    (concatenate-integer-vectors
      (program interpreter)
      (request-input)))
  (values))

;;; -------------------------------------------------------

(defun get-current-argument (interpreter)
  "Returns the INTERPRETER's currently selected argument."
  (declare (type Interpreter interpreter))
  (the integer
    (aref (program interpreter)
      (program-ip interpreter))))

;;; -------------------------------------------------------

(defun get-number-of-arguments (interpreter)
  "Returns the tally of arguments comprising the INTERPRETER's program."
  (declare (type Interpreter interpreter))
  (the fixnum
    (length
      (program interpreter))))

;;; -------------------------------------------------------

(defun advance-instruction-pointer (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in the underlying Deskin argument sequence and returns no
   value."
  (declare (type Interpreter interpreter))
  (setf (program-ip interpreter)
    (min
      (1+ (program-ip interpreter))
      (get-number-of-arguments interpreter)))
  (values))

;;; -------------------------------------------------------

(defun first-argument-p (interpreter)
  "Determines whether the currently selected argument constitutes the
   first member in the INTERPRETER's program, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (zerop
        (program-ip interpreter)))))

;;; -------------------------------------------------------

(defun last-argument-p (interpreter)
  "Determines whether the currently selected argument constitutes the
   desinent member in the INTERPRETER's program, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (program-ip interpreter)
          (1- (get-number-of-arguments interpreter))))))

;;; -------------------------------------------------------

(defun cycle-is-complete-p (interpreter)
  "Determines whether all arguments in the INTERPRETER's program have
   been processed, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (program-ip interpreter)
          (get-number-of-arguments interpreter)))))

;;; -------------------------------------------------------

(defun prepare-for-cycle (interpreter)
  "Resets the INTERPRETER's state as a parasceve for an expected
   iterance cycle's inchoation and returns no value."
  (declare (type Interpreter interpreter))
  (psetf
    (program-ip                  interpreter) 0
    (has-already-issued-output-p interpreter) NIL)
  (values))

;;; -------------------------------------------------------

(defun shall-issue-output-p (interpreter)
  "Determines whether the contemporaneous state of the INTERPRETER's
   program homologates the currently selected argument's printing to the
   standard output, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (or (first-argument-p interpreter)
          (and (last-argument-p             interpreter)
               (has-already-issued-output-p interpreter))))))

;;; -------------------------------------------------------

(defun issue-output-if-possible (interpreter)
  "Prints the INTERPRETER's current argument in a form covenable to the
   configured output mode to the standard output and returns no value."
  (declare (type Interpreter interpreter))
  (when (shall-issue-output-p interpreter)
    (case (get-output-mode interpreter)
      (:character
        (format T "~c"
          (code-char
            (get-current-argument interpreter))))
      (:numeric
        (format T "~&~d"
          (get-current-argument interpreter)))
      (otherwise
        (error "Invalid output mode: ~s."
          (get-output-mode interpreter))))
    (setf (has-already-issued-output-p interpreter) T))
  (values))

;;; -------------------------------------------------------

(defun update-current-argument (interpreter)
  "Stores the decremented value of the INTERPRETER's current argument
   in a memory cell amenable to the original argument state's value
   and returns the decremented variant."
  (declare (type Interpreter interpreter))
  (with-slots (memory) interpreter
    (declare (type memory memory))
    (let* ((original-value    (get-current-argument interpreter))
           (decremented-value (1- original-value)))
      (declare (type integer original-value))
      (declare (type integer decremented-value))
      (setf (gethash original-value memory) decremented-value)
      (the integer decremented-value))))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Deskin program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  
  ;; Request input and append arguments to program.
  (append-input-to-program interpreter)
  
  ;; Program loop.
  (loop named perpetual-cycle do
    (prepare-for-cycle interpreter)
    ;; Loop over instructions.
    (loop until (cycle-is-complete-p interpreter) do
      (issue-output-if-possible interpreter)
      (let ((decremented-value (update-current-argument interpreter)))
        (declare (type integer decremented-value))
        (cond
          ((and (minusp decremented-value)
                (last-argument-p interpreter))
            (return-from perpetual-cycle NIL))
          ((minusp decremented-value)
            (advance-instruction-pointer interpreter))
          (T
            NIL)))
      (advance-instruction-pointer interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Deskin (code &key (output-mode :character))
  "Interprets the piece of Deskin source CODE, administering the
   OUTPUT-MODE to the cause of its printing operations, and returns no
   value."
  (declare (type string      code))
  (declare (type output-mode output-mode))
  (execute-program
    (make-instance 'Interpreter
      :program
        (extract-numbers
          (convert-to-simple-string code))
      :output-mode output-mode))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-Deskin "1 72 101 108 108 111 44 32 119 111 114 108 100 33 0")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Deskin "1" :output-mode :numeric)
