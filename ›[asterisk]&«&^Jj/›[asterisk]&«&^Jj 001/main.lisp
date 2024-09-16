;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "›*&«&^Jj", invented by the Esolang user "Gggfr" and
;; presented on June 27th, 2024, itself begotten as an extension of
;; "›*&«&^" by the user "Ractangle", employing a variety of symbols in
;; order to manipulate an infinite arrangement of signed integer cells.
;; 
;; 
;; Concept
;; =======
;; The ›*&«&^Jj programming language dedicates itself to the
;; manipulation of a dextrally infinite dispansion of signed
;; integer-valued cells, its competences amplecting basic arithmetics,
;; the tape's navigation, and a jump-based iterance construct.
;; 
;; == THE MEMORY: A DEXTRALLY INFINITE ACCOUNT OF INTEGER REGISTERS ==
;; The program memory's conception proceeds from a linear arrangement of
;; integer-valued registers, or cells, bourneless in their dextral
;; expansion, with each unit's capacity meted in a signed integer number
;; of any mickleness.
;; 
;; In its incipial status amplecting an aefauld register, further
;; components are capacitated in their appendage to the tape's rear,
;; any of these, siclike to the original accumulator, begotten in the
;; default state of zero (0).
;; 
;; A memory pointer, at the program's inchoation empight on the first
;; unit, selects at any instant the currently active register. Its
;; amenability to poco a poco perambulations along both airts
;; homologates the selection's modulation. An attempt to transcend
;; beyond the sinistral or dextral march, as imposed by the current
;; memory conformation, accompasses no further causatum.
;; 
;; 
;; Instructions
;; ============
;; The ›*&«&^Jj enumerates a decimal instruction set, a septuple thereof
;; the language's veridical neoterisms, while a treble registers its
;; acquisition from the ›*&«&^ stock-father's cleronomy.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall constitute the administration of
;; acquaintance with the operative features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >*      | Increments the current register value by one and
;;           | multiplies the modified state by two.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following applies:
;;           |   tape[regPointer] <- (tape[regPointer] + 1) * 2
;;           |---------------------------------------------------------
;;           | This operation constitutes an adjusted appropriation
;;           | from the ›*&«&^ programming language.
;;   ..................................................................
;;   <<      | Decrements the current register value by two.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adjusted appropriation
;;           | from the ›*&«&^ programming language.
;;   ..................................................................
;;   ^       | Prints the character whose ASCII code corresponds to the
;;           | current register value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adjusted appropriation
;;           | from the ›*&«&^ programming language.
;;   ..................................................................
;;   J       | Queries the standard input for a character and stores
;;           | its ASCII code in the current register.
;;           |---------------------------------------------------------
;;           | This operation constitutes an autochthonous element of
;;           | the ›*&«&^Jj programming language.
;;   ..................................................................
;;   j       | Appends a new, initially zero-valued, register to the
;;           | program tape's rear.
;;           |---------------------------------------------------------
;;           | This operation constitutes an autochthonous element of
;;           | the ›*&«&^Jj programming language.
;;   ..................................................................
;;   /       | Translates the register pointer to the previous
;;           | register.
;;           |---------------------------------------------------------
;;           | If the register pointer is commorant in the leftmost
;;           | register, no causatum is exercised.
;;           |---------------------------------------------------------
;;           | This operation constitutes an autochthonous element of
;;           | the ›*&«&^Jj programming language.
;;   ..................................................................
;;   \       | Translates the register pointer to the next register.
;;           |---------------------------------------------------------
;;           | If the register is commorant in the rightmost register,
;;           | no causatum is exercised.
;;           |---------------------------------------------------------
;;           | This operation constitutes an autochthonous element of
;;           | the ›*&«&^Jj programming language.
;;   ..................................................................
;;   &&      | Sets the current register value to the difference of its
;;           | right neighbor register's value decremented by its left
;;           | neighbor.
;;           |---------------------------------------------------------
;;           | If any of the accolent cells does not exist, its value
;;           | is substituted by the constant zero (0).
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, the following applies:
;;           |   leftValue  <- 0
;;           |   rightValue <- 0
;;           |   
;;           |   if register tape[regPointer - 1] exists then
;;           |     leftValue <- tape[regPointer - 1]
;;           |   else
;;           |     leftValue <- 0
;;           |   end if
;;           |   
;;           |   if register tape[regPointer + 1] exists then
;;           |     rightValue <- tape[regPointer + 1]
;;           |   else
;;           |     rightValue <- 0
;;           |   end if
;;           |   
;;           |   tape[regPointer] <- rightValue - leftValue
;;           |---------------------------------------------------------
;;           | This operation constitutes an autochthonous element of
;;           | the ›*&«&^Jj programming language.
;;   ..................................................................
;;   [       | If the current register contains zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "]" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation
;;           | from the brainfuck programming language.
;;   ..................................................................
;;   ]       | If the current register does not contain zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "[" token; otherwise
;;           | proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation constitutes a verbatim appropriation
;;           | from the brainfuck programming language.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The reification of this interpreter's implementation proceeds in the
;; programming language Common Lisp.
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
;; Date:   2024-09-04
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024›*&«&^]
;;   The Esolang contributors, "›*&«&^", August 14th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%80%BA*%26%C2%AB%26%5E"
;;   Notes:
;;     - Specification of the "›*&«&^" programming language, this being
;;       the predecessor of "›*&«&^Jj".
;;   
;;   [esolang2024›*&«&^Jj]
;;   The Esolang contributors, "›*&«&^Jj", July 26th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%80%BA*%26%C2%AB%26%5EJj"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination's provenance is realized in
   the TYPE-NAME, its formal parameters appropriated in an ipsissima
   verba fashion from the LAMBDA-LIST, and whose predicate apportions
   to the subject of this docimasy the CANDIDATE-NAME, evaluating the
   BODY forms with access granted to the same, expected to return in the
   desinent form's first result a generalized boolean value of \"true\"
   upon the candidate's covenableness, otherwise \"false\".
   ---
   The first BODY form, if resolving to a string object, is adminstered
   a construe as a documentation string to the derived type, and is
   subsequently reappropriated for this telos."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   T)
                                                 (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, therein associating with a key of
   the KEY-TYPE a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(define-predicated-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, its
   default established by the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation on
   instructions."
  '(member
     :increment
     :decrement
     :output
     :input
     :create-register
     :move-left
     :move-right
     :subtract
     :jump-forward
     :jump-back))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines an association betwixt the
   recognized identifiers tokens and their operative tantamounts, their
   reification elicited by a hash table whose string keys respond to
   ``instruction'' values."
  '(hash-table-of simple-string instruction))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional nexus betwixt jump
   points in a ›*&«&^Jj, the respective bournes being represented by
   mediation of their positions, both furnished as keys and values of
   a hash table."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a ›*&«&^Jj program as a one-dimesional
   simple array comprehending zero or more ``instruction'' members."
  '(simple-array instruction (*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a generalized boolean specimen
   and produces a veridical Boolean tantamount therefrom, returning for
   a non-``NIL'' input a ``boolean'' value of ``T'', otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-starts-with-p (source prefix start)
  "Determines whether the PREFIX can be detected proceeding from the
   inclusive START position into the SOURCE, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type string prefix))
  (declare (type fixnum start))
  (the boolean
    (not (null
      (string= source prefix
        :start1 start
        :end1   (min (+ start (length prefix))
                     (length source)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of vector operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-integer-vector (initial-size)
  "Creates and returns a dynamic vector of integer elements, measuring
   at its inchoation the INITIAL-SIZE."
  (declare (type (integer 0 *) initial-size))
  (the (vector integer *)
    (make-array initial-size
      :element-type    'integer
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized ›*&«&^Jj language identifiers with
   representative instructions.")

;;; -------------------------------------------------------

(flet ((register-identifier (identifier instruction)
        "Associates the IDENTIFIER with the INSTRUCTION in the global
         +IDENTIFIERS+ table and returns no value."
        (declare (type simple-string identifier))
        (declare (type instruction   instruction))
        (setf (gethash identifier +IDENTIFIERS+) instruction)
        (values)))
  ;; Instructions from "›*&«&^".
  (register-identifier ">*" :increment)
  (register-identifier "<<" :decrement)
  (register-identifier "^"  :output)
  ;; Instructions from "›*&«&^Jj".
  (register-identifier "J"  :input)
  (register-identifier "j"  :create-register)
  (register-identifier "/"  :move-left)
  (register-identifier "\\" :move-right)
  (register-identifier "&&" :subtract)
  (register-identifier "["  :jump-forward)
  (register-identifier "]"  :jump-back)
  (values))

;;; -------------------------------------------------------

(defun probe-instruction-at (source start)
  "Determines whether a ›*&«&^Jj identifier can be detected proceeding
   from the inclusive START position into the SOURCE, returning two
   values:
     (1) If an instruction identifier could be detected, the associated
         ``instruction'' object, otherwise ``NIL''.
     (2) If an instruction identifier could be detected, the position
         in the SOURCE immediately succeeding its occupied parcel,
         otherwise the position immediately succeeding the START."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values (or null instruction) fixnum)
    (loop
      for identifier
        of-type simple-string
        being   the hash-keys in +IDENTIFIERS+
      using (hash-value instruction)
      when (string-starts-with-p source identifier start) do
        (return
          (values instruction
            (+ start (length identifier))))
      finally
        (return
          (values NIL
            (1+ start))))))

;;; -------------------------------------------------------

(defun search-instruction (source start)
  "Proceeding from the START position into the SOURCE, seeks the next
   ›*&«&^Jj instruction and returns two values:
     (1) If an instruction identifier could be detected, the associated
         ``instruction'' object, otherwise ``NIL''.
     (2) If an instruction identifier could be detected, the position
         in the SOURCE immediately succeeding its occupied parcel,
         otherwise the length of the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((next-instruction NIL)
        (new-position     start))
    (declare (type (or null instruction) next-instruction))
    (declare (type fixnum                new-position))
    (loop
      until (or next-instruction (>= new-position (length source))) do
        (multiple-value-setq (next-instruction new-position)
          (probe-instruction-at source new-position)))
    (the (values (or null instruction) fixnum)
      (values next-instruction new-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Scanner
  (:constructor make-scanner (source)))
  "The ``Scanner'' class is apportioned the onus of extracting from a
   piece of ›*&«&^Jj source code the entailed instructions."
  (source   (error "Missing source.") :type string :read-only T)
  (position 0                         :type fixnum :read-only NIL))

;;; -------------------------------------------------------

(defun get-next-instruction (scanner)
  "Returns the next ›*&«&^Jj instruction from the SCANNER, or responds
   with ``NIL'' if none such exist in its source."
  (declare (type Scanner scanner))
  (multiple-value-bind (next-instruction new-position)
      (search-instruction
        (scanner-source   scanner)
        (scanner-position scanner))
    (declare (type (or null instruction) next-instruction))
    (declare (type fixnum                new-position))
    (setf (scanner-position scanner) new-position)
    (the (or null instruction) next-instruction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a ›*&«&^Jj ``program'' from the list of
   INSTRUCTIONS."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Parser ()
  ((scanner
    :initarg       :scanner
    :initform      (error "The parser is missing a scanner.")
    :reader        get-scanner
    :type          Scanner
    :documentation "The entity responsible for the provision of
                    instructions."))
  (:documentation
    "The ``Parser'' class furnishes an entity entalented with such
     competence as to realize the assemblage of a ›*&«&^Jj program from
     a sequence of instructions."))

;;; -------------------------------------------------------

(defun make-parser (scanner)
  "Creates and returns a fresh ``Parser'' devoted to the assemblage of
   a ›*&«&^Jj program from a scanner's instructions."
  (declare (type Scanner scanner))
  (the Parser
    (make-instance 'Parser :scanner scanner)))

;;; -------------------------------------------------------

(defun assemble-program (parser)
  "Ordains the PARSER to a ›*&«&^Jj program's assemblage and returns the
   resulting instruction sequence as a one-dimensional simple array."
  (declare (type Parser parser))
  (the program
    (make-program
      (loop
        for next-instruction
          of-type (or null instruction)
          =       (get-next-instruction
                    (get-scanner parser))
        while next-instruction
          collect next-instruction))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-jump-table ()
  "Creates and returns an initially vacant ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Associates the START-POINT and END-POINT in the JUMP-TABLE and
   returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-jump-table (program)
  "Supputates and returns for the ›*&«&^Jj PROGRAM a jump table which
   affiliates its forward and back jump positions by adminiculum of
   their respective zero-based locations in the PROGRAM."
  (declare (type program program))
  (let ((jump-table        (make-empty-jump-table))
        (jump-start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-start-points))
    (loop
      for instruction of-type instruction across program
      and position    of-type fixnum      from   0 by 1
      if (eq instruction :jump-forward) do
        (push position jump-start-points)
      else if (eq instruction :jump-back) do
        (if jump-start-points
          (connect-jump-points jump-table
            (pop jump-start-points)
            position)
          (error "Unmatched jump end point at position ~d." position))
      end
      finally
        (when jump-start-points
          (error "Unmatched jump start point~d at ~
                  position~:p ~{~a~^, ~}."
            (length jump-start-points)
            jump-start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table source-point)
  "Returns for the SOURCE-POINT in the JUMP-TABLE the jump destination
   position, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (or (gethash source-point jump-table)
        (error "No destination associated with the jump point ~d."
          source-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program tape.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((registers
    :initform      (make-dynamic-integer-vector 1)
    :accessor      tape-registers
    :type          (vector integer *)
    :documentation "A extendable vector of integer-valued registers.")
   (pointer
    :initform      0
    :accessor      tape-pointer
    :type          (integer 0 *)
    :documentation "The cell or register pointer, serving to maintain
                    the currently active unit's index into the
                    REGISTERS."))
  (:documentation
    "The ``Tape'' class furnishes an implementation of the ›*&«&^Jj
     program memory as a dextrally infinite dispansion of integer-valued
     cells or registers, amenable to a mobile pointer which at any
     instant selects the currently active unit among these."))

;;; -------------------------------------------------------

(defun make-empty-tape ()
  "Creates and returns an initially empty ``Tape'' composed of a single
   register."
  (the Tape
    (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun get-register-vector-fill-pointer (tape)
  "Returns the position of the TAPE register vector's fill pointer."
  (declare (type Tape tape))
  (the fixnum
    (fill-pointer
      (tape-registers tape))))

;;; -------------------------------------------------------

(defun add-register (tape)
  "Inserts a new register in its inchoate state at the TAPE's rear and
   returns no value."
  (declare (type Tape tape))
  (vector-push-extend 0
    (tape-registers tape))
  (values))

;;; -------------------------------------------------------

(defun valid-register-index-p (tape probed-index)
  "Determines whether the PROBED-INDEX wones in the TAPE register's
   defined bournes, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Tape   tape))
  (declare (type fixnum probed-index))
  (the boolean
    (get-boolean-value-of
      (array-in-bounds-p
        (tape-registers tape)
        probed-index))))

;;; -------------------------------------------------------

(defun get-register-value-at (tape index)
  "Returns the integer value stored in the TAPE's register amenable to
   the INDEX, or responds with the default value of zero (0) if extant
   beyond the proper bournes."
  (declare (type Tape   tape))
  (declare (type fixnum index))
  (the integer
    (or (and (valid-register-index-p tape index)
             (aref (tape-registers tape) index))
        0)))

;;; -------------------------------------------------------

(defun get-left-neighbor-register-value (tape)
  "Returns the integer value stored in the TAPE's register immediately
   to the tape pointer's left, or responds with the default value of
   zero (0) if extant beyond the proper bournes."
  (declare (type Tape tape))
  (the integer
    (get-register-value-at tape
      (1- (tape-pointer tape)))))

;;; -------------------------------------------------------

(defun get-right-neighbor-register-value (tape)
  "Returns the integer value stored in the TAPE's register immediately
   to the tape pointer's right, or responds with the default value of
   zero (0) if extant beyond the proper bournes."
  (declare (type Tape tape))
  (the integer
    (get-register-value-at tape
      (1+ (tape-pointer tape)))))

;;; -------------------------------------------------------

(defun current-register-value (tape)
  "Returns the integer value stored in the TAPE's current register."
  (declare (type Tape tape))
  (the integer
    (get-register-value-at tape
      (tape-pointer tape))))

;;; -------------------------------------------------------

(defun (setf current-register-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current register and returns no
   value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf (aref (tape-registers tape)
          (tape-pointer tape))
    new-value)
  (values))

;;; -------------------------------------------------------

(defun current-register-contains-zero-p (tape)
  "Determines whether the TAPE's current register contains the value
   zero (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-register-value tape)))))

;;; -------------------------------------------------------

(defun first-register-selected-p (tape)
  "Determines whether the TAPE pointer currently resides in the first
   register, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (zerop
        (tape-pointer tape)))))

;;; -------------------------------------------------------

(defun last-register-selected-p (tape)
  "Determines whether the TAPE pointer currently resides in the last
   register, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Tape tape))
  (the boolean
    (get-boolean-value-of
      (>= (tape-pointer      tape)
          (1- (get-register-vector-fill-pointer tape))))))

;;; -------------------------------------------------------

(defun move-tape-pointer-left (tape)
  "Translates the TAPE's register pointer one cell to the left, if
   possible, and returns no value."
  (declare (type Tape tape))
  (unless (first-register-selected-p tape)
    (decf (tape-pointer tape)))
  (values))

;;; -------------------------------------------------------

(defun move-tape-pointer-right (tape)
  "Translates the TAPE's register pointer one cell to the right, if
   possible, and returns no value."
  (declare (type Tape tape))
  (unless (last-register-selected-p tape)
    (incf (tape-pointer tape)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Interpreter is aissing a ›*&«&^Jj program.")
    :reader        get-program
    :type          program
    :documentation "The ›*&«&^Jj program to evaluated.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          fixnum
    :documentation "The current instruction pointer (IP) position in the
                    program as a zero-based subscript.")
   (jump-points
    :initform      (make-empty-jump-table)
    :accessor      get-jump-points
    :type          jump-table
    :documentation "A bilateral association betwixt the jump points in
                    the ›*&«&^Jj PROGRAM, mediated by their zero-based
                    indices into the same.")
   (tape
    :initform      (make-empty-tape)
    :reader        get-tape
    :type          Tape
    :documentation "The ›*&«&^Jj program memory, its diorism proceeding
                    from an infinite tape of integer-valued cells."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     veridical operative expression to a ›*&«&^Jj furnished in the
     static guise of its instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Populates the INTERPRETER's jump table and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'jump-points)
    (supputate-jump-table
      (get-program interpreter)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'', the onus of which
   constitutes the ›*&«&^Jj PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the current instruction from the INTERPRETER's maintained
   program."
  (declare (type Interpreter interpreter))
  (the instruction
    (aref
      (get-program interpreter)
      (program-ip  interpreter))))

;;; -------------------------------------------------------

(defun jump-to-opposite-endpoint (interpreter)
  "Expecting the INTERPRETER's instruction pointer (IP) to reside on a
   jump endpoint, relocates this cursor to the opposite bourne and
   returns no value."
  (declare (type Interpreter interpreter))
  (setf (program-ip interpreter)
    (get-jump-destination
      (get-jump-points interpreter)
      (program-ip      interpreter)))
  (values))

;;; -------------------------------------------------------

(defun program-is-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's program has been processed in
   its entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (not
        (array-in-bounds-p
          (get-program interpreter)
          (program-ip  interpreter))))))

;;; -------------------------------------------------------

(defun advance-in-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction in its program and returns no value."
  (declare (type Interpreter interpreter))
  (unless (program-is-exhausted-p interpreter)
    (incf (program-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun process-current-instruction (interpreter)
  "Processes the INTERPRETER's current instruction and returns no
   value."
  (declare (type Interpreter interpreter))
  (case (get-current-instruction interpreter)
    (:increment
      (setf (current-register-value
              (get-tape interpreter))
        (* (1+ (current-register-value
                 (get-tape interpreter)))
           2)))
    
    (:decrement
      (decf
        (current-register-value
          (get-tape interpreter))
        2))
    
    (:output
      (format T "~c"
        (code-char
          (current-register-value
            (get-tape interpreter))))
      (finish-output))
    
    (:input
      (format T "~&>> ")
      (finish-output)
      (setf (current-register-value
              (get-tape interpreter))
        (char-code
          (read-char)))
      (clear-input))
    
    (:create-register
      (add-register
        (get-tape interpreter)))
    
    (:move-left
      (move-tape-pointer-left
        (get-tape interpreter)))
    
    (:move-right
      (move-tape-pointer-right
        (get-tape interpreter)))
    
    (:subtract
      (setf (current-register-value
              (get-tape interpreter))
        (- (get-right-neighbor-register-value
             (get-tape interpreter))
           (get-left-neighbor-register-value
             (get-tape interpreter)))))
    
    (:jump-forward
      (when (current-register-contains-zero-p
              (get-tape interpreter))
        (jump-to-opposite-endpoint interpreter)))
    
    (:jump-back
      (unless (current-register-contains-zero-p
                (get-tape interpreter))
        (jump-to-opposite-endpoint interpreter)))
    
    (otherwise
      (error "Unrecognized instruction: ~s."
        (get-current-instruction interpreter))))
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the ›*&«&^Jj program consigned to the INTERPRETER's custody
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-is-exhausted-p interpreter) do
    (process-current-instruction interpreter)
    (advance-in-program          interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-›*&«&^Jj (code)
  "Interprets the piece of ›*&«&^Jj source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (assemble-program
        (make-parser
          (make-scanner code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "H".
(interpret-›*&«&^Jj ">*>*>*>*>*>*<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<^")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-›*&«&^Jj "J[^J]")

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Memory layout:
;;   registers[0]: Copy of user input register registers[1] for
;;                 contingent perpetual printing of the number "1".
;;   registers[1]: User input register; either 48 (= "0") or 49 (= "1");
;;                 subsequently reduced to either 0 or 1 by adminiculum
;;                 of registers[2] in order to probe the activation of
;;                 the perpetual loop printing registers[0].
;;   registers[2]: Reduction register; initially containing 24, it is
;;                 reduced to zero (0) while decrementing the user input
;;                 register registers[1] by steps of two (2).
;; 
;; Procedure:
;;   add register
;;   move register pointer right
;;   registers[1] <- user input (= 48 or 49)
;;   print registers[1]
;;   
;;   move register pointer left
;;   { Apply "&&" instruction once. }
;;   registers[0] <- registers[1]
;;   
;;   add register
;;   move register pointer right
;;   move register pointer right
;;   { Apply five times the instruction ">*". }
;;   registers[2] <- 62
;;   { Apply seven times the instruction "<<". }
;;   registers[2] <- registers[2] - 14
;;   
;;   while registers[2] != 0 do
;;     move register pointer left
;;     { Apply the instruction "<<" once per register. }
;;     registers[2] <- registers[2] - 2
;;     registers[1] <- registers[1] - 2
;;     move register pointer right
;;   end while
;;   
;;   move register pointer left
;;   
;;   while registers[1] != 0 do
;;     print  registers[1]
;;   end while
(interpret-›*&«&^Jj
  "
  j
  \\
  J
  ^
  
  /
  &&
  
  j
  \\\\
  >*>*>*>*>*
  <<<<<<<<<<<<<<
  
  [
    <<
    /
    <<
    \\
  ]
  
  /
  
  [
    /
    ^
    \\
  ]
  ")
