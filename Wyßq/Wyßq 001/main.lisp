;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Wyßq", invented by the Esolang user "Buckets" and
;; presented on November 12th, 2025, its proprium's patefaction the
;; NAND-combinations' adhibition on a bilaterally infinite dispansion
;; of integer-indexed cells, themselves commorancies to integral
;; elements, and the subsequent relocation in accordance with the
;; second NAND operand.
;; 
;; 
;; Concept
;; =======
;; The Wyßq programming language constitutes a specimen whose
;; competences' patefaction wists of a singular provenance only, an
;; instruction whose dever is realized in the logical NAND-combination
;; of two memory cells, concomitant to the result's tranfers into the
;; first participating operand, and the ultimity of the instruction
;; pointer's relocation to the command designated by the second
;; operand.
;; 
;; == THE MEMORY: AN INFINITE CATENA OF INTEGER-VALUED CELLS ==
;; The data castaldy's bailiwick limns an assignment to a bilaterally
;; infinite catena of cells, entalented with the amenability to signed
;; integer subscripts, and everichon among these units acting as the
;; salvatory to an aefauld signed integral datum, disencumbered in its
;; magnitude from any impositions.
;; 
;; == ONE INSTRUCTION, TWO CAUSATA: NAND-COMBINE AND JUMP ==
;; A single instruction, molded into the guise
;; 
;;   Wyßq A B
;;        * *
;; 
;; with "A" and "B" serving as the signed integer memory addresses,
;; entertains a participation in the language's operative avails; its
;; entelechy a champarty of two strata:
;; 
;;   (1) The memory cells amenable to the address {A} is NAND-combined
;;       with the cell located at {B}, the result's transfer transpiring
;;       into the first operand cell in order to overwrite its hitherto
;;       valid content:
;;       
;;         memory[A] <- memory[A] NAND memory[B]
;;       
;;       As a parergon's incorporation into this treatise, the following
;;       epexegesis appertains to the NAND truth table:
;;       
;;         ---------------------------
;;         Input 1 | Input 2 | Output
;;         --------+---------+--------
;;            0    |    0    |   1
;;         ...........................
;;            0    |    1    |   1
;;         ...........................
;;            1    |    0    |   1
;;         ...........................
;;            1    |    1    |   0
;;         ---------------------------
;;   
;;   (2) Imparting a perclose to an instruction's twyfold competences,
;;       the instruction pointer (IP) is relocated to the zero-based
;;       position of the program command designated by the value in
;;       the memory cells woning under the address {B}, decremented by
;;       a moutenance of one (1); scilicet:
;;       
;;         ip <- (memory[B] - 1)
;; 
;; 
;; Instructions
;; ============
;; An aefauld membership already serves in the instruction set's
;; exhaustion, its causatum the conflation of a NAND-combination and
;; an unconditional relocation of the program's instruction pointer
;; (IP).
;; 
;; == OVERVIEW ==
;; A bastant mete of nortelry's adhibition anent the language's
;; competences designates the following apercu's dever.
;; 
;; Please heed the demarcation of succedaneous content by a catena of
;; asterisks ("*"), their thus occupied tmemata intended for their
;; supplating by actual Wyßq code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   Wyßq A B | Sets the memory cell amenable to the address {A} to
;;        * * | the NAND-combination of itself as the first operand and
;;            | the memory cell at {B} as the second; subsequently,
;;            | relocate the instruction pointer (IP) to the command
;;            | at the zero-based index (memory[B] - 1).
;;            |--------------------------------------------------------
;;            | In a pseudocode diction, it holds:
;;            | 
;;            |   memory[A] <- memory[A] NAND memory[B]
;;            |   ip        <- (memory[B] - 1)
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort accompassed
;; in the programming language Common Lisp, the entire chevisance a
;; twissel of stages' coefficiency:
;; 
;;   (1) Imprimis, the Wyßq source code string experiences a
;;       reformulation inwith whose gestion the instructions are
;;       extracted into a vector of representative address operand
;;       tuples, collated in a zero-based sequence.
;;   
;;   (2) The process' coda mings of the thus assembled operations'
;;       application on the program memory.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen:2013:lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-07-19
;; 
;; Sources:
;;   [christensen:2013:lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang:2025:Wyßq]
;;   The Esolang contributors, "Wyßq", November 12th, 2025
;;   URL: "https://esolangs.org/wiki/Wy%C3%9Fq"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type, the agnomination of whom accompts for the
   TYPE-NAME's ipsissima verba dation, the formal parameters remaining
   an appropriation of the LAMBDA-LIST, and whose docimasy's subject
   entertains its accessibility via the CANDIDATE-NAME; evaluates the
   BODY forms, and construes the desinent form's primary return value
   as the perquisition's conclusion, with a \"generalized boolean\"
   value of \"true\" being considered paregal to the fathomed object's
   compatibility with the thus imposed antecedent, while a \"false\"
   response shall serve as its refutation.
   ---
   The first BODY form, upon its evaluation to a string, will be
   imputed to contribute the type definition's documentation string,
   and, as a corollary, will be reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or
          (and
            (stringp (first body))
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

(define-a-predicated-type hash-table-of (candidate
                                         &optional (key-type   '*)
                                                   (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency admitting zero or more entries, everichon key of which
   complies with the KEY-TYPE and is ligated to a value of the
   VALUE-TYPE, for both the generic sentinel ``*'' accommodates the
   default configuration."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and
          (typep current-key   key-type)
          (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*))
  "The ``list-of'' type defines a linked list enumerating zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   thilk is imposed the generic sentinel ``*'' as the default
   configuration."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype address-jumelle ()
  "The ``address-jumelle'' type defines the instruction operands as
   a twissel of addresses, their fons et origo the signed integer
   numbers, and its plasmature's patefaction a cons cell whose first
   tmema lays its amplection around the operand \"A\", while the second
   moeity's woning ostends an accommodation to the operand \"B\"."
  '(cons integer integer))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Wyßq as a one-dimensional
   simply array of ``address-jumelle'' objects, thilk contribute each
   instruction's operands."
  '(simple-array address-jumelle (*)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Wyßq program memory as a parse
   vector of signed integer-valued cells, themselves entalented with a
   ponibility derived from the selfsame vale, the manifestation a hash
   table whose keys and values both are desumed from the integer realm."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the binary operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nand-combine-the-bits (first-bit second-bit)
  "Returns the bit resulting from the NAND-combination of the FIRST-BIT
   and the SECOND-BIT in this exact order."
  (declare (type bit first-bit))
  (declare (type bit second-bit))
  (the bit
    (if (= first-bit second-bit 1)
      0
      1)))

;;; -------------------------------------------------------

(defun measure-the-maximum-bit-count (first-number second-number)
  "Returns the maximum accompt of bitx necessary for the representation
   of both the FIRST-NUMBER and the SECOND-NUMBER."
  (declare (type integer first-number))
  (declare (type integer second-number))
  (the (integer 0 *)
    (max
      (integer-length first-number)
      (integer-length second-number))))

;;; -------------------------------------------------------

(defun nand-combine-the-integers (first-number second-number)
  "Returns non-negative integer number representing the NAND-combination
   of the FIRST-NUMBER and the SECOND-NUMBER's bits."
  (declare (type integer first-number))
  (declare (type integer second-number))
  (the (integer 0 *)
    (loop
      with nand-result of-type (integer 0 *) = 0
      
      for bit-position
        of-type (integer -1 *)
        from    0
        below   (measure-the-maximum-bit-count
                  first-number
                  second-number)
      do
        (setf (ldb (byte 1 bit-position) nand-result)
          (nand-combine-the-bits
            (ldb (byte 1 bit-position) first-number)
            (ldb (byte 1 bit-position) second-number)))
      
      finally
        (return nand-result))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Interprets the OBJECT according to its aspect as a \"generalized
   boolean\" and returns a veridicous Boolean tantamount thereof,
   producing for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of character) +WHITESPACE-CHARACTERS+))

;;; -------------------------------------------------------

(defparameter +WHITESPACE-CHARACTERS+
  (list
    (code-char  9)              ;; Horizontal tab       (HT).
    (code-char 10)              ;; Newline or line feed (LF).
    (code-char 11)              ;; Vertical tabulation  (VT).
    (code-char 12)              ;; Form feed            (FF).
    (code-char 13)              ;; Carriage return      (CR).
    (code-char 32))             ;; Space                (SP).
  "Enumerates the whitespace characters in a list.")

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (find candidate +WHITESPACE-CHARACTERS+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-whitespace (source start)
  "Proceeding from the inclusive START position into the SOURCE, locates
   the nearest following whitespace and returns its position; or, upon
   its carency, responds with the SOURCE's length instead."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-the-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, locates
   the nearest following word and returns the inclusive position of its
   first character; or, upon its carency, responds with the SOURCE's
   length instead."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun demarcate-the-next-word (source start)
  "Proceeding from the inclusive START position into the SOURCE, locates
   the nearest following word and returns two values:
     (1) If a word could be detected, the inclusive position of its
         first character in the SOURCE; otherwise the SOURCE's length.
     (2) If a word could be detected, the position into the SOURCE
         immediately succeeding the word's desinent character; otherwise
         the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the (values fixnum fixnum)
    (let ((word-start-index (locate-the-next-word source start)))
      (declare (type fixnum word-start-index))
      (values
        word-start-index
        (locate-the-next-whitespace source word-start-index)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Wyßq lexical analyzer (lexer).         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun source-is-exhausted-p (source start end)
  "Determines whether the START and END positions into the SOURCE
   signify its desitive margent's transgression, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (declare (type fixnum        end))
  (the boolean
    (convert-into-a-boolean-value
      (= start end (length source)))))

;;; -------------------------------------------------------

(defun command-starts-at-p (source start end)
  "Determines whether a Wyßq command commences at the inclusive START
   position into the SOURCE and terminates immediately prevenient END
   index, signified by the occupation of the exact tmema atwixen these
   meares by the language keyword \"Wyßq\", returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (declare (type fixnum        end))
  (the boolean
    (convert-into-a-boolean-value
      (string= source "Wyßq" :start1 start :end1 end))))

;;; -------------------------------------------------------

(defun parse-an-operand (source start)
  "Proceeding from the inclusive START position into the SOURCE, parses
   the nearest following token as a signed or unsigned integer literal
   and returns two values:
     (1) The parsed integer value.
     (2) The position into the SOURCE immediately succeeding the parsed
         number's desinent character."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the (values integer fixnum)
    (multiple-value-bind (word-start-index word-end-index)
        (demarcate-the-next-word source start)
      (declare (type fixnum word-start-index))
      (declare (type fixnum word-end-index))
      (parse-integer source
        :start word-start-index
        :end   word-end-index))))

;;; -------------------------------------------------------

(defun extract-the-next-command (source start)
  "Proceeding from the START position into the SOURCE, attempts to
   locate the nearest following command and returns four values:
     (1) If a command could be detected, a ``boolean'' value of ``T'',
         otherwise ``NIL''.
     (2) If a command could be detected, the first operand, \"A\", as
         an integer number; otherwise the default value zero (0).
     (3) If a command could be detected, the second operand, \"B\", as
         an integer number; otherwise the default value zero (0).
     (4) If a command could be detected, the position into the SOURCE
         immediately succeeding the desinent character occupied by the
         command; otherwise the SOURCE's length as a signification of
         its exhaustion."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the (values boolean integer integer fixnum)
    (multiple-value-bind (word-start-index word-end-index)
        (demarcate-the-next-word source start)
      (declare (type fixnum word-start-index))
      (declare (type fixnum word-end-index))
      (cond
        ((source-is-exhausted-p source word-start-index word-end-index)
          (values NIL 0 0 word-end-index))
        ((command-starts-at-p source word-start-index word-end-index)
          (let ((first-operand  0)
                (second-operand 0))
            (declare (type integer first-operand))
            (declare (type integer second-operand))
            (multiple-value-setq (first-operand word-end-index)
              (parse-an-operand source word-end-index))
            (multiple-value-setq (second-operand word-end-index)
              (parse-an-operand source word-end-index))
            (values T first-operand second-operand word-end-index)))
        (T
          (error
            "Expected a command to start at the position ~d, but ~
             encountered the token ~s."
            word-start-index
            (subseq source word-start-index word-end-index)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Wyßq parser.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-wyßq-program (source)
  "Parses the piece of Wyßq SOURCE code and returns a one-dimensional
   simple array comprehending its ensconced command operand jumelles."
  (declare (type simple-string source))
  (the program
    (coerce
      (loop
        with ip              of-type fixnum  = 0
        with command-found-p of-type boolean = NIL
        with address-a       of-type integer = 0
        with address-b       of-type integer = 0
        do
          (multiple-value-setq (command-found-p address-a address-b ip)
            (extract-the-next-command source ip))
        while command-found-p
          collect (cons address-a address-b))
      '(simple-array address-jumelle (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``memory'' whose cells enkerly assume the
   default state of zero (0)."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun make-a-memory-initialized-with* (start-address
                                        &rest initial-elements)
  "Creates and returns a fresh ``memory'' whose attiguous cell catena,
   commencing from the inclusive START-ADDRESS upwards, acquire the
   INITIAL-ELEMENTS, and whose remaining contents are assigned the
   default state of zero (0)."
  (declare (type integer           start-address))
  (declare (type (list-of integer) initial-elements))
  (the memory
    (let ((memory (prepare-a-pristine-memory)))
      (declare (type memory memory))
      (loop
        for current-address of-type integer from start-address
        and current-element of-type integer in   initial-elements
        do  (setf (gethash current-address memory) current-element)
        finally (return memory)))))

;;; -------------------------------------------------------

(defun read-the-memory-cell-at (memory address)
  "Returns the value of the MEMORY cell amenable to the ADDRESS."
  (declare (type memory  memory))
  (declare (type integer address))
  (the integer
    (gethash address memory 0)))

;;; -------------------------------------------------------

(defun nand-combine-the-memory-cells (memory
                                      first-address
                                      second-address)
  "NAND-combines the value stored in MEMORY cell amenable to the
   FIRST-ADDRESS with that empight in the SECOND-ADDRESS, stores the
   result in the FIRST-ADDRESS, and returns no value."
  (declare (type memory  memory))
  (declare (type integer first-address))
  (declare (type integer second-address))
  (setf (gethash first-address memory)
    (nand-combine-the-integers
      (read-the-memory-cell-at memory first-address)
      (read-the-memory-cell-at memory second-address)))
  (values))

;;; -------------------------------------------------------

(defun demarcate-the-memory-address-range (memory)
  "Determines the MEMORY's extreme addresses for the range of those
   cells expressly modified and returns two values:
     (1) The inclusive smallest occupied address.
     (2) The inclusive largest  occupied address."
  (declare (type memory memory))
  (loop
    for current-address of-type integer being the hash-keys in memory
    minimize current-address into smallest-address
    maximize current-address into largest-address
    finally
      (return
        (values smallest-address largest-address))))

;;; -------------------------------------------------------

(defun print-the-memory (memory)
  "Prints the MEMORY in a human-readable plasmature to the standard
   output conduit and returns no value."
  (declare (type memory memory))
  (format T "~&Memory layout:")
  (multiple-value-bind (smallest-address largest-address)
      (demarcate-the-memory-address-range memory)
    (declare (type integer smallest-address))
    (declare (type integer largest-address))
    (loop
      for current-address
        of-type integer
        from    smallest-address
        to      largest-address
      do
        (format T "~&~2t[~d] = ~d" current-address
          (read-the-memory-cell-at memory current-address))))
  (force-output)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Wyßq interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (real 0 *) +DELAY-ATWIXEN-CYCLES+))

;;; -------------------------------------------------------

(defparameter +DELAY-ATWIXEN-CYCLES+ 1/4
  "The tally of seconds to delay execution atwixen two commands'
   evaluations.")

;;; -------------------------------------------------------

(defun execute-the-wyßq-program (program memory)
  "Executes the Wyßq PROGRAM, naiting the MEMORY, concluding the gestion
   with an ostention of the MEMORY's contents on the standard output
   conduit, and returns no value."
  (declare (type program program))
  (declare (type memory  memory))
  (loop
    with ip of-type fixnum = 0
    
    ;; Attempt to extract the next command, until the instruction
    ;; pointer (IP) has been relocated to an invalid position.
    while (array-in-bounds-p program ip) do
      (destructuring-bind (address-a . address-b)
          (aref program ip)
        (declare (type integer address-a))
        (declare (type integer address-b))
        ;; NAND-combine memory[A] and memory[B] into memory[A].
        (nand-combine-the-memory-cells memory address-a address-b)
        ;; Go to (memory[B] - 1).
        (setf ip
          (1- (read-the-memory-cell-at memory address-b))))
      (print-the-memory memory)
      (terpri)
      (terpri)
      (sleep +DELAY-ATWIXEN-CYCLES+))
  (print-the-memory memory)
  (values))

;;; -------------------------------------------------------

(defun interpret-the-wyßq-code
    (code
     &optional (memory (prepare-a-pristine-memory)))
  "Interprets the piece of Wyßq source CODE, naiting, if optated, a
   bespoke MEMORY, concluding the gestion with an ostention of the
   MEMORY's contents on the standard output conduit, and returns no
   value."
  (declare (type simple-string code))
  (declare (type memory        memory))
  (execute-the-wyßq-program
    (parse-the-wyßq-program code)
    memory)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This program implements an olamic iterance by jumping from the first
;; command, located at the index zero (0), to the third, at the position
;; two (2), and vice versa.
(interpret-the-wyßq-code
  "Wyßq  0  1
   Wyßq -1 -1
   Wyßq  0  1"
  (make-a-memory-initialized-with* 0 #b11 #b11))
