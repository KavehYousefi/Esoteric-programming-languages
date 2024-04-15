;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter as well as converters for the
;; esoteric programming language "ObfuscatedFuck", invented by the
;; Esolang user "Xyzzy" and presented on January 22nd, 2023, the
;; kenspeckle proprium of which resides in its entheus derived from
;; Urban Mueller's "brainfuck", the instructions' acquisition being
;; rendered a concomitant of an intermediate encoding, whence the
;; scion segues into the stock-father, and vice versa.
;; 
;; 
;; Concept
;; =======
;; The ObfuscatedFuck programming language's apprehension of the entire
;; foundational system, including the memory model, data types, and
;; operative warklumes, constitutes a verbatim appropriation from
;; brainfuck --- however, the syntaxis' components experience two strata
;; of indirection's administration, ere their capacitation to replicate
;; the entheus' contingencies is fulfilled.
;; 
;; == OBFUSCATEDFUCK: TOKENS TO BITS, BITS TO SYMBOLS ==
;; ObfuscatedFuck's nuncupated character repertoire serves in the
;; furnishment of a decoding process' inchoation, whence a bit string
;; is produced, segregated into binary triples, and this, in the
;; ultimity's entelechy, mapped to brainfuck's octuple instruction set.
;; 
;; == OBFUSCATEDFUCK TOKENS RESOLVE TO BIT SEQUENCES ==
;; A tally of fifteen symbols partakes of an effective agency in the
;; translation's incipient tier, producing from these characters bit
;; sequences of one two three binary digits' expansion:
;; 
;;   -----------------------------------
;;   ObfuscatedFuck token | Bit sequence
;;   ---------------------+-------------
;;   o                    | 0
;;   ...................................
;;   O                    | 0
;;   ...................................
;;   0                    | 0
;;   ===================================
;;   1                    | 1
;;   ...................................
;;   I                    | 1
;;   ...................................
;;   \                    | 1
;;   ...................................
;;   /                    | 1
;;   ...................................
;;   |                    | 1
;;   ===================================
;;   8                    | 00
;;   ...................................
;;   d                    | 01
;;   ...................................
;;   q                    | 01
;;   ===================================
;;   b                    | 10
;;   ...................................
;;   p                    | 10
;;   ...................................
;;   P                    | 10
;;   ...................................
;;   %                    | 101
;;   -----------------------------------
;; 
;; == BIT SEQUENCES CONCATENATE INTO A BINARY STRING ==
;; The produce of these ObfuscatedFuck symbol's bit sequences, ensuing
;; from a concatenation into a single binary string, establishes the
;; foundation for the subsequent segregation stage.
;; 
;; == THREE-BIT COMMAND CODES TRANSLATE TO BRAINFUCK COMMANDS ==
;; The catena of bits, when divide into triple-bit substrings, maps to
;; any of eight recognized brainfuck operation tokens:
;; 
;;   ----------------------------
;;   Bit code | brainfuck command
;;   ---------+------------------
;;   000      | +
;;   ............................
;;   001      | -
;;   ............................
;;   010      | <
;;   ............................
;;   011      | >
;;   ............................
;;   100      | .
;;   ............................
;;   101      | ,
;;   ............................
;;   110      | [
;;   ............................
;;   111      | ]
;;   ----------------------------
;; 
;; == THE DECODING PROCESS: OBFUSCATEDFUCK RESOLVES TO BRAINFUCK ==
;; The following enumeration's agency shall communicate the basic
;; stages from a piece of ObfuscatedFuck source code to the brainfuck
;; equivalency:
;; 
;;   (0) Prepare an empty brainfuck output stream p[out] and an empty
;;       bit string output stream p[bin].
;;   
;;   (1) Iterate each character c[in] of the ObfuscatedFuck program
;;       p[in].
;;       
;;       (1.1) Translate the character c[in] into the bit sequence
;;             affiliated in concord with the language' stipulations,
;;             thus obtaining a bit string of one to three characters'
;;             size, c[bin].
;;       
;;       (1.2) Append the bit sequence c[bin] to the binary output
;;             stream p[bin].
;;   
;;   (2) Iterate each triple of consecutive binary digits c[code] from
;;       the binary output stream p[bin].
;;       
;;       (2.1) Translate the binary digits c[code] into the
;;             corresponding brainfuck command token c[out] according to
;;             the language's stipulations.
;;       
;;       (2.2) Append the brainfuck command token c[out] to the
;;             brainfuck output stream p[out].
;; 
;; 
;; Instructions
;; ============
;; The recipient of brainfuck's legacy, ObfuscatedFuck expresses a
;; congruency of the octuple instruction set by a discrepant symbol
;; repertoire.
;; 
;; == OVERVIEW ==
;; Proceeding from this gnarity anenst the two language's operative
;; equiparation, brainfuck's commands shall be elucidated in a tabular
;; apercu:
;; 
;;   ------------------------------------------------------------------
;;   brainfuck command | Effect
;;   ------------------+-----------------------------------------------
;;   +                 | Increments the current cell value by one. Upon
;;                     | the upper bourne's transgression, settled at
;;                     | the value 255, the state wraps around to the
;;                     | minimum of zero (0).
;;   ..................................................................
;;   -                 | Decrements the current cell value by one. Upon
;;                     | the lower bourne's transgression, settled at
;;                     | the value zero (0), the state wraps around to
;;                     | the maximum of 255.
;;   ..................................................................
;;   <                 | Translates the cell pointer one step to the
;;                     | left.
;;   ..................................................................
;;   >                 | Translates the cell pointer one step to the
;;                     | right.
;;   ..................................................................
;;   .                 | Prints the character whose ASCII code
;;                     | corresponds to the current cell value to the
;;                     | standard output.
;;   ..................................................................
;;   ,                 | Queries the standard input for a character and
;;                     | stores its ASCII code in the current cell.
;;   ..................................................................
;;   [                 | If the current cell value equals zero (0),
;;                     | moves the instruction pointer (IP) forward to
;;                     | the position immediately succeeding the
;;                     | matching "]" instruction; otherwise proceeds
;;                     | as usual.
;;   ..................................................................
;;   ]                 | If the current cell value does not equal
;;                     | zero (0), moves the instruction pointer (IP)
;;                     | back to the position immediately succeeding
;;                     | the matching "[" instruction; otherwise
;;                     | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-12
;; 
;; Sources:
;;   [esolang2023ObfuscatedFuck]
;;   The Esolang contributors, "ObfuscatedFuck", September 3rd, 2023
;;   URL: "https://esolangs.org/wiki/ObfuscatedFuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
  (type-name (candidate-variable &rest lambda-list)
   &body body)
  "Defines a derived type yclept by the TYPE-NAME, and appropriating its
   formal parameters verbatim from the LAMBDA-LIST, while the
   implementation, provided via the BODY forms, accesses the object
   subjected to th docimasy by the CANDIDATE-VARIABLE's agnomination,
   the desinent evaluated BODY form's primary return value, if amounting
   to a generalized boolean truth value of \"true\", vouches for the
   candidate's eligibility, while a \"false\" response refutes the
   same.
   ---
   The first BODY form, is establishing a string object, is construed as
   the derived type's documentation string and reappropriated for this
   purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body)) (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism of which lays its amplectation around such operations as
   ``format'' and ``write-char'', as forbisens to be adduced."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-predicated-type association-list-of
    (candidate
     &optional (key-type   '*)
               (value-type '*)
               (length     '*))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of the LENGTH tally of entries, the same may be
   chosen arbitrarily if equal to the sentinel symbol ``*'' or omitted,
   each such entry's key complying to the KEY-TYPE and the associated
   value to the VALUE-TYPE, for both holds the default generic sentinel
   of ``*''."
  (and
    (listp candidate)
    (or (eq length '*)
        (= (length (the list candidate)) length))
    (loop
      for    element of-type T in (the list candidate)
      always (typep element `(cons ,key-type ,value-type)))))

;;; -------------------------------------------------------

(deftype token-table ()
  "The ``token-table'' type defines a bidirectional mapping betwixt the
   ObfuscatedFuck tokens and their bit string equivalents, realized in
   an association list, or alist, the entries of which assign to every
   token character a simple string tantamout."
  '(association-list-of character simple-string 15))

;;; -------------------------------------------------------

(deftype command-code-table ()
  "The ``command-code-table'' type defines a bidirectional mapping
   betwixt the ObfuscatedFuck tokens' bit string representation and the
   equivalent brainfuck token, realized in an association list, or
   alist, the entries of which assign to every triad of binary digits,
   ensconced in a simple string, the tantamount character entity."
  '(association-list-of (simple-string 3) character 8))

;;; -------------------------------------------------------

(deftype binary-encoding-table ()
  "The ``binary-encoding-table'' type defines a bidirectional mapping
   betwixt the octuple brainfuck operations' bit string equivalents and
   a chosen ObfuscatedFuck token sequence representation, realized in an
   association list, or alist, the keys of which are established in
   simple strings of a triad's capacity, answering to simple string
   ObfuscatedFuck characters."
  '(association-list-of (simple-string 3) simple-string 8))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type   '*)
               (value-type '*))
  "The ``hash-table-of'' type defines a hash table the componency of
   which enumerates a tally of zero or more entries, each key of which
   conforms to the KEY-TYPE and whose value assumes the VALUE-TYPE, both
   of which default to the generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and
          (or (eq key-type '*)
              (typep key key-type))
          (or (eq value-type '*)
              (typep value value-type))))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional mapping betwixt the
   forward and back jump points in a brainfuck program, realized by
   mediation of their positions inside of the respective code, and
   manifesting in a hash table whose keys and values both assume
   fixnum-typed subscripts."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list whose componency enumerates a
   tally of zero or more elements, each siccan complies with the
   ELEMENT-TYPE, defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (every
        #'(lambda (element)
            (declare (type T element))
            (typep element element-type))
        (the list candidate)))))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value commorant in the
   closed integer interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of unsigned
   byte-valued, amenable to a signed integer index, and realized by
   mediation of a hash table, the integral keys of which answer to
   ``octet'' values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token decoder.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type token-table +TOKEN-TABLE+))

;;; -------------------------------------------------------

(defparameter +TOKEN-TABLE+
  '((#\o . "0")
    (#\0 . "0")
    (#\O . "0")
    (#\1 . "1")
    (#\I . "1")
    (#\\ . "1")
    (#\/ . "1")
    (#\| . "1")
    (#\8 . "00")
    (#\d . "01")
    (#\q . "01")
    (#\b . "10")
    (#\p . "10")
    (#\P . "10")
    (#\% . "010"))
  "Associates the ObfuscatedFuck tokens with the corresponding bit
   sequences in a bilateral mode.")

;;; -------------------------------------------------------

(defun get-token-entry (token)
  "Returns the entry for the ObfuscatedFuck TOKEN in the +TOKEN-TABLE+,
   or ``NIL'' upon its disrespondency."
  (declare (type character token))
  (the (or null (cons character simple-string))
    (assoc token +TOKEN-TABLE+ :test #'char=)))

;;; -------------------------------------------------------

(defun obfuscatedFuck-token-p (candidate)
  "Determines whether the CANDIDATE represents a recognized
   ObfuscatedFuck token, such resolves to a bit sequence, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (get-token-entry candidate)))))

;;; -------------------------------------------------------

(defun decode-obfuscatedFuck-token (token)
  "Returns the bit sequence associated with the ObfuscatedFuck token."
  (declare (type character token))
  (the simple-string
    (or (cdr (get-token-entry token))
        (error "No ObfuscatedFuck token: ~s." token))))

;;; -------------------------------------------------------

(defun binary-decode-obfuscatedFuck-program
    (code
     &optional (destination NIL))
  "Generates for the piece of ObfuscatedFuck source CODE the equivalent
   bit string representation, writes the same to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value; otherwise
   responding with a fresh string comprehending the result."
  (declare (type string      code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop for token of-type character across code do
        (when (obfuscatedFuck-token-p token)
          (format destination "~a"
            (decode-obfuscatedFuck-token token))))
      (with-output-to-string (bit-string)
        (declare (type string-stream bit-string))
        (binary-decode-obfuscatedFuck-program code bit-string)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of bit string decoder.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-code-table +COMMAND-CODE-TABLE+))

;;; -------------------------------------------------------

(defparameter +COMMAND-CODE-TABLE+
  '(("000" . #\+)
    ("001" . #\-)
    ("010" . #\<)
    ("011" . #\>)
    ("100" . #\.)
    ("101" . #\,)
    ("110" . #\[)
    ("111" . #\]))
  "Associates the recognized ObfuscatedFuck command codes, represented
   by bit strings, to the corresponding brainfuck instruction tokens,
   utilizing for this endeavor an association list whose keys are
   provided by binary strings and whose values assume single
   characters.")

;;; -------------------------------------------------------

(defun decode-command-code (command-code)
  "Returns the brainfuck command token corresponding to the
   ObfuscatedFuck COMMAND-CODE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type (string 3) command-code))
  (the character
    (or (cdr (assoc command-code +COMMAND-CODE-TABLE+ :test #'string=))
        (error "The string ~s does not represent an ObfuscatedFuck ~
                command code."
          command-code))))

;;; -------------------------------------------------------

(defun decode-bit-string (bits &optional (destination NIL))
  "Transcripts the ObfuscatedFuck bit string BITS into the equivalent
   brainfuck source code, writes the same to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   responding with a fresh string comprehending the output."
  (declare (type string      bits))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for command-offset
          of-type fixnum
          from    0
          below   (length bits)
          by      3
        for command-code
          of-type (string 3)
          =       (subseq bits command-offset
                    (+ command-offset 3))
        do
          (format destination "~c"
            (decode-command-code command-code)))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (decode-bit-string bits brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ObfuscatedFuck decoder.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-ObfuscatedFuck-program (obfuscatedFuck-code
                                      &optional (destination NIL))
  "Translates the piece of OBFUSCATEDFUCK-CODE into its brainfuck
   equivalent, writes the result to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the output."
  (declare (type string      obfuscatedFuck-code))
  (declare (type destination destination))
  (the (or null string)
    (decode-bit-string
      (binary-decode-obfuscatedFuck-program obfuscatedFuck-code)
      destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary encoder.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type binary-encoding-table +BINARY-DECODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +BINARY-ENCODING-TABLE+
  '(("000" . "8o")
    ("001" . "81")
    ("010" . "%")
    ("011" . "d1")
    ("100" . "bo")
    ("101" . "1d")
    ("110" . "1b")
    ("111" . "111"))
  "Maps the bit string representations of the octuple brainfuck
   instructions to ObfuscatedFuck tokens.")

;;; -------------------------------------------------------

(defun encode-command-code (bits &optional (destination NIL))
  "Detects an ObfuscatedFuck token sequence answering to the bit string
   BITS, writes the same to the DESTINATION, and returns for a
   non-``NIL'' the ``NIL'' value, otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh string comprehending the output."
  (declare (type string      bits))
  (declare (type destination destination))
  (the (or null string)
    (format destination "~a"
      (or (cdr (assoc bits +BINARY-ENCODING-TABLE+ :test #'string=))
          (error "No ObfuscatedFuck token answers to the bit string ~s."
            bits)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck encoder.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (token)
  "Determines whether the TOKEN is associated with a brainfuck command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (rassoc token +COMMAND-CODE-TABLE+ :test #'char=)))))

;;; -------------------------------------------------------

(defun binary-encode-brainfuck-command (brainfuck-command)
  "Returns the ObfuscatedFuck bit string associated with the
   BRAINFUCK-COMMAND."
  (declare (type character brainfuck-command))
  (the string
    (or (car (rassoc brainfuck-command +COMMAND-CODE-TABLE+
               :test #'char=))
        (error "The token \"~c\" does not represent a brainfuck ~
                operation."
          brainfuck-command))))

;;; -------------------------------------------------------

(defun binary-encode-brainfuck-program (brainfuck-code
                                        &optional (destination NIL))
  "Generates for the piece of BRAINFUCK-CODE an
   ObfuscatedFuck-compatible bit string equivalency, writes the same to
   the DESTINATION, and returns for a non-``NIL'' DESTINATION the
   ``NIL'' value, otherwise responding with a fresh string comprehending
   the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for brainfuck-token
          of-type character
          across  brainfuck-code
        when (brainfuck-command-p brainfuck-token) do
          (format destination "~a"
            (binary-encode-brainfuck-command brainfuck-token)))
      (with-output-to-string (bit-string)
        (declare (type string-stream bit-string))
        (binary-encode-brainfuck-program brainfuck-code bit-string)))))

;;; -------------------------------------------------------

(defun encode-brainfuck-command (brainfuck-command
                                 &optional (destination NIL))
  "Detects for the BRAINFUCK-COMMAND an equivalent ObfuscatedFuck token
   sequence and writes the same to the DESTINATION, and returns for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise, for a ``NIL''
   DESTINATION, responds with a fresh string comprehending the result."
  (declare (type character   brainfuck-command))
  (declare (type destination destination))
  (the (or null string)
    (encode-command-code
      (binary-encode-brainfuck-command brainfuck-command)
      destination)))

;;; -------------------------------------------------------

(defun encode-brainfuck-program (brainfuck-code
                                 &optional (destination NIL))
  "Translates the piece of BRAINFUCK-CODE into an equivalent
   ObfuscatedFuck program, writes the result to the DESTINATION, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise,
   for a ``NIL'' DESTINATION, responds with a fresh string comprehending
   the output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for brainfuck-token
          of-type character
          across  brainfuck-code
        when (brainfuck-command-p brainfuck-token) do
          (format destination "~a"
            (encode-brainfuck-command brainfuck-token)))
      (with-output-to-string (obfuscatedFuck-code)
        (declare (type string-stream obfuscatedFuck-code))
        (encode-brainfuck-program
          brainfuck-code
          obfuscatedFuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the jump START-POINT and END-POINT in the JUMP-TABLE in a
   bidirectional manner and returns no value.
   ---
   Any extant entry key amenable to either the START-POINT, END-POINT,
   or both, is superseded."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-jump-table (brainfuck-code)
  "Calculates and returns for the piece of BRAINFUCK-CODE a jump table,
   responsible for the connection of its jump points by adminiculum of
   their locations inside of the program."
  (declare (type string brainfuck-code))
  (let ((jump-table   (make-hash-table :test #'eql))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for position
        of-type fixnum
        from    0
        below   (length brainfuck-code)
      for token
        of-type character
        =       (char brainfuck-code position)
      
      if (char= token #\[) do
        (push position start-points)
      else if (char= token #\]) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            position)
          (error "Unmatched jump end point at position ~d." position))
      end
      
      finally
        (when start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points) start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-opposite-jump-point (jump-table current-position)
  "Returns the jump point opposite to the CURRENT-POSITION in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     current-position))
  (the fixnum
    (or (gethash current-position jump-table)
        (error "No jump point associated with the position ~d."
          current-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          cell-table
    :documentation "A sparse vector of unsigned byte-valued cells.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The mobile cell pointer, amenable to gradual
                    translations along both of the tape's axes,
                    selecting the currently active cell."))
  (:documentation
    "The ``Tape'' class implements the program memory as a bilaterally
     infinite type of unsigned byte-valued cells, operated upon by a
     motile cell pointer, responsible for the designation of the
     currently active unit, that amenable to modulations and
     perqusitions."))

;;; -------------------------------------------------------

(defmacro with-tape ((tape) &body body)
  "Evaluates the TAPE, binds its slot ``cells'' to the local symbol
   macro ``$cells'' and ``pointer'' to ``$pointer'', evaluates the BODY
   forms, and returns the desinent form's results."
  (let ((evaluated-tape (gensym)))
    (declare (type symbol evaluated-tape))
    `(let ((,evaluated-tape ,tape))
       (declare (type Tape ,evaluated-tape))
       (declare (ignorable ,evaluated-tape))
       (symbol-macrolet
           (($cells
             (the cell-table
               (slot-value ,evaluated-tape 'cells)))
            ($pointer
             (the integer
               (slot-value ,evaluated-tape 'pointer))))
         (declare (type cell-table $cells))
         (declare (ignorable       $cells))
         (declare (type integer    $pointer))
         (declare (ignorable       $pointer))
         ,@body))))

;;; -------------------------------------------------------

(defun current-cell-value (tape)
  "Returns the unsigned byte value stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the integer
    (with-tape (tape)
      (gethash $pointer $cells 0))))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   subjecting the same to a wrapping into the recognized unsigned byte
   range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (with-tape (tape)
    (setf (gethash $pointer $cells 0)
      (mod new-value 256)))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (with-tape (tape)
    (incf $pointer))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (with-tape (tape)
    (decf $pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck interpreter.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-character ()
  "Queries the standard input for a character and returns its ASCII
   code."
  (format T "~&>> ")
  (finish-output)
  (the fixnum
    (prog1
      (char-code
        (read-char NIL NIL #\Null))
      (clear-input))))

;;; -------------------------------------------------------

(defun interpret-brainfuck (code)
  "Interprets the piece of brainfuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (supputate-jump-table code))
        (tape     (make-instance 'Tape)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Tape       tape))
    (symbol-macrolet ((token (the character (char code ip))))
      (declare (type character token))
      (loop while (< ip (length code)) do
        (case token
          (#\+ (incf (current-cell-value tape)))
          (#\- (decf (current-cell-value tape)))
          (#\< (move-cell-pointer-left tape))
          (#\> (move-cell-pointer-right tape))
          (#\. (write-char (code-char (current-cell-value tape))))
          (#\, (setf (current-cell-value tape)
                 (query-for-character)))
          (#\[ (when (zerop (current-cell-value tape))
                 (setf ip
                   (get-opposite-jump-point jump-table ip))))
          (#\] (unless (zerop (current-cell-value tape))
                 (setf ip
                   (get-opposite-jump-point jump-table ip))))
          (otherwise NIL))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-ObfuscatedFuck (code)
  "Interprets the piece of ObfuscatedFuck source CODE and returns no
   value."
  (declare (type string code))
  (interpret-brainfuck
    (decode-ObfuscatedFuck-program code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!".
(interpret-ObfuscatedFuck
  "8o8o8o8o8o8o8o8o1bd18o8o8o8o1bd18o8od18o8o8od18o8o8od18o%%%%81111d18od18od181d1d18o1b%111%81111d1d1bod1818181bo8o8o8o8o8o8o8obobo8o8o8obod1d1bo%81bo%bo8o8o8obo818181818181bo8181818181818181bod1d18obod18o8obo")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on an input of the
;; "null character" (ASCII code = 0).
(interpret-ObfuscatedFuck "1d1bbo1d111")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-ObfuscatedFuck "1dbo1b8181d18o1bd1d1111%1bbo111%%111")

;;; -------------------------------------------------------

;; Generate the ObfuscatedFuck equivalent to the brainfuck repeating cat
;; program and prin the same to the standard output.
(encode-brainfuck-program ",[.,]" T)
