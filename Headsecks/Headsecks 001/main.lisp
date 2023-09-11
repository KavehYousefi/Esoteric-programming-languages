;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Headsecks", invented by the Esolang user "Andkerosine" and
;; presented on May 21st, 2012, the programs of which encode
;; instructions from Urban Mueller's language "brainfuck" in Unicode
;; characters, the code points of which, when divided by the quantity
;; eight (8), yield a remainder in the integral range [0, 7], referring
;; to the digits commorant in the octal numeral system, the same,
;; equinumerant to brainfuck's operational cardinality, map in
;; accordance to an unambiguous scheme to the entheus' command roster.
;; 
;; 
;; Concept
;; =======
;; The Headsecks programming language is founded upon an encoding of
;; brainfuck instructions in Unicode characters whose character codes
;; ought to be divided by the number eight (8), producing a remainder
;; in the octal digit range [0, 7], which maps to the respective
;; octuple members of brainfuck by some accommodated scheme.
;; 
;; == THE DECODING PROCESS: FROM HEADSECKS TO BRAINFUCK ==
;; The decoding process approximates a quintiple of stages, enumerated
;; via an informal diction through the listing alow:
;; 
;;   (1) Prepare an empty ordered sequence bp for storing brainfuck
;;       instructions:
;;         bp <- empty ordered sequence
;;   
;;   (2) For each Headsecks character c determine the integral Unicode
;;       character point uc, that is:
;;         uc <- Unicode code point for character c
;;   
;;   (3) Divide uc by eight (8), yielding the octal digit oc as an
;;       integer number in the closed range [0, 7]:
;;         oc <- uc modulo 8
;;   
;;   (4) Query the decoding table for the brainfuck instruction bc which
;;       associates with the octal digit oc:
;;         bc <- decodingTable(oc)
;;   
;;   (5) Append the brainfuck instruction bc to the overall brainfuck
;;       output code bp:
;;         append bc to bp
;; 
;; The following pseudocode formulation shall communicate a tantamount
;; to the aboon explications by a more stringently regulated diction's
;; adminiculum:
;; 
;;   function decodeHeadsecks (headsecksCode, decodingTable)
;;     with:
;;       headsecksCode --- the Headsecks program to decode
;;       decodingTable --- a mapping o -> bc, from any octal digit o,
;;                         with o as an integer in [0, 7], to a
;;                         brainfuck instruction bc.
;;     
;;     brainfuckCode <- empty series of brainfuck instructions
;;     
;;     for each character c in headsecksCode do
;;       let uc <- get Unicode code point for c
;;       let oc <- uc mod 8
;;       let bc <- decodingTable(oc)
;;       
;;       append bc to brainfuckCode
;;     end for
;;     
;;     return brainfuckCode
;; 
;; == THE DECODING TABLE: A COMPONENT OF DISQUISITION ==
;; No imperative appertains to the actual mapping betwixt a Headsecks
;; cipher in its octal design and its brainfuck paregal; natheless, a
;; scheme is proffered as a rede, rather than a behest:
;; 
;;   ------------------------------------------
;;   Headsecks cipher | brainfuck command token
;;   -----------------+------------------------
;;   0                | +
;;   ..........................................
;;   1                | -
;;   ..........................................
;;   2                | <
;;   ..........................................
;;   3                | >
;;   ..........................................
;;   4                | .
;;   ..........................................
;;   5                | ,
;;   ..........................................
;;   6                | [
;;   ..........................................
;;   7                | ]
;;   ------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in Common Lisp, its circumference
;; amplecting a Headsecks code interpreter, as well as bidirectional
;; conversion routines betwixt the language and its entheus brainfuck.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-10
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2016Headsecks]
;;   The Esolang contributors, "Headsecks", March 16, 2016
;;   URL: "https://esolangs.org/wiki/Headsecks"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octal-digit ()
  "The ``octal-digit'' type defines a digit in the octal numeral system
   as an integral datum in the closed range [0, 7]."
  '(integer 0 7))

;;; -------------------------------------------------------

(deftype command-token-test ()
  "The ``command-token-test'' type defines a function responsible for
   assaying a character regarding its role as a token admissible to
   encoding and decoding, thus conforming to the signature
     lambda (character) => generalized boolean
   where the CHARACTER designates the token to probe for its eligibility
   as an effective constituent, and the GENERALIZED-BOOLEAN determines
   whether the CHARACTER shall be encoded or decoded, confirming upon a
   non-``NIL'' value, rejecting for ``NIL''."
  '(function (character) *))

;;; -------------------------------------------------------

(deftype brainfuck-instruction ()
  "The ``brainfuck-instruction'' type enumerates the recognized
   variations of brainfuck operations."
  '(member
    :increment
    :decrement
    :move-right
    :move-left
    :input
    :output
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype brainfuck-program ()
  "The ``brainfuck-program'' type defines an executable sequence of
   brainfuck instructions as a vector of zero or more
   ``brainfuck-instruction'' objects."
  '(vector brainfuck-instruction *))

;;; -------------------------------------------------------

(deftype decoding-table ()
  "The ``decoding-table'' type defines a function which maps Headsecks
   ciphers, represented by octal digits in the range [0, 7], to
   brainfuck commands, thus conforming to the signature
     lambda (octal-digit) => brainfuck-instruction
   where OCTAL-DIGIT constitutes the ``octal-digit'' Headsecks token,
   and BRAINFUCK-instruction the corresponding `brainfuck-instruction''
   datum.
   ---
   Upon the function's failure to accommodate a valid brainfuck
   equivalent to the supplied Headsecks cipher, an error of the type
   ``Decoding-Error'' shall be signaled."
  '(function (octal-digit) brainfuck-instruction))

;;; -------------------------------------------------------

(deftype encoding-table ()
  "The ``encoding-table'' type defines a function responsible for the
   encoding of a brainfuck instruction as a Headsecks octal digit
   cipher, thus conforming to the signature
     lambda (brainfuck-instruction) => octal-digit
   where BRAINFUCK-INSTRUCTION designates the brainfuck command to
   encode, and OCTAL-DIGIT specifies the resulting Headsecks cipher as
   an ``octal-digit''.
   ---
   The function is expected to signal an error of the type
   ``Encoding-Error'' if incapable of generating a cipher."
  '(function (brainfuck-instruction) octal-digit))

;;; -------------------------------------------------------

(deftype headsecks-program ()
  "The ``headsecks-program'' defines a basic Headsecks instruction
   sequences as vector of zero or more ciphers, represented as
   ``octal-digit''s."
  '(vector octal-digit *))

;;; -------------------------------------------------------

(deftype headsecks-formatter ()
  "The ``headsecks-formatter'' type defines a function which for a
   given Headsecks cipher issues a representation to the committed
   output sink, realized as a function conforming to the signature
     lambda (octal-digit destination) => ignored-result
   where the OCTAL-DIGIT represents the Headsecks cipher to write to the
   DESTINATION, and the DESTINATION specifies the output sink to print
   the formatted cipher, returning any result, which is simply ignored.
   ---
   The formatter function is expected to signal an error of the type
   ``Formatter-Error'' upon an anomalous situation's transpiration."
  '(function (octal-digit destination) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, such
   amplect, among others, the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table whose zero or more
   entries are composed of a key that conforms to the KEY-TYPE and a
   value which matches the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
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
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements which conform to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping betwixt forward and back
   jump points in a brainfuck program, mediated by their indices in the
   same, and realized as a hash table that associates the locations as
   fixnums."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of an octuple
   tally of accolent bits, thus being an occupant of the integral range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-vector ()
  "The ``cell-vector'' type defines an sparse vector of unsigned bytes,
   infinite in their bilateral extent, and realized as a hash table, the
   keys of which contribute the signed integer indices, and refer to
   ``octet'' elements."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation condition "Decoding-Error".                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Decoding-Error (error)
  ((offending-cipher
    :initarg       :offending-cipher
    :reader        decoding-error-offending-cipher
    :type          octal-digit
    :documentation "The Headsecks cipher, represented by an octal digit,
                    whose brainfuck equivalent could not be derived by a
                    decoder table."))
  (:report
    (lambda (condition stream)
      (declare (type Decoding-Error condition))
      (declare (type destination    stream))
      (format stream "The Headsecks cipher ~d could not be decoded."
        (decoding-error-offending-cipher condition))))
  (:documentation
    "The ``Decoding-Error'' condition serves to signal that, during the
     conversion of a Headsecks cipher stream, an encoded token could not
     be matched against a requested brainfuck command."))

;;; -------------------------------------------------------

(defun signal-decoding-error (offending-cipher)
  "Signals an error of the type ``Decoding-Error'' apprizing about the
   OFFENDING-CIPHER, which represent the Headsecks octal digit whose
   brainfuck equivalent could not be generated."
  (declare (type octal-digit offending-cipher))
  (error 'Decoding-Error :offending-cipher offending-cipher))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Headsecks-to-brainfuck converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-token-test +DEFAULT+HEADSECKS-TOKEN-TEST+))
(declaim (type decoding-table     +DEFAULT-DECODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +DEFAULT+HEADSECKS-TOKEN-TEST+
  #'(lambda (headsecks-token)
      (declare (type character headsecks-token))
      (declare (ignore         headsecks-token))
      T)
  "Defines a command token test which assigns to every character an
   operative agency, thus always returning the ``boolean'' value of
   ``T'', irregardless of the input.")

;;; -------------------------------------------------------

(defparameter +DEFAULT-DECODING-TABLE+
  #'(lambda (headsecks-cipher)
      (declare (type octal-digit headsecks-cipher))
      (the brainfuck-instruction
        (case headsecks-cipher
          (0         :increment)
          (1         :decrement)
          (2         :move-left)
          (3         :move-right)
          (4         :output)
          (5         :input)
          (6         :jump-forward)
          (7         :jump-back)
          (otherwise (signal-decoding-error headsecks-cipher)))))
  "Defines the default Headsecks-to-brainfuck converter table, which
   establishes the following equiparations:
     ------------------------------------------------------
     Headsecks cipher | brainfuck token | brainfuck command
     -----------------+-----------------+------------------
     0                | +               | :increment
     ......................................................
     1                | -               | :decrement
     ......................................................
     2                | <               | :move-left
     ......................................................
     3                | >               | :move-right
     ......................................................
     4                | .               | :output
     ......................................................
     5                | ,               | :input
     ......................................................
     6                | [               | :jump-forward
     ......................................................
     7                | ]               | :jump-back
     ------------------------------------------------------")

;;; -------------------------------------------------------

(defun decode-Headsecks-code
    (headsecks-code
     &key (command-token-test (constantly T))
          (decoding-table     +DEFAULT-DECODING-TABLE+))
  "Converts the piece of HEADSECKS-CODE into an equivalent brainfuck
   program, determining those input tokens to be construed as commands
   by the COMMAND-TOKEN-TEST's mediation, and employing the
   DECODING-TABLE in order to produce for an admitted Headsecks
   instruction character's octal cipher a brainfuck instruction, finally
   returning the thus generated brainfuck program."
  (declare (type string             headsecks-code))
  (declare (type command-token-test command-token-test))
  (declare (type decoding-table     decoding-table))
  (the brainfuck-program
    (coerce
      (loop
        for token of-type character across headsecks-code
        when (funcall command-token-test token)
          collect
            (funcall decoding-table
              (mod (char-code token) 8)))
      '(simple-array brainfuck-instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Encoding-Error".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Encoding-Error (error)
  ((offending-instruction
    :initarg       :offending-instruction
    :reader        encoding-error-offending-instruction
    :type          brainfuck-instruction
    :documentation "The brainfuck instruction whose equivalent Headsecks
                    cipher could not be determined."))
  (:report
    (lambda (condition stream)
      (declare (type Encoding-Error condition))
      (declare (type destination    stream))
      (format stream "The brainfuck instruction ~s could not be ~
                      encoded as a Headsecks cipher."
        (encoding-error-offending-instruction condition))))
  (:documentation
    "The ``Encoding-Error'' condition serves to signal an anomalous
     siguation involving the attempted encoding of a brainfuck
     instruction, whose endeavor has been inflicted with a failure."))

;;; -------------------------------------------------------

(defun signal-encoding-error (brainfuck-instruction)
  "Signals an error of the type ``Encoding-Error'', apprizing about the
   BRAINFUCK-INSTRUCTION whose encoding could not be accompassed."
  (declare (type brainfuck-instruction brainfuck-instruction))
  (error 'Encoding-Error :brainfuck-instruction brainfuck-instruction))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Headsecks-converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type encoding-table +DEFAULT-ENCODING-TABLE+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-ENCODING-TABLE+
  #'(lambda (brainfuck-instruction)
      (declare (type brainfuck-instruction brainfuck-instruction))
      (the octal-digit
        (case brainfuck-instruction
          (:increment    0)
          (:decrement    1)
          (:move-left    2)
          (:move-right   3)
          (:output       4)
          (:input        5)
          (:jump-forward 6)
          (:jump-back    7)
          (otherwise (signal-encoding-error brainfuck-instruction)))))
  "Defines the default mapping of brainfuck instructions to Headsecks
   ciphers, the latter communicated in the form of octal digits, thus
   assuming the guise:
     ----------------------------------------------------------
     brainfuck instruction | brainfuck token | Headsecks cipher
     ----------------------+-----------------+-----------------
     :increment            | +               | 0
     ..........................................................
     :decrement            | -               | 1
     ..........................................................
     :move-left            | <               | 2
     ..........................................................
     :move-right           | >               | 3
     ..........................................................
     :output               | .               | 4
     ..........................................................
     :input                | ,               | 5
     ..........................................................
     :jump-forward         | [               | 6
     ..........................................................
     :jump-back            | ]               | 7
     ----------------------------------------------------------")

;;; -------------------------------------------------------

(defun encode-brainfuck-program
    (brainfuck-program
     &key (encoding-table +DEFAULT-ENCODING-TABLE+))
  "Converts the BRAINFUCK-PROGRAM into a vector of Headsecks ciphers,
   each represented by an octal digit, by the ENCODING-TABLE's
   mediation, and returns the thus produced ``headsecks-program''."
  (declare (type brainfuck-program brainfuck-program))
  (declare (type encoding-table    encoding-table))
  (the headsecks-program
    (coerce
      (loop
        for brainfuck-instruction
          of-type brainfuck-instruction
          across  brainfuck-program
        collect
          (funcall encoding-table brainfuck-instruction))
      '(simple-array octal-digit (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition "Formatter-Error".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Formatter-Error (error)
  ((offending-cipher
    :initarg       :offending-cipher
    :reader        formatter-error-offending-cipher
    :type          octal-digit
    :documentation "The Headsecks cipher processed during the anomalous
                    situation's transpiration.")
   (offending-destination
    :initarg       :offending-destination
    :reader        formatter-error-offending-destination
    :type          destination
    :documentation "The destination processed during the anomalous
                    situation's transpiration."))
  (:report
    (lambda (condition stream)
      (declare (type Formatter-Error condition))
      (declare (type destination     stream))
      (format stream "An error has transpired during the printing of ~
                      the Headsecks cipher ~d to the destination ~s."
        (formatter-error-offending-cipher      condition)
        (formatter-error-offending-destination condition))))
  (:documentation
    "The ``Formatter-Error'' condition serves to signal that, during the
     printing of a Headsecks cipher, represented by an octal digit, to
     a destination, an anomalous situation has been detected."))

;;; -------------------------------------------------------

(defun signal-formatting-error (offending-cipher offending-destination)
  "Signals a ``Formatting-Error'' which refers to the OFFENDING-CIPHER
   and the OFFENDING-DESTINATION as the possible etiologies for this
   anomaly."
  (declare (type octal-digit offending-cipher))
  (declare (type destination offending-destination))
  (error 'Formatter-Error
    :offending-cipher      offending-cipher
    :offending-destination offending-destination))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Headsecks cipher formatter.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type headsecks-formatter +DEFAULT-HEADSECKS-FORMATTER+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-HEADSECKS-FORMATTER+
  #'(lambda (headsecks-cipher destination)
      (declare (type octal-digit headsecks-cipher))
      (declare (type destination destination))
      (format destination "~d" headsecks-cipher)
      (values))
  "Defines the default Headsecks cipher format, which simply issues the
   supplied cipher in its verbatim numeric form to its destination and
   returns no value.")

;;; -------------------------------------------------------

(defun format-Headsecks-program
    (headsecks-program
     &key (formatter   +DEFAULT-HEADSECKS-FORMATTER+)
          (destination NIL))
  "Prints the HEADSECKS-PROGRAM to the DESTINATION, representing its
   octal digit ciphers by the content issued through the FORMATTER, and
   returns for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise
   responding with a fresh string comprehending the result."
  (declare (type headsecks-program   headsecks-program))
  (declare (type headsecks-formatter formatter))
  (declare (type destination         destination))
  (the (or null string)
    (if destination
      (loop
        for headsecks-cipher
          of-type octal-digit
          across  headsecks-program
        do
          (funcall formatter headsecks-cipher destination))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (format-Headsecks-program headsecks-program
          :formatter   formatter
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (brainfuck-program)
  "Generates and returns for the BRAINFUCK-PROGRAM the jump table which
   associates the forward jump positions in the same with the back jump
   posts, and vice versa."
  (declare (type brainfuck-program brainfuck-program))
  (let ((jump-table          (make-hash-table :test #'eql))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for instruction
        of-type brainfuck-instruction
        across  brainfuck-program
      and position
        of-type fixnum
        from    0
        by      1
      
      if (eq instruction :jump-forward) do
        (push position forward-jump-points)
      else if (eq instruction :jump-back) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (setf (gethash start-point jump-table) end-point)
            (setf (gethash end-point   jump-table) start-point))
          (error "Unmatched back jump point at position ~d." position))
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-target (jump-table current-position)
  "Returns the position of the opposite jump point to the
   CURRENT-POSITION in the JUMP-TABLE, or signals an error of an
   unspecified type upon its absence."
  (declare (type jump-table jump-table))
  (declare (type fixnum     current-position))
  (the fixnum
    (or (gethash current-position jump-table)
        (error "No end point defined for the position ~d."
          current-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-brainfuck-program (brainfuck-program)
  "Executes the BRAINFUCK-PROGRAM and returns no value."
  (declare (type brainfuck-program brainfuck-program))
  (let ((ip           0)
        (jump-table   (build-jump-table brainfuck-program))
        (memory       (make-hash-table :test #'eql))
        (cell-pointer 0))
    (declare (type fixnum      ip))
    (declare (type cell-vector memory))
    (declare (type integer     cell-pointer))
    (flet
        ((program-terminated-p ()
          "Determines whether the BRAINFUCK-PROGRAM is exhausted, which
           imputes that the instruction pointer IP has transcended its
           bounds, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (the boolean
            (not (array-in-bounds-p brainfuck-program ip))))
         
         (get-current-instruction ()
          "Returns the instruction in the BRAINFUCK-PROGRAM at the
           current instruction pointer (IP) position, or ``NIL'' if the
           same has transcended the program's bournes."
          (the (or null brainfuck-instruction)
            (when (array-in-bounds-p brainfuck-program ip)
              (aref brainfuck-program ip))))
         
         (get-current-cell ()
          "Returns the byte value stored in the current cell."
          (the octet
            (gethash cell-pointer memory 0)))
         
         (set-current-cell (new-value)
          "Stores the NEW-VALUE in the current cell, contingently
           preceded by a wrapping adjustment in order to accommodate the
           cell range [0, 255], and returns no value."
          (declare (type integer new-value))
          (setf (gethash cell-pointer memory 0)
            (mod new-value 256))
          (values)))
      
      (loop until (program-terminated-p) do
        (let ((current-instruction (get-current-instruction)))
          (declare (type (or null brainfuck-instruction)
                         current-instruction))
          (case current-instruction
            ((NIL)
              (loop-finish))
            
            (:increment
              (set-current-cell
                (1+ (get-current-cell))))
            
            (:decrement
              (set-current-cell
                (1- (get-current-cell))))
            
            (:move-left
              (decf cell-pointer))
            
            (:move-right
              (incf cell-pointer))
            
            (:output
              (write-char
                (code-char
                  (get-current-cell))))
            
            (:input
              (format T "~&>> ")
              (finish-output)
              (set-current-cell
                (char-code
                  (read-char)))
              (clear-input))
            
            (:jump-forward
              (when (zerop (get-current-cell))
                (setf ip
                  (get-jump-target jump-table ip))))
            
            (:jump-back
              (unless (zerop (get-current-cell))
                (setf ip
                  (get-jump-target jump-table ip))))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                current-instruction ip))))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Headsecks
    (headsecks-code
     &key (command-token-test (constantly T))
          (decoding-table     +DEFAULT-DECODING-TABLE+))
  "Interprets the piece of HEADSECKS-CODE, aided by the
   COMMAND-TOKEN-TEST for the valid command characters' detection, and
   the DECODING-TABLE for mapping the Headsecks ciphers to brainfuck
   instructions, and returns no value."
  (declare (type string             headsecks-code))
  (declare (type command-token-test command-token-test))
  (declare (type decoding-table     decoding-table))
  (execute-brainfuck-program
    (decode-Headsecks-code headsecks-code
      :command-token-test command-token-test
      :decoding-table     decoding-table))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World".
;; 
;; The newline character contributes to the code by decrementing the
;; memory's cell pointer.
(interpret-Headsecks
  "☰☰☰☰☰☰☰☰☰☰☶☳☰☰☰☰☰☰☰☳☰☰☰☰☰☰☰☰☰☰☳☰☰☰☳☰☲☲☲☲☱☷☳☰☰☴☳☰☴☰☰☰☰☰☰☰☴☴☰☰☰☴☳☰☰☴
☲☰☰☰☰☰☰☰☰☰☰☰☰☰☰☰☴☳☴☰☰☰☴☱☱☱☱☱☱☴☱☱☱☱☱☱☱☱☴☳☰☴☳☴")

;;; -------------------------------------------------------

;; The "Konnichi wa" program, employing the kana dialect.
;; 
;; Please heed that is intended output could not yet be determined.
(interpret-Headsecks
  "ビるィせセコびせかルゎチこるビびッセッべゃ゛ッびんんルィ゛ィらィんニスゃヘ゠だヰヒたヱィホゃかにつめこゃッっカッニほっほゃレひゴぴずもオげずやずオぺオヲず゜コんるニ゛ヴビぼケるニレよこコにルほこかコにャニコほッゴリぺ゚をぺぢヲなゲりりぺぴペぢなペゲぢひひゔべラが")

;;; -------------------------------------------------------

;; An infinitely repeating cat program which terminates on a user input
;; of the "null character".
(interpret-Headsecks "EDFEDG")

;;; -------------------------------------------------------

;; Decodes the infinite loop Headsecks program and returns its brainfuck
;; paregal:
;;   (:increment :jump-forward :jump-back)
;; which is a tantamount of the brainfuck tokens
;;   +[]
(decode-Headsecks-code "PҾз")

;;; -------------------------------------------------------

;; Encodes an infinite cat program, presented as a brainfuck program, as
;; a Headsecks equivalent, employing the target range composed of the
;; Unicode code points 64 through 71, that is, the character set
;; {"@", "A", "B", "C", "D", "E", "F", "G"}.
;; 
;; The result string constitutes:
;;   "EDFEDG"
(format-Headsecks-program
  (encode-brainfuck-program
    (coerce
      '(:input
        :output
        :jump-forward
        :input
        :output
        :jump-back)
      'brainfuck-program))
  :formatter
    #'(lambda (headsecks-cipher destination)
        (declare (type octal-digit headsecks-cipher))
        (declare (type destination destination))
        (format destination "~c"
          (code-char
            (+ 64 headsecks-cipher)))))
