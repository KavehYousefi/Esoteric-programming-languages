;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ASCIIfuck", invented by the Esolang user "ChuckEsoteric08"
;; and presented on December 30th, 2023, the proprium maintaining its
;; woning in it being an encoding of Urban Mueller's "brainfuck" inside
;; of the ASCIIfuck source character's ASCII codes, the same are split
;; into a numeric equivalent of the entheus' octuple instruction set and
;; a repetition specifier for the tally of its invocations.
;; 
;; 
;; Concept
;; =======
;; The ASCIIfuck programming language constitutes a variation on
;; brainfuck founded upon the stock-father's octuple instruction set's
;; encoding in the source code character's ASCII codes, their
;; concameration into a twain of digits produces both a numeric cipher
;; in the equipollent range [1, 8] to carry the operational identity,
;; and, as a parhedral second moiety, the number of times to repeat the
;; causatum.
;; 
;; == DECODING ASCIIFUCK TO BRAINFUCK ==
;; An ASCIIfuck program's decoding proceeds by the conversion of each of
;; its characters into the corresponding ASCII code, splitting the same
;; into a twain of command code and repetition count, with the former
;; mapping values in the closed interval of [1, 8] to the octuple
;; brainfuck operations, while the latter impose the tally of times to
;; invoke the decoded behest.
;; 
;; A more stringent formality embues this species of presentation, a
;; pseudocode treatise upon the ASCIIfuck to brainfuck decoding process:
;; 
;;   function decodeASCIICode (asciiCode)
;;     Input:
;;       asciiCode --- The unsigned byte value to split into a twain of
;;                     command code and repetition count.
;;     
;;     Output:
;;       parts     --- A tuple (commandCount, repetitionCount), where
;;                     the commandCount imposes a restriction to the
;;                     integral range [0, 10], whereas the
;;                     repetitionCount remains in the integer
;;                     range [0, 9].
;;     
;;     Process:
;;       let parts <- nil
;;       
;;       if asciiCode < 10 then
;;         parts <- (0,  asciiCode)
;;       else if 10 <= asciiCode < 100
;;         let leftDigit  <- floor(asciiCode / 10)
;;         let rightDigit <- asciiCode modulo 10
;;         parts          <- (leftDigit, rightDigit)
;;       else
;;         let leftDigit       <- floor(asciiCode / 100)
;;         let remainingDigits <- asciiCode modulo 100
;;         let centerDigit     <- floor(remainingDigits / 10)
;;         let rightDigit      <- remainingDigits modulo 10
;;         parts               <- (leftDigit + centerDigit, rightDigit)
;;       end if
;;       
;;       return parts
;;   end function
;;   
;;   
;;   function getBrainfuckCommand (commandCode)
;;     Input:
;;       commandCode      --- An integer number in the range [0, 10]
;;                            which, when commorant in the valid
;;                            sub-interval [1, 8], provides an
;;                            ASCIIfuck encoding of a brainfuck command.
;;     
;;     Output:
;;       brainfuckCommand --- The brainfuck command token which is
;;                            encoded by the numeric commandCode.
;;     
;;     Process:
;;       let brainfuckCommand <- nil
;;       
;;       if commandCode = 1 then
;;         brainfuckCommand <- "+"
;;       else if commandCode = 2 then
;;         brainfuckCommand <- "-"
;;       else if commandCode = 3 then
;;         brainfuckCommand <- ">"
;;       else if commandCode = 4 then
;;         brainfuckCommand <- "<"
;;       else if commandCode = 5 then
;;         brainfuckCommand <- ","
;;       else if commandCode = 6 then
;;         brainfuckCommand <- "."
;;       else if commandCode = 7 then
;;         brainfuckCommand <- "["
;;       else if commandCode = 8 then
;;         brainfuckCommand <- "]"
;;       else
;;         error: "Invalid command code."
;;       end if
;;       
;;       return brainfuckCommand
;;   end function
;;   
;;   
;;   function decodeASCIIfuckToBrainfuck (asciifuckCode)
;;     Input:
;;       asciifuckCode --- The piece of ASCIIfuck source code to convert
;;                         into a brainfuck program.
;;     
;;     Output:
;;       brainfuckCode --- The brainfuck source code which is equivalent
;;                         to the input asciifuckCode.
;;     
;;     Process:
;;       let brainfuckCode <- empty string
;;       
;;       for each character c in the ASCIIfuck code do
;;         let asciiCode                  <- ASCII code for character c
;;         let (commandCode, repetitions) <- decodeASCIICode(asciiCode)
;;         
;;         let brainfuckCommand <- getBrainfuckCommand(commandCode)
;;         
;;         repeat repetitions do
;;           append brainfuckCommand to brainfuckCode
;;         end repeat
;;       end for
;;       
;;       return brainfuckCode
;; 
;; == ASCIIFUCK COMMAND CODES MAP TO BRAINFUCK COMMAND TOKENS ==
;; A cursory ilk of a nortelry's administration, with a curtailed
;; mentioning of the operation's causatum, shall be the following
;; juxtaposition's dation, the same equiparates the numeric ASCIIfuck
;; command codes with their affiliated brainfuck instruction tokens.
;; 
;;   ------------------------------------------------------------------
;;   Command code | brainfuck command | Effect
;;   -------------+-------------------+--------------------------------
;;   1            | +                 | Increment current cell
;;   ..................................................................
;;   2            | -                 | Decrement current cell
;;   ..................................................................
;;   3            | >                 | Move cell pointer right
;;   ..................................................................
;;   4            | <                 | Move cell pointer left
;;   ..................................................................
;;   5            | ,                 | Input character
;;   ..................................................................
;;   6            | .                 | Output character
;;   ..................................................................
;;   7            | [                 | Jump forward
;;   ..................................................................
;;   8            | ]                 | Jump back
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-06
;; 
;; Sources:
;;   [esolang2023ASCIIfuck]
;;   The Esolang contributors, "ASCIIfuck", December 30th, 2023
;;   URL: "https://esolangs.org/wiki/ASCIIfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ascii-code ()
  "The ``ascii-code'' type defines an ASCII character code as an
   integral value in the range [0, 255]."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype decimal-digit ()
  "The ``decimal-digit'' type defines a decimal digit as an integral
   object desumed from the range [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype decimal-digit-pair ()
  "The ``decimal-digit-pair'' type defines a compound of two consecutive
   decimal digit encoded as a single integral object desumed from the
   range [0, 99]."
  '(integer 0 99))

;;; -------------------------------------------------------

(deftype command-code ()
  "The ``command-code'' type defines the encoding of a brainfuck
   instruction in an integral object as an integer number occupying the
   closed interval [1, 8]."
  '(integer 1 8))

;;; -------------------------------------------------------

(deftype brainfuck-command ()
  "The ``brainfuck-command'' enumerates the valid brainfuck operation
   types."
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
  "The ``brainfuck-program'' type defines an executable brainfuck
   program as a vector compact of zero or more ``brainfuck-command''
   objects."
  '(vector brainfuck-command *))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the same
   defaults to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose zero or more
   entries are composed of keys conforming to the KEY-TYPE and values
   assuming the VALUE-TYPE, both defaulting to the generic sentinel
   ``*''."
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
                (and
                  (typep key   key-type)
                  (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   forward and back jump points in a brainfuck program, both represented
   by their zero-based position into the same."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits as an integer number whose elements wone in the closed interval
   of [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, a
   realm that encloses, among others, the operations ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ASCIIfuck processor.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-digits (number)
  "Extracts from the NUMBER its three digits, and returns these as three
   values:
     (1) The digit located at the highest-valued position in the NUMBER.
     (2) The digit located at the second highest-valued position in the
         NUMBER.
     (3) The digit located at the lowest-valued position in the NUMBER."
  (declare (type ascii-code number))
  (the (values decimal-digit decimal-digit decimal-digit)
    (multiple-value-bind (left-digit center-and-right-digits)
        (floor number 100)
      (declare (type decimal-digit      left-digit))
      (declare (type decimal-digit-pair center-and-right-digits))
      (multiple-value-bind (center-digit right-digit)
          (floor center-and-right-digits 10)
        (declare (type decimal-digit center-digit))
        (declare (type decimal-digit right-digit))
        (values left-digit center-digit right-digit)))))

;;; -------------------------------------------------------

(defun split-ascii-code (ascii-code)
  "Splits the ASCII-CODE asunder into its two significant compartments
   and returns these as two values:
     (1) The sum of the left and center digits, any of which defaults to
         zero (0) if the ASCII-CODE's expanse does not suffice.
     (2) The right digit."
  (declare (type ascii-code ascii-code))
  (multiple-value-bind (left-digit center-digit right-digit)
      (extract-digits ascii-code)
    (declare (type decimal-digit left-digit))
    (declare (type decimal-digit center-digit))
    (declare (type decimal-digit right-digit))
    (the (values (integer 0 10) decimal-digit)
      (values
        (+ left-digit center-digit)
        right-digit))))

;;; -------------------------------------------------------

(defun decode-character (token)
  "Decodes the TOKEN into its command code and its number of
   repetitions, returning two values:
     (1) The command code, as an integral number in the range [1, 8],
         which specifies the brainfuck operation to perform.
     (2) The tally of repetitions, as an integral number in the range
         [0, 9], that specifies the number of times to repeat the
         command designated in the first return value (1)."
  (declare (type character token))
  (multiple-value-bind (command-code number-of-repetitions)
      (split-ascii-code
        (char-code token))
    (declare (type (integer 0 10) command-code))
    (declare (type decimal-digit  number-of-repetitions))
    (the (values command-code decimal-digit)
      (if (<= 1 command-code 8)
        (values command-code number-of-repetitions)
        (error "Invalid command code: ~d." command-code)))))

;;; -------------------------------------------------------

(defun decode-ASCIIfuck-command-code (command-code)
  "Returns for the ASCIIfuck COMMAND-CODE the corresponding brainfuck
   instruction."
  (declare (type command-code command-code))
  (the brainfuck-command
    (case command-code
      (1 :increment)
      (2 :decrement)
      (3 :move-right)
      (4 :move-left)
      (5 :input)
      (6 :output)
      (7 :jump-forward)
      (8 :jump-back)
      (otherwise
        (error "Invalid ASCIIfuck command code: ~s." command-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "ASCIIfuck-Decoder".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ASCIIfuck-Decoder ()
  ()
  (:documentation
    "The ``ASCIIfuck-Decoder'' interface establishes the foundry for all
     classes in their pursuit of a ASCIIfuck program's consumption or
     transformation according with any intention."))

;;; -------------------------------------------------------

(defgeneric start-decoding (decoder)
  (:documentation
    "Prepares the ASCIIfuck DECODER for a contingent sequence of
     decoding behests and returns no value."))

;;; -------------------------------------------------------

(defgeneric decode-ASCIIfuck-command (decoder
                                      command-code
                                      number-of-repetitions)
  (:documentation
    "Imposes upon the DECODER the consumption of the numeric ASCIIfuck
     COMMAND-CODE, repeated a NUMER-OF-REPETITIONS times, and returns no
     value."))

;;; -------------------------------------------------------

(defgeneric finish-decoding (decoder)
  (:documentation
    "Apprizes the ASCIIfuck DECODER about a decoding process' patration
     and returns a value covenable for this context.
     ---
     Please note that, succeeding from this operation's invocation, the
     DECODER's state, without a prevenient behest upon the
     ``start-decoding'' routine, is supputated as ineligible for 
     further manipulations by the ``decode-ASCIIfuck-command'' and
     ``finish-decoding'' applications. In a concrete diction, this
     operation may render the DECODER irrespondent for an iterum
     request until the aforementioned stipulation has been effected."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "ASCIIfuck-Parser".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ASCIIfuck-Parser (ASCIIfuck-Decoder)
  ((brainfuck-commands
    :initform      NIL
    :type          (list-of brainfuck-command)
    :documentation "Gathers the translated brainfuck commands."))
  (:documentation
    "The ``ASCIIfuck-Parser'' class realizes an ``ASCIIfuck-Decoder''
     which assumes the onus of compiling a sequence of brainfuck
     commands from a series of ASCIIfuck command-repetitions
     specifications."))

;;; -------------------------------------------------------

(defun make-ASCIIfuck-parser ()
  "Creates and returns a new ``ASCIIfuck-Parser''."
  (the ASCIIfuck-Parser
    (make-instance 'ASCIIfuck-Parser)))

;;; -------------------------------------------------------

(defmethod start-decoding ((decoder ASCIIfuck-Parser))
  (declare (type ASCIIfuck-Parser decoder))
  (setf (slot-value decoder 'brainfuck-commands) NIL)
  (values))

;;; -------------------------------------------------------

(defmethod decode-ASCIIfuck-command
    ((decoder               ASCIIfuck-Parser)
     (command-code          integer)
     (number-of-repetitions integer))
  (declare (type ASCIIfuck-Parser decoder))
  (declare (type command-code     command-code))
  (declare (type decimal-digit    number-of-repetitions))
  (let ((brainfuck-command
          (decode-ASCIIfuck-command-code command-code)))
    (declare (type brainfuck-command brainfuck-command))
    (loop repeat number-of-repetitions do
      (push brainfuck-command
        (slot-value decoder 'brainfuck-commands))))
  (values))

;;; -------------------------------------------------------

(defmethod finish-decoding ((decoder ASCIIfuck-Parser))
  (declare (type ASCIIfuck-Parser decoder))
  (the brainfuck-program
    (coerce
      (nreverse
        (slot-value decoder 'brainfuck-commands))
      '(simple-array brainfuck-command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "ASCIIfuck-Translator".              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass ASCIIfuck-Translator (ASCIIfuck-Decoder)
  ((brainfuck-code
    :initform      (make-string-output-stream)
    :type          string-stream
    :documentation "Gradually builds the brainfuck source code."))
  (:documentation
    "The ``ASCIIfuck-Translator'' class realizes an
     ``ASCIIfuck-Decoder'' which is assigned the dever of an ASCIIfuck
     program's translation into a piece of brainfuck source code."))

;;; -------------------------------------------------------

(defun make-ASCIIfuck-translator ()
  "Creates and returns a new ``ASCIIfuck-Translator''."
  (the ASCIIfuck-Translator
    (make-instance 'ASCIIfuck-Translator)))

;;; -------------------------------------------------------

(defmethod start-decoding ((decoder ASCIIfuck-Translator))
  (declare (type ASCIIfuck-Translator decoder))
  (setf (slot-value decoder 'brainfuck-code)
    (make-string-output-stream))
  (values))

;;; -------------------------------------------------------

(defmethod decode-ASCIIfuck-command
    ((decoder               ASCIIfuck-Translator)
     (command-code          integer)
     (number-of-repetitions integer))
  (declare (type ASCIIfuck-Translator decoder))
  (declare (type command-code     command-code))
  (declare (type decimal-digit    number-of-repetitions))
  (let ((brainfuck-command
          (decode-ASCIIfuck-command-code command-code)))
    (declare (type brainfuck-command brainfuck-command))
    (format
      (slot-value decoder 'brainfuck-code)
      "~v@{~c~:*~}"
      number-of-repetitions
      (case brainfuck-command
        (:increment    #\+)
        (:decrement    #\-)
        (:move-right   #\>)
        (:move-left    #\<)
        (:input        #\,)
        (:output       #\.)
        (:jump-forward #\[)
        (:jump-back    #\])
        (otherwise
          (error "Invalid brainfuck command: ~s." brainfuck-command)))))
  (values))

;;; -------------------------------------------------------

(defmethod finish-decoding ((decoder ASCIIfuck-Translator))
  (declare (type ASCIIfuck-Translator decoder))
  (the string
    (get-output-stream-string
      (slot-value decoder 'brainfuck-code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of ASCIIfuck decoder.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun decode-ASCIIfuck-code (asciifuck-code decoder)
  "Decodes the piece of ASCIIFUCK-CODE in the DECODER's context and
   returns its decoding result, obtained via the ``finish-decoding''
   operation."
  (declare (type string            asciifuck-code))
  (declare (type ASCIIfuck-Decoder decoder))
  (flet ((process-asciifuck-token (command-code number-of-repetitions)
          "Decodes the COMMAND-CODE, repeated the NUMBER-OF-REPETITIONS
           tally of times, and returns no value."
          (declare (type command-code  command-code))
          (declare (type decimal-digit number-of-repetitions))
          (decode-ASCIIfuck-command
            decoder
            command-code
            number-of-repetitions)
          (values)))
    (the T
      (loop
        initially
          (start-decoding decoder)
        
        for asciifuck-token
          of-type character
          across  asciifuck-code
        
        do
          (multiple-value-call #'process-asciifuck-token
            (decode-character asciifuck-token))
        
        finally
          (return
            (finish-decoding decoder))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-jump-table ()
  "Creates and returns an empty ``jump-table''."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Associates the START-POINT and the END-POINT in the JUMP-TABLE in a
   bidirectional manner and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (setf (gethash start-point jump-table) end-point)
  (setf (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun get-jump-target (jump-table source-point)
  "Returns the destination for the SOURCE-POINT in the JUMP-TABLE, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     source-point))
  (the fixnum
    (or (gethash source-point jump-table)
        (error "No end point exists for the jump point ~d."
          source-point))))

;;; -------------------------------------------------------

(defun compute-jump-table (brainfuck-program)
  "Creates and returns for the BRAINFUCK-PROGRAM a jump table which
   associates the forward jump points' location with that of the back
   jump positions, and vice versa."
  (declare (type brainfuck-program brainfuck-program))
  
  (let ((jump-table          (make-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type brainfuck-command across brainfuck-program
      for position of-type fixnum            from   0 by 1
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (connect-jump-points jump-table
            (pop forward-jump-points)
            position)
          (error "Unmatched back jump point at position ~d." position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer octet)
    :documentation "A sparse vector represented by a hash table, the
                    keys of which impose the cell indices, while the
                    values maintain the cell objects.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, responsible for the current cell's
                    designation by referring to its index, a key, in the
                    CELLS hash table."))
  (:documentation
    "The ``Memory'' class models the ASCIIfuck program memory as a
     bilaterally infinite tape of unsigned byte-valued cells, the
     currently active member among which is designated by a mobile
     cell pointer."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a new ``Memory''."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the octet
    (gethash
      (slot-value memory 'pointer)
      (slot-value memory 'cells)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceding this transfer by a wrapping of the input in the valid
   unsigned byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (slot-value memory 'pointer)
      (slot-value memory 'cells)
      0)
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun cell-pointer-position (memory)
  "Returns the MEMORY cell pointer's position."
  (declare (type Memory memory))
  (the integer
    (slot-value memory 'pointer)))

;;; -------------------------------------------------------

(defun (setf cell-pointer-position) (new-position memory)
  "Relocates the MEMORY's cell pointer to the NEW-POSITION and returns
   no value."
  (declare (type integer new-position))
  (declare (type Memory  memory))
  (setf (slot-value memory 'pointer) new-position)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-brainfuck-program (brainfuck-program)
  "Processes the BRAINFUCK-PROGRAM and returns no value."
  (declare (type brainfuck-program brainfuck-program))
  (let ((ip         0)
        (jump-table (compute-jump-table brainfuck-program))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length brainfuck-program)) do
      (let ((current-command (aref brainfuck-program ip)))
        (declare (type brainfuck-command current-command))
        (case current-command
          (:increment
            (incf (current-cell-value memory)))
          (:decrement
            (decf (current-cell-value memory)))
          (:move-right
            (incf (cell-pointer-position memory)))
          (:move-left
            (decf (cell-pointer-position memory)))
          (:input
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value memory)
              (char-code
                (read-char)))
            (clear-input))
          (:output
            (write-char
              (code-char
                (current-cell-value memory))))
          (:jump-forward
            (when (zerop (current-cell-value memory))
              (setf ip
                (get-jump-target jump-table ip))))
          (:jump-back
            (unless (zerop (current-cell-value memory))
              (setf ip
                (get-jump-target jump-table ip))))
          (otherwise
            (error "Unrecognized command ~s at position ~d."
              current-command ip))))
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun interpret-ASCIIfuck (asciifuck-code)
  "Interprets the piece of ASCIIFUCK-CODE and returns no value."
  (declare (type string asciifuck-code))
  (process-brainfuck-program
    (decode-ASCIIfuck-code asciifuck-code
      (make-ASCIIfuck-parser)))
  (values))

;;; -------------------------------------------------------

(defun translate-ASCIIfuck-to-brainfuck (asciifuck-code
                                         &optional (destination NIL))
  "Converts the ASCIIFUCK-CODE into a tantamount equivalent brainfuck,
   writes the resulting brainfuck code to the DESTINATION, and returns
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending the output."
  (declare (type string      asciifuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (format destination "~a"
        (decode-ASCIIfuck-code asciifuck-code
          (make-ASCIIfuck-translator)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-ASCIIfuck-to-brainfuck asciifuck-code output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a user input of the
;; "NULL character".
(interpret-ASCIIfuck "3G=3Q")

;;; -------------------------------------------------------

;; Print "Hello World!".
;; This program is tantamount to the brainfuck code
;;   ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
(interpret-ASCIIfuck "lGyhGyfygygye,oQyeyeyo eG)Q)oQ =yq=k>g= =)o=)=g=t=v= e=yf=")

;;; -------------------------------------------------------

;; Convert the repeating cat program into a piece of brainfuck code,
;; which amounts to:
;;   ,[.,]
(translate-ASCIIfuck-to-brainfuck "3G=3Q")
