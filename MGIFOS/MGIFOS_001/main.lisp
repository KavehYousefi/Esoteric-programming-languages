;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "MGIFOS", invented by the Esolang user "Marz",
;; as well as routines for the encoding and decoding bewixt MGIFOS and
;; "brainfuck", invented by Urban Mueller.
;; 
;; Concept
;; =======
;; MGIFOS constitutes an equivalent of the esoteric programming
;; language brainfuck, the single-character instructions of which have
;; been substituted by bit sequences composed of exactly four bits. Any
;; program in this language consists of asterisks ("*") only, with their
;; tally determining the semantics.
;; 
;; == THE NAME "MGIFOS" ==
;; The name "MGIFOS" constitutes an acronym of the phrase "My God It's
;; Full of Stars", an allusion to the only effective token in the
;; language, the asterisk or star character "*".
;; 
;; == THE TALLY OF ASTERISKS BINARY-ENCODES THE COMMANDS ==
;; The problem of mapping the eight instructions from brainfuck, each
;; accommodated with its unique symbol, to the singular asterisk is
;; vanquished by aid of a binary encoding. The correlation betwixt a
;; sequence of asterisks and the commands amounts to the following:
;; 
;;   (1) Count the number of asterisks in the MGIFOS code and store
;;       this integer tally in "c":
;;         c <- number of asterisks in the program
;;   (2) Convert the integer number c into its binary form "cb":
;;         cb <- binary form of c
;;   (3) Split the binary sequence cb into groups of four bits
;;       (nybbles) "cg". Each such group represents one MGIFOS command
;;       in its binary form.
;;         cg <- split c into nybble groups
;;   (4) Iterate cg from the group at the most significant position "g1"
;;       to that of the least significant "gN" and execute the command
;;       represented in its binary form. Whether this means the
;;       interpretation of the code or its conversion into another form,
;;       for instance a brainfuck program, relies on one's private
;;       objectives.
;;         for group index i from 1 to cg.length
;;           process command represented by nybble g[i]
;;         end for
;; 
;; The quadruple of following sections elucidates the just presented
;; enumeration in greater detail.
;; 
;; == STEP 1: COUNT THE ASTERISKS ==
;; Being a process of multiple steps, incipiently the occurrences of
;; "*" in the MGIFOS source code are tallied, yielding an integer number
;; greater or equal to zero, but unbounded to the upper space. Any other
;; character, if present in the code, is neglected and hence does not
;; contribute to the result.
;; 
;; == STEP 2: CONVERT THE TALLY INTO BITS ==
;; As with any datum, an integer can be represented unambiguously by a
;; series of one or more bits. This bit sequence contains the MGIFOS
;; commands in their binary form.
;; 
;; == STEP 3: SPLIT THE BINARY SEQUENCE INTO NYBBLES ==
;; Each command in MGIFOS is represented in its binary form as a group
;; of four adjacent bits, also known as a nybble. The bit sequence of
;; the asterisk count must thus be split into four bit groups, or
;; nybbles, in order to extract its incorporated instructions. Given a
;; bit "cb" representation of the integer tally "c", the number of
;; groups "cg.length" admits calculation by aide of the following
;; formula:
;;   cg.length = ceiling (cb.length / 4)
;; 
;; == STEP 4: PROCESS THE GROUPS/COMMANDS ==
;; Starting from the nybble at the most significant position and
;; proceeding towards that at the least significant, process each such
;; binary-encoded command in accordance with the designated purpose.
;; If intent on executing the program, MGIFOS, being an equivalent of
;; brainfuck, can be interpreted in the fashion of its ancestry.
;; 
;; 
;; Conversions
;; ===========
;; MGIFOS being one among many produces of brainfuck's afflatus partakes
;; of the amenity commorant as a coattribute in derivates: the
;; contingence of transcription to brainfuck and vice versa. The
;; section below will elucidate the procedure in both directions. Please
;; note that the basic gnarity involving MGIFOS's instruction set in
;; its binary representation, as adduced in the "Concepts" section,
;; assumes the role of a prerequisite to further comprehension.
;; 
;; == FROM MGIFOS TO BRAINFUCK ==
;; Given an MGIFOS source code, its commands in their binary form must
;; be extracted, ere each such is transliterated into its brainfuck
;; equivalent and gathered in a sink of own's desideration. In concrete
;; terms, the following ought to be applied:
;; 
;;   (1) Count the number of asterisks in the MGIFOS code and store
;;       this integer tally in "c":
;;         c <- number of asterisks in the program
;;   (2) Convert the integer number c into its binary form "cb":
;;         cb <- binary form of c
;;   (3) Split the binary sequence cb into groups of four bits
;;       (nybbles) "cg". Each such group represents one MGIFOS command
;;       in its binary form.
;;         cg <- split c into nybble groups
;;   (4) Iterate cg from the group at the most significant position "g1"
;;       to that of the least significant "gN", query the analogous
;;       brainfuck instruction to the MGIFOS nybble, and write it to the
;;       destination brainfuck code "brainfuckCode".
;;         for group index i from 1 to ceiling (cg.length / 4)
;;           let brainfuckCommand <- brainfuckCodeFor (g_[i])
;;           write brainfuckCommand to brainfuckCode
;;         end for
;; 
;; During the translation from MGIFOS to brainfuck, the abstract
;; function "brainfuckCodeFor" returns for an MGIFOS instruction defined
;; by four bits a brainfuck character. The following associations hold:
;; 
;;   MGIFOS binary instruction | brainfuck instruction
;;   --------------------------+----------------------
;;    0001                     | >
;;   .................................................
;;    0010                     | <
;;   .................................................
;;    0011                     | +
;;   .................................................
;;    0100                     | -
;;   .................................................
;;    0101                     | .
;;   .................................................
;;    0110                     | ,
;;   .................................................
;;    0111                     | [
;;   .................................................
;;    1000                     | ]
;; 
;; == FROM BRAINFUCK TO MGIFOS ==
;; Symmetry inhabits the principles of conversions between MGIFOS and
;; brainfuck; issuing therefrom, the obtention of the former from the
;; latter can be conceived per saltum. Natheless, the steps will be
;; subjected to disquisition.
;; 
;;   (1) Iterate each instruction "bfi" of the brainfuck source code and
;;       obtain the MGIFOS binary code "g" for it, each such
;;       constituting four bits, that is, one nybble. Concatenate the
;;       nybbles from the most significant position to the least into
;;       one bit pattern "cb":
;;         let cb <- 0
;;         for brainfuck instruction index i from 1 to brainfuckCode.length
;;           let bfi <- brainfuckCode[i]
;;           let g   <- mgifosBitsFor (bfi)
;;           insert the bits of g in the least significant position of cb
;;         end for
;;   (2) Convert the bit pattern "cb" into an integer number "c", this
;;       value being tantamount to the number of asterisks necessary to
;;       represent the MGIFOS code equivalent to the brainfuck source.
;;         c <- integer value represented by bit pattern cb
;;   (3) Write "c" number of asterisks to the MGIFOS result code.
;;         for repetition from 1 to c
;;           write "*" to mgifosResultCode
;;         end for
;; 
;; A mediator from the brainfuck realm to MGIFOS, the abstract function
;; "mgifosBitsFor" returns for a brainfuck instruction token the four
;; bits constituting an MGIFOS equivalent. The mapping is as follows:
;; 
;;   brainfuck instruction | MGIFOS binary instruction
;;   ----------------------+--------------------------
;;    >                    | 0001
;;   .................................................
;;    <                    | 0010
;;   .................................................
;;    +                    | 0011
;;   .................................................
;;    -                    | 0100
;;   .................................................
;;    .                    | 0101
;;   .................................................
;;    ,                    | 0110
;;   .................................................
;;    [                    | 0111
;;   .................................................
;;    ]                    | 1000
;; 
;; 
;; Architecture
;; ============
;; MGIFOS, being a variety of brainfuck, establishes its foundation upon
;; brainfuck's model, chosen to be in concord with the most liberal
;; construe.
;; 
;; == THE SALVATORY: AN INFINITE TAPE ==
;; The data salvatory's instantiation manifests in the form of an
;; infinite sequence of cells, similar to a tape, linearly arranged and
;; bilaterally unbounded. Each cell stores an arbitrary integer number
;; of any size and sign. Operations for incrementing and decrementing
;; a cell's content exist.
;; 
;; == THE POINTER: A SELECTOR FOR THE ACTIVE CELL ==
;; At every instant, a single cell is designated as the active one. The
;; cursor into the same is nevened the pointer. Instructions exist to
;; enable its navigation to the left and right.
;; 
;; 
;; Data Types
;; ==========
;; MGIFOS is based upon the manipulation of unbounded integers stored
;; in the memory cells, and communication with the user in the form of
;; characters obtained by interpreting the former's valid range
;; according to the ASCII repertoire.
;; 
;; == INTEGERS CONSTITUTE THE COMPUTATIONAL FOUNDATION ==
;; All kind of data is represented by unbounded integer numbers in the
;; range [-infinity, +infinity] and stored in the memory. This
;; repository itself consists in cells enumerated by the same type. The
;; sparse instructions granted as avails operate upon this
;; representation only.
;; 
;; == CHARACTERS ARE EMPLOYED ON THE INTERFACES ==
;; The conduit betwixt the user and the program, encompassing both input
;; and output, relies on character objects, which are internally
;; representend by their numeric code in the memory. A user input is
;; converted into its character code to be persisted; likewise, if an
;; output is requested, the current cell's integer value is translated
;; into the associated ASCII character and transmitted to the display.
;; 
;; == MEMORY ==
;; The data management is implemented in the memory, composed of
;; contiguously aligned cells, each storing an arbitrary integer number.
;; 
;; 
;; Syntax
;; ======
;; A kenspeckle aspect of the syntax resides in its nimious homogeneity:
;; The only significant token in a program manifests in the asterisk
;; "*". Whitespaces are homologated as negligible constituents, while
;; other characters infringe the language's sanity and eventuate errors.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) description applies:
;; 
;;   program    := { whitespace } , "*" , { whitespace } ;
;;   whitespace := " " | "\lf" | "\n" | "\r" | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; MGIFOS's instruction set constitutes a reformulation of brainfuck's
;; in the form of a four-bit sequences.
;; 
;; == OVERVIEW ==
;; The following table lists all eight instructions with a juxtaposition
;; of their effect.
;; 
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;    0001   | Moves the cell pointer one step to the right.
;;   ..................................................................
;;    0010   | Moves the cell pointer one step to the left.
;;   ..................................................................
;;    0011   | Increments the value of the active cell by one.
;;   ..................................................................
;;    0100   | Decrements the value of the active cell by one.
;;   ..................................................................
;;    0101   | Outputs the ASCII character associated with the numeric
;;           | value of the active cell.
;;   ..................................................................
;;    0110   | Queries the user for a character and stores its ASCII
;;           | code in the active cell.
;;   ..................................................................
;;    0111   | If the active cell value equals zero (0), moves past the
;;           | "1000" command at the matching level; otherwise proceeds
;;           | as usual.
;;   ..................................................................
;;    1000   | If the active cell does not equal zero (0), moves back
;;           | to the command immediately after the "0001" at the
;;           | matching level; otherwise proceeds as usual.
;; 
;; 
;; Implementation
;; ==============
;; The implementation at hand ostends a very simple design with a focus
;; on instructive value, the major components of which shall be subjects
;; to further disquisitions.
;; 
;; == THE INFINITE TAPE: A HASH TABLE ==
;; The tape's infinite expansion eludes the capacities of a vector in
;; Common Lisp; whence issues the necessity of an alternative data
;; repository with no restriction on the size and concomitant indexed
;; access. The ``hash-table'' class redes itself as a natural choice for
;; this task, assigning to each integer key a value of the same type
;; type, both without further qualifications in their design. Compatible
;; with the key datum, a separate integer value finds employment in the
;; agency of a pointer.
;; 
;; == THE INTERPRETER PREPARES AN MGIFOS COMMAND VECTOR ==
;; While being nevened an interpreter, attributes of a compiler imbue
;; this implementation, as an input MGIFOS program is incipiently
;; transformed into a vector of its binary command tokens, that is, a
;; collection into nybbles. Two benefits are yielded by this
;; materialization:
;; 
;;   (1) Superfluous characters in the code string are removed.
;;   (2) Navigation through the commands is facilitated.
;; 
;; The realization of this structure adheres to the command nybble
;; extraction process elucidated in the "Concepts" section, which please
;; see. Most of these operations depend, immediately or by mediation,
;; upon the preprocessed MGIFOS command vector manifestation.
;; 
;; == CONVERSION FROM MGIFOS TO BRAINFUCK ==
;; The implementation encompasses a converter from MGIFOS to brainfuck,
;; with the former's source either being in string or binary form:
;; 
;;   convert-bits-to-brainfuck
;;     Converts a binary sequence, which represents the tally of
;;     asterisks composing an MGIFOS program, to brainfuck code.
;;   
;;   convert-MGIFOS-to-brainfuck
;;     Converts a piece of MGIFOS code to brainfuck.
;; 
;; == CONVERSION FROM BRAINFUCK TO MGIFOS ==
;; Two functions exist for the purpose of converting brainfuck code into
;; MGIFOS, either in the latter's binary or string variant:
;; 
;;   convert-brainfuck-to-bits
;;     Converts a brainfuck program into the binary representation of
;;     the equivalent MGIFOS code, which is tantamount to the tally of
;;     asterisks contained in the same.
;; 
;;   convert-brainfuck-to-MGIFOS
;;     Converts a brainfuck program into the equivalent MGIFOS code
;;     that constitutes a string of asterisks.
;; 
;; == INTERPRETER ==
;; In addition to the conversion routines, an interpreter capable of
;; executing MGIFOS programs in string and binary form is supplied:
;; 
;;   interpret-MGIFOS-binary
;;     Interprets the MGIFOS code represented in its binary form, which
;;     concomitantly equals the number of contained asterisks.
;;   
;;   interpret-MGIFOS-code
;;     Interprets the MGIFOS code provided as a string of asterisks.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-11
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/MGIFOS"
;;   -> "https://esolangs.org/wiki/Brainfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype nybble ()
  "The ``nybble'' type defines a sequence of four adjacent bits,
   encoded in an ``integer'' datum."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype alist-of (&optional (key-type T) (value-type T))
  "The ``alist-of'' type defines an association list (alist) of zero or
   more entries, each of these represented by a cons composed of a left
   element of the KEY-TYPE and a right element of the VALUE-TYPE, with
   both constituents resolving to the default ``T'' type."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (and
                    (consp element)
                    (typep (car element) key-type)
                    (typep (cdr element) value-type)))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, the keys of which conform to the KEY-TYPE and the values to
   the VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a data sink compatible with
   Common Lisp's output operations, including ``format'' and
   ``write-char'', among others."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conversion table.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (alist-of nybble character) +CONVERSION-TABLE+))

;;; -------------------------------------------------------

(defparameter +CONVERSION-TABLE+
  '((#b0001 . #\>)
    (#b0010 . #\<)
    (#b0011 . #\+)
    (#b0100 . #\-)
    (#b0101 . #\.)
    (#b0110 . #\,)
    (#b0111 . #\[)
    (#b1000 . #\]))
  "The conversion table maps each MGIFOS command in its binary
   representation to a brainfuck command character.")

;;; -------------------------------------------------------

(defun get-mgifos-command-for (brainfuck-command)
  "Returns the MGIFOS command associated with the BRAINFUCK-COMMAND,
   or ``NIL'' if no association could be detected."
  (declare (type character brainfuck-command))
  (the (or null nybble)
    (car (rassoc brainfuck-command +CONVERSION-TABLE+ :test #'char=))))

;;; -------------------------------------------------------

(defun get-brainfuck-command-for (mgifos-command)
  "Returns the brainfuck command associated with the MGIFOS-COMMAND,
   or ``NIL'' if no association could be detected."
  (declare (type nybble mgifos-command))
  (the (or null character)
    (cdr (assoc mgifos-command +CONVERSION-TABLE+ :test #'=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 0 4) +BITS-PER-COMMAND+))

;;; -------------------------------------------------------

(defparameter +BITS-PER-COMMAND+ 4
  "The number of bits per binary representation of a MGIFOS command.
   ---
   As each command in this language is represented by a nybble, the
   size amounts to four (4).")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of MGIFOS-to-brainfucker encoder.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-stars (code)
  "Returns the number of asterisks ('*') or stars in the CODE."
  (declare (type string code))
  (the (integer 0 *) (count #\* code :test #'char=)))

;;; -------------------------------------------------------

(defun extract-MGIFOS-binary-commands (bits)
  "Returns a vector of the MGIFOS commands extracted from the
   integer-encoded bits, each such command being represented as a
   binary nybble."
  (declare (type unsigned-byte bits))
  (the (vector nybble)
    (loop
      for byte-position
        of-type (integer 0 *)
        from    0
        below   (integer-length bits)
        by      +BITS-PER-COMMAND+
      collect
        (ldb (byte +BITS-PER-COMMAND+ byte-position) bits)
      into
        tokens
      finally
        (return (coerce (nreverse tokens) '(vector nybble))))))

;;; -------------------------------------------------------

(defun convert-bits-to-brainfuck (number-of-stars
                                  &optional (destination T))
  "Converts the MGIFOS code, represented by the NUMBER-OF-STARS
   contained in it --- or, equivalently, the commands in their binary
   representation --- to a brainfuck program which is written to the
   DESTINATION."
  (declare (type (integer 0 *) number-of-stars))
  (declare (type destination   destination))
  (let ((binary-commands (extract-MGIFOS-binary-commands number-of-stars)))
    (declare (type (vector nybble) binary-commands))
    (if destination
      (loop
        for mgifos-command
          of-type nybble
          across  binary-commands
        do
          (format destination "~a"
            (or (get-brainfuck-command-for mgifos-command)
                (error "Invalid MGIFOS command: ~s." mgifos-command))))
      (the string
        (with-output-to-string (output)
          (declare (type string-stream output))
          (convert-bits-to-brainfuck number-of-stars output))))))

;;; -------------------------------------------------------

(defun convert-MGIFOS-to-brainfuck (mgifos-code
                                    &optional (destination T))
  "Converts the MGIFOS-CODE composed of asterisks into the equivalent
   brainfuck program which is written to the DESTINATION."
  (declare (type string      mgifos-code))
  (declare (type destination destination))
  (the string
    (convert-bits-to-brainfuck (count-stars mgifos-code) destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-MGIFOS decoder.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-bits (brainfuck-code)
  "Converts the BRAINFUCK-CODE to the binary representation of the
   equivalent MGIFOS program, which is tantamount to the tally of
   asterisks it comprehends, and returns these integer-encoded bits."
  (declare (type string brainfuck-code))
  (let ((mgifos-bits 0))
    (declare (type unsigned-byte mgifos-bits))
    (loop for character of-type character across brainfuck-code do
      (let ((mgifos-command (get-mgifos-command-for character)))
        (declare (type (or null nybble) mgifos-command))
        (when mgifos-command
          (setf mgifos-bits (ash mgifos-bits +BITS-PER-COMMAND+))
          (setf (ldb (byte +BITS-PER-COMMAND+ 0) mgifos-bits)
                mgifos-command))))
    (the unsigned-byte mgifos-bits)))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-MGIFOS (brainfuck-code
                                    &optional (destination T))
  "Converts the BRAINFUCK-CODE to the equivalent MGIFOS code, composed
   of asterisks only, and writes it the DESTINATION."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (let ((number-of-stars (convert-brainfuck-to-bits brainfuck-code)))
    (declare (type (integer 0 *) number-of-stars))
    (if destination
      (loop repeat number-of-stars do
        (write-char #\* destination))
      (the string
        (with-output-to-string (output)
          (declare (type string-stream output))
          (convert-brainfuck-to-MGIFOS brainfuck-code output))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of MGIFOS interpreter.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-MGIFOS-commands (commands)
  "Interprets the MGIFOS program supplied in the form of a vector of
   COMMANDS, each such in its binary nybble form, and returns no value."
  (declare (type (vector nybble) commands))
  
  (let ((position 0)
        (command  (aref commands 0)))
    (declare (type fixnum           position))
    (declare (type (or null nybble) command))
    
    (let ((memory  (make-hash-table :test #'eql))
          (pointer 0))
      (declare (type (hash-table-of integer integer) memory))
      (declare (type integer                         pointer))
      
      (flet
          ((advance ()
            "Moves the POSITION one command forward, if possible, and
             updates the current COMMAND."
            (if (< position (1- (length commands)))
              (setf command (aref commands (incf position)))
              (setf command NIL))
            (values))
           
           (recede ()
            "Moves the POSITION one command back, if possible, and
             updates the current COMMAND."
            (if (plusp position)
              (setf command (aref commands (decf position)))
              (setf command NIL))
            (values)))
        
        (loop do
          (case command
            ;; No more commands remaining.
            ((NIL)
              (loop-finish))
            
            ;; Move pointer right.
            (#b0001
              (incf pointer)
              (advance))
            
            ;; Move pointer left.
            (#b0010
              (decf pointer)
              (advance))
            
            ;; Increment the memory under the pointer.
            (#b0011
              (incf (gethash pointer memory 0))
              (advance))
            
            ;; Decrement the memory under the pointer.
            (#b0100
              (decf (gethash pointer memory 0))
              (advance))
            
            ;; Output the current cell value.
            (#b0101
              (write-char (code-char (gethash pointer memory 0)))
              (advance))
            
            ;; Input a character and its code in the current cell.
            (#b0110
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type (or null character) input))
                (clear-input)
                (setf (gethash pointer memory) (char-code input)))
              (advance))
            
            ;; Jump past matching "1000" if the current cell value
            ;; equals 0.
            (#b0111
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched '0111'."))
                      (#b1000
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (#b0111
                        (incf level)
                        (advance))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back after "0111" if the current cell value does
            ;; not equal 0.
            (#b1000
              (cond
                ((not (zerop (gethash pointer memory 0)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched '1000'."))
                      (#b0111
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#b1000
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            (otherwise
              (error "Invalid command '~b' at index ~d."
                command position)))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-MGIFOS-binary (bits)
  "Interprets the MGIFOS code supplied in the integer-encoded BITS,
   which concomitantly equals the number of stars in the equivalent
   plaintext code, and returns no value."
  (declare (type unsigned-byte bits))
  (interpret-MGIFOS-commands
    (extract-MGIFOS-binary-commands bits))
  (values))

;;; -------------------------------------------------------

(defun interpret-MGIFOS-code (code)
  "Interprets the MGIFOS CODE supplied as a string and returns no value."
  (declare (type string code))
  (interpret-MGIFOS-commands
    (extract-MGIFOS-binary-commands
      (count-stars code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert the "Hello, World!" program from brainfuck to MGIFOS and
;; interpret it.
(interpret-MGIFOS-binary
  (convert-brainfuck-to-bits "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))

;;; -------------------------------------------------------

;; Convert the cat program from brainfuck to MGIFOS and interpret it.
(interpret-MGIFOS-commands
  (extract-MGIFOS-binary-commands
    (convert-brainfuck-to-bits ",[.,]")))

;;; -------------------------------------------------------

;; The infinite cat program specified by the number of asterisks ("*")
;; and executed.
(interpret-MGIFOS-binary 423272)

;;; -------------------------------------------------------

;; Increase the first cell by two and print it. This constitutes the
;; equivalent of the brainfuck program "++.".
(interpret-MGIFOS-code "*****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************")

;;; -------------------------------------------------------

;; Convert the brainfuck program "++." into MGIFOS and interpret it.
(interpret-MGIFOS-commands
  (extract-MGIFOS-binary-commands
    (convert-brainfuck-to-bits "++.")))
