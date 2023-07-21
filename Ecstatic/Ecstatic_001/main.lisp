;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Ecstatic", invented by the Esolang user "JWinslow23" and
;; presented in the year 2013, as well as routines for encoding and
;; decoding betwixt this language and "brainfuck" by Urban Mueller.
;; 
;; 
;; Concept
;; =======
;; Ecstatic constitutes an equivalent of the esoteric programming
;; language brainfuck, the single-character instructions of which have
;; been substituted by bit sequences composed of exactly three bits. Any
;; program in this language consists of exclamation marks ("!") only,
;; with their tally determining the semantics.
;; 
;; == ENCODING ==
;; The encoding process of brainfuck code comprises two steps:
;;   (1) Creating a bit sequence from the brainfuck code and converting
;;       it into an unsigned integer number "n".
;;   (2) Creating a series of "n" exclamation marks "!", which already
;;       represents the Ecstatic code.
;; 
;; The first moeity of encoding is performed by substituting each of the
;; eight brainfuck instructions by a predefined three-bit sequence and
;; inserting these bits at the least significant position of the output
;; bit series. The following table describes the encoding adhibited to
;; the brainfuck instruction:
;; 
;;   ---------------------------------------------
;;   brainfuck instruction | Binary representation
;;   ----------------------+----------------------
;;   +                     | 000
;;   .............................................
;;   -                     | 001
;;   .............................................
;;   >                     | 010
;;   .............................................
;;   <                     | 011
;;   .............................................
;;   .                     | 100
;;   .............................................
;;   ,                     | 101
;;   .............................................
;;   [                     | 110
;;   .............................................
;;   ]                     | 111
;;   ---------------------------------------------
;; 
;; With all instructions having been encoded, a final bit group "001"
;; is inserted at the most significant position of the total bits. The
;; completed bit sequence is then interpreted as an unsigned integer
;; number.
;; 
;; In the second part of the encoding, the obtained integer number is
;; utilized as a tally: By producing the exclamation mark "!" this
;; number of times, the Ecstatic code equivalent to the brainfuck
;; program is generated.
;; 
;; Note that brainfuck tacitly ignores any character not defined as an
;; instruction, an expression of tolerance that is usually appropriated
;; to state commands in the source code. The actions of the encoding
;; perforce extinguish these comments, as the respective content is
;; simply skipped.
;; 
;; == DECODING ==
;; Being a symmetrical procedure, the decoding process inverts the
;; principles of the encoding:
;;   (1) Given a piece of Ecstatic source code under scrutinity, the
;;       number of exclamation marks ("!") is tallied and this unsigned
;;       integer value is converted into its binary representation.
;;   (2) Each three bits, omitting the sentinel portion in the three
;;       most significant positions, from the most to the least
;;       significant position, is translated into one brainfuck
;;       instruction and concatenated into the decoded brainfuck code.
;; 
;; The decoding process in its incipiency tallies the only character
;; permissive to a piece of Ecstatic source code, the exclamation mark
;; ("!"). The thus produced unsigned integer value is converted into its
;; binary representation, and must be cleared of the sentinel portion
;; "001", constituting the three most significant bits. Note that its
;; existence and location can be indagated in the agency of a check
;; value for the discovery of invalid or corrupted Ecstatic code. The
;; sentinel bits must be retrievable at the topmost bits, with the "1"
;; placed at the bit position
;;   1 + (n * 3)
;; for an n >= 1, where the bits are enumerated from zero. Failure to
;; satisfy this condition can be adduced as an evidence of faulty code.
;; 
;; Discencumbered from the sentinel portion, the bit sequence must then
;; be processed: Any three consecutive bits form an encoded brainfuck
;; command, arranged from the most significant position toward the least
;; significant. When decoding such a triplet, the above encoder table
;; is perused anew, the columns now interchanged, whence one gains:
;; 
;;   --------------------------------------
;;   Binary pattern | brainfuck instruction
;;   ---------------+----------------------
;;   000            | +
;;   ......................................
;;   001            | -
;;   ......................................
;;   010            | >
;;   ......................................
;;   011            | <
;;   ......................................
;;   100            | .
;;   ......................................
;;   101            | ,
;;   ......................................
;;   110            | [
;;   ......................................
;;   111            | ]
;;   --------------------------------------
;; 
;; By transferring the brainfuck instructions into a common sink, the
;; decoded program is produced.
;; 
;; 
;; Implementation
;; ==============
;; Naturally, Common Lisp programs encode binary sequences as simple
;; integer values, mostly but not mandatorily unsigned. The unbound
;; magnitude of this data type, as opposed to the finite and ``fixnum''
;; restricted capacity of bit vectors, renders the rational behind the
;; choice of bit conveyance in this form fairly patent.
;; 
;; Five principal functions have been defined in order to realize the
;; conversion from brainfuck to Ecstatic and vice versa:
;;   - convert-brainfuck-to-binary
;;       Encodes a piece of brainfuck code as a bit sequence according
;;       to the instruction conversion table, represented by an unsigned
;;       integer number.
;;   - convert-brainfuck-to-ecstatic
;;       Converts a piece of brainfuck code to Ecstatic source code.
;;   - convert-binary-to-brainfuck
;;       Decodes an unsigned integer value, whose bits contain the
;;       binary equivalents of brainfuck instructions, into a piece of
;;       brainfuck code.
;;   - convert-ecstatic-to-binary
;;       Converts a piece of Ecstatic code to a bit sequence.
;;   - convert-ecstatic-to-brainfuck
;;       Converts a piece of Ecstatic code to brainfuck source code.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-09
;; 
;; Sources:
;;   [esolang2020Ecstatic]
;;   The Esolang contributors, "Ecstatic", 2020
;;   URL: "https://esolangs.org/wiki/Ecstatic"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; -- Implementation of brainfuck encoder.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-binary (brainfuck-code)
  "Returns an integer-encoded binary representation of the
   BRAINFUCK-CODE according to the Ecstatic mapping of instructions to
   bit sequences.
   ---
   Please note that, given the sole capability of encoding brainfuck
   instructions, any characters not contributing to this set --- usually
   employed as comments --- are tacitly disregarded."
  (declare (type string brainfuck-code))
  (let ((binary          0)
        (binary-position 0))
    (declare (type (unsigned-byte *)   binary))
    (declare (type (integer       0 *) binary-position))
    (flet ((append-bits (bits)
             "Inserts in the least significant (LSB) position of the
              BINARY number the three BITS and returns no value."
             (declare (type (unsigned-byte 3) bits))
             (setf binary (ash binary 3))
             (setf (ldb (byte 3 0) binary) bits)
             (incf binary-position 3)
             (values))
           (prepend-bits (bits)
             "Inserts in the most significant (MSB) position of the
              BINARY number the three BITS and returns no value."
             (declare (type (unsigned-byte 3) bits))
             (setf (ldb (byte 3 binary-position) binary) bits)
             (incf binary-position 3)
             (values)))
      (loop for character of-type character across brainfuck-code do
        (case character
          (#\+ (append-bits #b000))
          (#\- (append-bits #b001))
          (#\> (append-bits #b010))
          (#\< (append-bits #b011))
          (#\. (append-bits #b100))
          (#\, (append-bits #b101))
          (#\[ (append-bits #b110))
          (#\] (append-bits #b111))
          (T   NIL))
        finally
          (prepend-bits #b001)))
    (the (unsigned-byte *) binary)))

;;; -------------------------------------------------------

(defun write-exclamation-points (tally-of-exclamations
                                 &optional (destination T))
  "Writes a count of TALLY-OF-EXCLAMATIONS exclamation marks ('!') to
   the destination, the latter of which defaults to the standard output,
   and returns the corresponding value of a ``format'' invocation with
   the same."
  (declare (type (integer 0 *) tally-of-exclamations))
  (declare (type destination   destination))
  (if destination
    (loop repeat tally-of-exclamations do
      (write-char #\! destination))
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (write-exclamation-points tally-of-exclamations output)))))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-ecstatic (brainfuck-code
                                      &optional (destination T))
  "Converts the BRAINFUCK-CODE into Ecstatic code and writes it to the
   DESTINATION, which defaults to the standard output.
   ---
   Please note that, given the sole capability of encoding brainfuck
   instructions, any characters not contributing to this set --- usually
   employed as comments --- are tacitly disregarded."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (write-exclamation-points
    (convert-brainfuck-to-binary brainfuck-code)
    destination))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of binary decoder.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-binary-to-brainfuck (binary &optional (destination T))
  "Converts the integer-encoded bit sequence BINARY into brainfuck
   instructions and writes these to the DESTINATION, which defaults to
   the standard output.
   ---
   Please note that the encoded brainfuck code must stringently conform
   to the Ecstatic coding scheme; this means that (1) it must consist of
   zero or three-bit groups, and (2) contain the sentinel bit sequence
   '001' at its most significant position."
  (declare (type (unsigned-byte *) binary))
  (declare (type destination       destination))
  (if destination
    (let ((binary-length (integer-length binary)))
      (declare (type (integer 0 *) binary-length))
      (cond
        ((zerop binary-length)
          (error "Empty binary sequence: '~b'." binary))
        ;; The sentinel prefix "001" bits must be prepended to zero or
        ;; more groups of three bits, thus exhibiting a length of
        ;;   1 + (n * 3)
        ;; with n being the number of bit groups.
        ((not (zerop (mod (1- binary-length) 3)))
          (error "Invalid binary sequence: '~v,'0b'."
                 binary-length binary))
        (T
          (loop
            for
              bit-position
              of-type integer
              from    (- binary-length 4) downto 0 by 3
            for
              bit-group
              of-type (unsigned-byte 3)
              =       (ldb (byte 3 bit-position) binary)
            do
              (case bit-group
                (#b000 (write-char #\+ destination))
                (#b001 (write-char #\- destination))
                (#b010 (write-char #\> destination))
                (#b011 (write-char #\< destination))
                (#b100 (write-char #\. destination))
                (#b101 (write-char #\, destination))
                (#b110 (write-char #\[ destination))
                (#b111 (write-char #\] destination))
                (T     (error "Invalid bit group: '~3,'0b'."
                              bit-group)))))))
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-binary-to-brainfuck binary output)))))

;;; -------------------------------------------------------

(defun convert-ecstatic-to-binary (ecstatic-code)
  "Returns an integer-encoded binary representation of the
   ECSTATIC-CODE by tallying the number of exclamation marks ('!')."
  (declare (type string ecstatic-code))
  (the (unsigned-byte *) (count #\! ecstatic-code :test #'char=)))

;;; -------------------------------------------------------

(defun convert-ecstatic-to-brainfuck (ecstatic-code
                                      &optional (destination T))
  "Converts the ECSTATIC-CODE into brainfuck code and writes this code
   to the DESTINATION, which defaults to the standard output."
  (declare (type string      ecstatic-code))
  (declare (type destination destination))
  (convert-binary-to-brainfuck
    (convert-ecstatic-to-binary ecstatic-code)
    destination))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Ecstatic interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-Ecstatic-binary-commands (bits)
  "Returns a vector of the Ecstatic commands extracted from the
   integer-encoded bits, each such command being represented as a
   binary triplet."
  (declare (type unsigned-byte bits))
  (the (vector (unsigned-byte 3))
    (loop
      for byte-position
        of-type (integer 0 *)
        from    0
        below   (integer-length bits)
        by      3
      collect
        (ldb (byte 3 byte-position) bits)
      into
        tokens
      finally
        (return
          (coerce (nreverse (butlast tokens))
            '(vector (unsigned-byte 3)))))))

;;; -------------------------------------------------------

(defun interpret-Ecstatic-commands (commands)
  "Interprets the Ecstatic program supplied in the form of a vector of
   COMMANDS, each such in its binary form, and returns no value."
  (declare (type (vector (unsigned-byte 3)) commands))
  
  (let ((position 0)
        (command  (aref commands 0)))
    (declare (type fixnum                      position))
    (declare (type (or null (unsigned-byte 3)) command))
    
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
            
            ;; Increment the memory under the pointer.
            (#b000
              (incf (gethash pointer memory 0))
              (advance))
            
            ;; Decrement the memory under the pointer.
            (#b001
              (decf (gethash pointer memory 0))
              (advance))
            
            ;; Move pointer right.
            (#b010
              (incf pointer)
              (advance))
            
            ;; Move pointer left.
            (#b011
              (decf pointer)
              (advance))
            
            ;; Output the current cell value.
            (#b100
              (write-char (code-char (gethash pointer memory 0)))
              (advance))
            
            ;; Input a character and its code in the current cell.
            (#b101
              (format T "~&Please input a character: ")
              (let ((input (read-char)))
                (declare (type (or null character) input))
                (clear-input)
                (setf (gethash pointer memory) (char-code input)))
              (advance))
            
            ;; Jump past matching "1000" if the current cell value
            ;; equals 0.
            (#b110
              (cond
                ((zerop (gethash pointer memory 0))
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched '110'."))
                      (#b111
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (#b110
                        (incf level)
                        (advance))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back after "0111" if the current cell value does
            ;; not equal 0.
            (#b111
              (cond
                ((not (zerop (gethash pointer memory 0)))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case command
                      ((NIL)
                        (error "Unmatched '111'."))
                      (#b110
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#b111
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

(defun interpret-Ecstatic-binary (bits)
  "Interprets the Ecstatic code provided in its binary form by the BITS,
   which is tantamount to the number of necessary exclamation marks '!',
   and returns no value."
  (declare (type unsigned-byte bits))
  (interpret-Ecstatic-commands
    (extract-Ecstatic-binary-commands bits))
  (values))

;;; -------------------------------------------------------

(defun interpret-Ecstatic-code (code)
  "Interprets the piece of Ecstatic CODE provided in the form of a
   sequence of ecphonemes (\"!\") and returns no value."
  (declare (type string code))
  (interpret-Ecstatic-commands
    (extract-Ecstatic-binary-commands
      (convert-ecstatic-to-binary code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(convert-brainfuck-to-binary ",.")

;;; -------------------------------------------------------

(write-exclamation-points (convert-brainfuck-to-binary ",."))

;;; -------------------------------------------------------

;; "Hello World!" produces 533,996,783,846,484,205,380,094,159,863,410,223,580,295,148,513,114,591,953,982,777,027,276,626,436,038,069,668,611,776,594,948.
(convert-brainfuck-to-binary "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

;;; -------------------------------------------------------

;; "Hello, world!" produces 11,745,396,970,761,548,557,844,427,413,766,327,995,589,772,739,314,086,919,355,093,720,961,223,541,738,614,442,550,691,210,954,782,889,076,179,929,044.
(convert-brainfuck-to-binary "-[------->+<]>-.-[->+++++<]>++.+++++++..+++.[->+++++<]>+.------------.--[->++++<]>-.--------.+++.------.--------.-[--->+<]>.")

;;; -------------------------------------------------------

(convert-binary-to-brainfuck 108)

;;; -------------------------------------------------------

(convert-binary-to-brainfuck 108 NIL)

;;; -------------------------------------------------------

;; Prints ",.".
(convert-ecstatic-to-brainfuck "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Ecstatic-commands
  (extract-Ecstatic-binary-commands 108))

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Ecstatic-commands
  (extract-Ecstatic-binary-commands 56623))

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Ecstatic-binary 56623)

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Ecstatic-code "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

;;; -------------------------------------------------------

;; Print "Hello World!"
(interpret-Ecstatic-commands
  (extract-Ecstatic-binary-commands
    (convert-brainfuck-to-binary "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")))
