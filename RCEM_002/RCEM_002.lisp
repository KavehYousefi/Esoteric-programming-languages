;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple parser and interpreter for the
;; esoteric programming language "RCEM" (Random Code Executation
;; Machine) invented by "Feuermonster".
;; 
;; Implementation
;; ==============
;; This implementation models the infinite tape by means of a
;; hash table, mapping each integer position, contingently negative,
;; to a ternary digit, or trit; in corollary, each cell, being a
;; composite of its index and its value, is molded into the hash table
;; key as the former, associating unambiguously with its value as that
;; of the cell --- hence, one tape cell constitutes one table entry.
;; 
;; Querying a non-existing entry by a cell position not yet defined
;; does not pose a difficulty, as such a request is replied to with the
;; default zero (0) value. It is important to note that a query of this
;; ilk does not concomitantly generate a new hash table entry. Only
;; explicit modifications, such as setting, incrementing or decrementing
;; a tape cell, generate new entries. Even a pointer movement to an
;; index not responding to a table entry key does not actuate a new
;; cell's establishment. In this regard, the tape implementation as a
;; hash table is not unlike a sparse matrix, which trades a more
;; resourceful design for reduced memory consumption.
;; 
;; In summary, the hash table approach, in juxtaposition to a vector
;; representation of the tape, offers the following benefits:
;;   (1) Definition of arbitrary cell indices:
;;         Cell positions may be modeled with more fidelity to the RCEM
;;       standard by permitting negative values, as well as a larger
;;       range. Hash tables are not restricted in their tally of
;;       entries, enabling a nearly "infinite" quantity of such. With
;;       the keys themselves being of the ``integer'' type, the
;;       unbounded nature of the cell enumeration becomes more tangible,
;;       as integers in Common Lisp, as opposed to ``fixnums'', are
;;       restricted in their magnitude merely by the system's memory.
;;       Arrays, or vectors as their one-dimensional counterpart, may
;;       only reference their elements by fixnum subscripts, which, per
;;       definition, imposes a --- implementation-dependent --- limit on
;;       their count.
;;   (2) Sparse structure:
;;         As stated above, the hash table implementation, generating
;;       entries as cell representatives only upon necessity, ultimately
;;       defines a sparse structure. Arrays, at least expressively, are
;;       not capable of tolerating gaps inside of their bounds. Both
;;       feasiblity in storage capacity and handling of memory
;;       impositions render hash tables in this matter a superior
;;       choice.
;; On the other hand, the following disadvantages ought be levied
;; against a hash table approach versus the array or vector:
;;   (1) Initial higher cost:
;;         Hash tables, being objects with a certain convolution in
;;       their implementation, when compared to the rather plain array
;;       substance, usually impose penalties for the stewardship of
;;       their state. These time and memory appropriations might, for a
;;       small number of modeled cells, be detrimental. It is, however,
;;       left to be hoped that the discriminations are slight at worst.
;;   (2) Unnatural modeling:
;;         A vector, by nature conceited to be enumerated numerically,
;;       more closely links to one's notion of a tape: a series of
;;       adjacent cells. In physical concerns, a hash table merely
;;       contains "randomly" arranged key-value, or position-cell,
;;       objects. This discrepancy in the modeling might confound
;;       readers and maintainers of this implementation.
;; 
;; As an apostille it shall be noted that entries are never deleted
;; from the hash table, even if amounting to a zero-valued cell, which,
;; as the default value, could be very well omitted. This fact finds
;; solely vindication in the simplicity of implementation. More
;; sophisticated versions of this program might introduce some
;; optimization strategy.
;; 
;; Cases of unspecified or underspecified formulations in the RCEM
;; documentation have perforce been mended by aid of assumptions. This
;; encompasses, among other things, the handling of the special "maybe"
;; value two (2) as a tape cell content in circumstances which
;; necessitate a tape section's interpretation as a binary sequence.
;; The choice has been to treat the number two analogously to a one (1).
;; 
;; Furthermore, decrementing a tape cell currently storing a value of
;; zero (0) or accepting user input interpretable as a negative number,
;; which would violate the valid ternary range of [0, 2] akin to an
;; increment aboon the upper bound, cannot be ascertained with perfect
;; confidence to rely upon the same, or even a similar, formula stated
;; in the documentation in accordance with the sense of:
;;   cellValue = cellValue % 3
;; Whether the ultimate cell value should be a "wrapped around" version
;; of the raw new value, or the deduction be simply ignored so as to
;; halt at zero, cannot be decided upon conclusively. The latter case
;; has been chosen in this implementation as canonical.
;; 
;; In a very like consideration, the I-Cell remains dubious in regard to
;; its negative manifestation. It is true that no limits are defined on
;; either laterality of its numeric gamut, yet two cases elude concrete
;; specification for values less than zero:
;;   (1) If intent on transferring the decimal I-Cell value to the tape,
;;       the binary representation of a negative base-10 quantity ought
;;       to be assumed in one of several possible ways, the two most
;;       popular of which are the two's complement and the one's
;;       complement, the former more widespread in use.
;;   (2) If intent on printing the I-Cell content as an ASCII character,
;;       a negative value does not map to any valid output.
;; The second point of dispute, involving the ASCII output, reaches into
;; the positive direction with values larger than 255 being formally not
;; specified as fit for the range. However, a translation by wrapping
;; around any decimal value n, intended to be translated into its ASCII
;; counterpart, using the formula
;;   n_new = n modulo 256
;; at least caters for a reasonable solution to this contention.
;; 
;; In order to fixate the specification, we apportion the principles of
;; least astonishment to the first issue by assuming a two's complement
;; as the standardized representation for any decimal number, positive,
;; zero, or negative. Nevertheless, in the current iteration of the
;; implementation no provisions are made to encode, decode or check
;; the I-Cell value's sign, adhering to simplicity athwart the benefit
;; of robustness. In corollary, a negative number is transferred upon
;; the tape as accorded by the Common Lisp system.
;; 
;; The second predicament concurs with the notions and compromises
;; explicated above by foregoing a particular handling of problematic
;; values in the version of the program at hand. It is expected that an
;; error, originating in the Common Lisp implementation's native
;; library, is signaled for a non-negative character code.
;; 
;; Loops define control structures that can either be entered, and
;; hence executed, or skipped entirely. If the introducing character,
;; or "initiator", is encountered, the entrance condition is indagated.
;; In the case of its fulfilment, each token inside of the iteration
;; body is interpreted. With the terminating character, or "terminator",
;; being encountered, the code position is returned to the corresponding
;; initiator character to check the iteration condition anew. If at any
;; time during the initiator check this condition fails, the position
;; is moved past the terminator without executing a single intermediate
;; command, and is placed immediately unto the character succeeding it.
;; This means that the terminating marker, in the case of a failure to
;; satisfy the loop entrance requirement, is completely negligible to
;; the surrounding code, being simply ignored. In pseudocode we obtain:
;;   (1) Read the initiator character.
;;   (2) Perform the loop entrance condition check for the initiator.
;;       (2.a) If the condition is satisfied:
;;             (2.a.1) Read the next character.
;;             (2.a.2) Check this character c.
;;                     (2.a.2.a) If c is a terminator: return to (1).
;;                     (2.a.2.b) Else: execute the action associated
;;                                     with the character.
;;       (2.b) If the condition is not satisfied:
;;             (2.b.1) Find the position p of the matching terminator.
;;             (2.b.2) Set the current code position pointer to (p + 1).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-05-22
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/RCEM"
;;   -> "https://en.wikipedia.org/wiki/Ternary_numeral_system"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype trit ()
  "The value compatible for storage in cells is restricted to the
   ternary choice 0 (Boolean false), 1 (Boolean true), and 2 (maybe),
   thus forming a trit (trinary digit)."
  '(integer 0 2))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional key-type value-type)
  "Defines a hash table whose keys are all of the type KEY-TYPE and
   whose values are all of the type VALUE-TYPE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and (hash-table-p object)
               (loop
                 for    key being the hash-keys in (the hash-table object)
                 using  (hash-value value)
                 always (and (typep key   key-type)
                             (typep value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ((cells
    :initarg       :cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer trit)
    :documentation "A sparse collection of cells, where each entry key
                    stores the integer cell position, mapped to the
                    ternary digit cell value.
                    ---
                    Querying a cell whose position does not map to the
                    key of any extant value does not create a new entry,
                    but instead returns the default value zero (0).
                    Entries are only created if the cell value is
                    explicitly set, incremented, or decremented. Note
                    that the cell at the POINTER as well does not need
                    to point to an existing entry.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          integer
    :documentation "The position of the current tape cell.
                    ---
                    As the tape is capable of expanding along both axes
                    without any bounds, the integer pointer might be
                    arbitrarily large or small."))
  (:documentation
    "Models the infinite tape of the RCEM architecture using a hash
     table, maintaining a current cell pointer.
     ---
     A tape cell manifests itself in a table entry, with the key
     designating the cell position and the associated value the cell
     value. Initially empty, entries are added only if explicitly
     desired or necessary: If the value of the current cell or one at
     a particular position is set, incremented, or decremented, a
     lacking is defined in the hash table. If a cell at the current or
     any other position is queried for its value, and no entry exists
     yet to model it, the default value zero (0) is returned. In this
     fashion, the hash table acts as a sparse data structure, similar
     to a sparse matrix that contains non-zero entries. Of course, in
     the tape's hash table, zero entries may exist. Additionally, no
     entries are removed, even the aforementioned zero-keyed ones."))

;;; -------------------------------------------------------

(defun make-tape ()
  "Creates and returns a new empty ``Tape''."
  (the Tape (make-instance 'Tape)))

;;; -------------------------------------------------------

(defun tape-current-cell (tape)
  "Returns the value of the current TAPE cell, as referenced by its
   pointer.
   ---
   If the cell at the current pointer does not yet exist, it is not
   created; instead the default value of zero (0) is returned."
  (declare (type Tape tape))
  (with-slots (cells pointer) tape
    (the trit (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defun (setf tape-current-cell) (new-cell-value tape)
  "Sets the value of the current TAPE cell, as referenced by its
   pointer, to the NEW-CELL-VALUE, possibly adjusted prior to its
   insertion, and returns the ultimate new value of the cell.
   ---
   If the NEW-CELL-VALUE exceeds the valid range of a ternary digit, or
   trit, of [0, 2], it is adjusted prior to its use, then utilized to
   update the current cell."
  (declare (type integer new-cell-value))
  (declare (type Tape    tape))
  (let ((new-value-as-trit (mod new-cell-value 3)))
    (declare (type trit new-value-as-trit))
    (with-slots (cells pointer) tape
      (setf (gethash pointer cells) new-value-as-trit)
      (the trit new-value-as-trit))))

;;; -------------------------------------------------------

(defun tape-cell-at (tape cell-index)
  "Returns the value of the TAPE cell at the CELL-INDEX.
   ---
   If a cell at the CELL-INDEX does not exist, it is not created;
   instead the default value of zero (0) is returned."
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (with-slots (cells) tape
    (the trit (gethash cell-index cells 0))))

;;; -------------------------------------------------------

(defun (setf tape-cell-at) (new-cell-value tape cell-index)
  "Sets the value of the TAPE cell located at the CELL-INDEX to the
   NEW-CELL-VALUE, possibly adjusting the same to be in the valid trit
   range [0, 2], and returns the ultimate new value of the cell.
      ---
   If the NEW-CELL-VALUE exceeds the valid range of a ternary digit, or
   trit, of [0, 2], it is adjusted prior to its use, then utilized to
   update the current cell."
  (declare (type integer new-cell-value))
  (declare (type Tape    tape))
  (declare (type integer cell-index))
  (let ((new-value-as-trit (mod new-cell-value 3)))
    (declare (type trit new-value-as-trit))
    (with-slots (cells) tape
      (setf (gethash cell-index cells) new-value-as-trit)
      (the trit new-value-as-trit))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-rcem (code)
  "Parses and interprets the RCEM CODE and returns the ``NIL'' value."
  (declare (type string code))
  (let ((position 0)            ;; Index of the current CODE character.
        (memory   (make-tape))  ;; The infinite tape.
        (i-cell   0))           ;; The I-Cell.
    (declare (type fixnum  position))
    (declare (type Tape    memory))
    (declare (type integer i-cell))
    (with-slots (cells pointer) memory
      (flet ((expect-token (token)
              "Checks whether the next characters of the CODE, starting at
               the current POSITION, match those of the TOKEN, advancing
               the POSITION in the process. If a mismatch occurs betwixt
               a CODE character and a TOKEN element, an error is signaled;
               otherwise the POSITION is set to the index of the character
               immediately following the match in the CODE."
              (declare (type string token))
              (loop
                for token-character of-type character across token
                for code-character  of-type character = (char code position)
                do  (if (char= token-character code-character)
                      (incf position)
                      (error "Invalid character: Expected ~s, but ~
                              encountered ~s at position ~d."
                             token-character code-character position))))
             
             (peek-next-character ()
              "Returns the next character of the CODE without moving the
               POSITION."
              (the character (char code (1+ position))))
             
             (read-integer ()
              "Starting at the current POSITION in the CODE, reads an
               unsigned integer number, sets the POSITION to the index
               of the character immediately following the parsed content,
               and returns the parsed value."
              (let ((end (or (position-if-not #'digit-char-p code :start position)
                             (length code))))
                (declare (type fixnum end))
                (the integer
                  (prog1
                    (parse-integer code :start position :end end)
                    (setf position end)))))
             
             (move-past-section-terminator (initiator terminator)
              "Starting at the current POSITION, sets its value to the
               index of the character immediately following the TERMINATOR
               character which, taken nesting into account, corresponds to
               the INITIATOR. This allows to skip code sections ensconced
               by an iteration constructor, for instance ``<x_o_>''."
              (declare (type character initiator))
              (declare (type character terminator))
              (let ((nesting-level 0))
                (declare (type (integer 0 *) nesting-level))
                (loop do
                  (incf position)
                  (let ((current-character (char code position)))
                    (declare (type character current-character))
                    (cond
                      ((char= current-character terminator)
                        (if (zerop nesting-level)
                          (loop-finish)
                          (decf nesting-level)))
                      ((char= current-character initiator)
                        (incf nesting-level))
                      (T NIL)))))
              (incf position)
              (the fixnum position))
             
             (move-to-section-initiator (initiator terminator)
              "Starting at the current POSITION, sets its value to the
               section INITIATOR character which, taken nesting into
               account, corresponds to the TERMINATOR, and returns the
               modified POSITION."
              (declare (type character initiator))
              (declare (type character terminator))
              (let ((nesting-level 0))
                (declare (type (integer 0 *) nesting-level))
                (loop do
                  (decf position)
                  (let ((current-character (char code position)))
                    (declare (type character current-character))
                    (cond
                      ((char= current-character initiator)
                        (if (zerop nesting-level)
                          (loop-finish)
                          (decf nesting-level)))
                      ((char= current-character terminator)
                        (incf nesting-level))
                      (T NIL)))))
              (the fixnum position))
             
             (check-cell-indices (start-cell-index end-cell-index)
              "Checks whether the START-CELL-INDEX is less than or equal
               to the END-CELL-INDEX, throwing an error upon a violation
               of this condition, otherwise returning ``T''."
              (declare (type integer start-cell-index))
              (declare (type integer end-cell-index))
              (if (> start-cell-index end-cell-index)
                (error "The start index ~d must be less than or equal ~
                        to the end index ~d."
                       start-cell-index end-cell-index)
                T)))
        
        (loop
          while (< position (length code))
          for   character of-type character = (char code position)
          do
          (case character
            
            ;; -- Instructions. --------------------------------------- ;;
            
            (#\r
              (incf position)
              (let ((number-of-steps (read-integer)))
                (declare (type integer number-of-steps))
                (incf pointer number-of-steps)))
            
            (#\l
              (incf position)
              (let ((number-of-steps (read-integer)))
                (declare (type integer number-of-steps))
                (decf pointer number-of-steps)))
            
            (#\s
              (incf position)
              (let ((new-cell-value (read-integer)))
                (declare (type integer new-cell-value))
                (setf (tape-current-cell memory) new-cell-value)))
            
            (#\o
              (expect-token "o_")
              (format T "~a" (tape-current-cell memory)))
            
            (#\i
              (expect-token "i_")
              (let ((input (read)))
                (declare (type T input))
                (setf (tape-current-cell memory) input)))
            
            (#\x
              (expect-token "x_")
              (setf (tape-current-cell memory) (random 3)))
            
            (#\^
              (incf position)
              (let ((pointer-offset (read-integer)))
                (declare (type integer pointer-offset))
                (setf (tape-current-cell memory)
                      (logxor
                        (tape-current-cell memory)
                        (tape-cell-at memory (+ pointer pointer-offset))))))
            
            (#\+
              (let ((next-character (peek-next-character)))
                (declare (type character next-character))
                (case next-character
                  ;; Command: ``+*''.
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (incf position)
                    (let ((pointer-offset (read-integer)))
                      (declare (type integer pointer-offset))
                      (setf (tape-current-cell memory)
                            (logand
                              (tape-current-cell memory)
                              (tape-cell-at memory (+ pointer pointer-offset))))))
                  ;; Command: ``++''.
                  (#\+
                    (expect-token "++")
                    (incf (tape-current-cell memory) 1))
                  (otherwise
                    (error "Invalid command: +~c." next-character)))))
            
            (#\-
              (expect-token "--")
              (decf (tape-current-cell memory) 1))
            
            (#\c
              (expect-token "c_")
              (case (tape-current-cell memory)
                (0 (setf (tape-current-cell memory) 1))
                (1 (setf (tape-current-cell memory) 0))
                (T NIL)))
            
            (#\2
              (incf position)
              (let ((new-cell-value (read-integer)))
                (declare (type integer new-cell-value))
                (declare (ignorable    new-cell-value))
                (when (= (tape-current-cell memory) 2)
                  (setf (tape-current-cell memory) new-cell-value))))
            
            (#\m
              (incf position)
              (let ((next-character (char code position)))
                (declare (type character next-character))
                (case next-character
                  (#\+
                    (incf i-cell 1)
                    (incf position))
                  
                  (#\-
                    (decf i-cell 1)
                    (incf position))
                  
                  (#\p
                    (format T "~d" i-cell)
                    (incf position))
                  
                  (#\o
                    (format T "~c" (code-char i-cell))
                    (incf position))
                  
                  (#\i
                    (let ((input (read)))
                      (declare (type T input))
                      (setf i-cell input)
                      (incf position)))
                  
                  (#\:
                    (expect-token "::")
                    
                    (let ((start-cell-index 0)
                          (end-cell-index   0)
                          (new-i-cell-value 0))
                      (declare (type integer start-cell-index))
                      (declare (type integer end-cell-index))
                      (declare (type integer new-i-cell-value))
                      
                      (setf start-cell-index (read-integer))
                      (expect-token "::")
                      (setf end-cell-index (read-integer))
                      
                      (check-cell-indices start-cell-index end-cell-index)
                      
                      (loop
                        for cell-index of-type integer from end-cell-index downto start-cell-index
                        for cell-value of-type trit    = (tape-cell-at memory cell-index)
                        for bit-index  from 0 by 1
                        do  (case cell-value
                              (0 (setf (ldb (byte 1 bit-index) new-i-cell-value) 0))
                              (1 (setf (ldb (byte 1 bit-index) new-i-cell-value) 1))
                              (2 (setf (ldb (byte 1 bit-index) new-i-cell-value) 1))
                              (T (error "Invalid cell value in cell ~d: ~d." cell-index cell-value))))
                      (setf i-cell new-i-cell-value)))
                  
                  (otherwise
                    (error "Invalid command: ``m~c''." next-character)))))
            
            (#\z
              (incf position)
              (expect-token "::")
              
              (let ((start-cell-index 0)
                    (end-cell-index   0))
                (declare (type integer start-cell-index))
                (declare (type integer end-cell-index))
                
                (setf start-cell-index (read-integer))
                (expect-token "::")
                (setf end-cell-index (read-integer))
                
                (check-cell-indices start-cell-index end-cell-index)
                
                (loop
                  for cell-index
                      of-type integer
                      from    start-cell-index
                      to      end-cell-index
                  for i-cell-bit-index
                      of-type unsigned-byte
                      from    0
                      by      1
                  do  (setf (tape-cell-at memory cell-index)
                            (if (logbitp i-cell-bit-index i-cell)
                                1
                                0)))))
            
            
            ;; -- Iteration constructs. ------------------------------- ;;
            
            (#\(
              (if (zerop (tape-current-cell memory))
                (incf position)
                (move-past-section-terminator #\( #\))))
            
            (#\)
              (move-to-section-initiator #\( #\)))
            
            (#\<
              (if (not (zerop i-cell))
                (incf position)
                (move-past-section-terminator #\< #\>)))
            
            (#\>
              (move-to-section-initiator #\< #\>))
            
            (#\/
              (if (= (tape-current-cell memory) 2)
                (incf position)
                (move-past-section-terminator #\/ #\\)))
            
            (#\\
              (move-to-section-initiator #\/ #\\))
            
            (#\[
              (if (or (= (tape-current-cell memory) 2)
                      (= (random 2) 1))
                (incf position)
                (move-past-section-terminator #\[ #\])))
            
            (#\]
              (move-to-section-initiator #\[ #\]))
            
            (otherwise
              (error "Invalid command: ~s at position ~d."
                     character position))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
(parse-rcem "s2[r1s2]")

(parse-rcem "s2o_")

;; Random loop.
;; ---
;; Does not print anything.
(parse-rcem "x_[r1x_]")

;; Version of the above which prints the current cell.
(parse-rcem "x_[r1x_o_]")

;; Random loop.
(parse-rcem "x_r9([r1][l2]x_)")

;; Prints the character 'A'.
(parse-rcem "r65s1l65(m+r1)mo")

;; Prints the decimal number 5.
(parse-rcem "s0r1s1r1s0r1s1l3m::0::3mp")

;; Prints the binary digit sequence '101'.
(parse-rcem "m+m+m+m+m+z::0::2o_r1o_r1o_")

;; Prints either 0 or 1.
(parse-rcem "x_/x_\\o_")

;; Sets the I-Cell to 0.
;; ---
;; Does not print anything.
(parse-rcem "<m->")

;; Version of the above which prints the I-Cell value.
(parse-rcem "<m->mp")
