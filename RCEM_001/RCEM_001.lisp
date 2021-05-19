;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple parser and interpreter for the
;; esoteric programming language "RCEM" (Random Code Executation
;; Machine) invented by "Feuermonster".
;; 
;; Implementation
;; ==============
;; For the purpose of simplicity, this implementation relies upon a
;; vector of ternary values, substituted upon transgression of its
;; length by one resized. This choice concords only partially with the
;; infinite tape length mandated by the RCEM architecture --- not only
;; because of the innate upper bound for an array size in Common Lisp,
;; but especially in that all indices are restricted to a non-negative
;; range, thus failing to satisfy the unconstraint expansion of the
;; represented tape along the negative moeity. A later version might
;; barter this naive approach with a hash table as to valorize
;; authenticity.
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
;; Date:   2021-05-16
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-rcem (code)
  "Parses and interprets the RCEM code and returns the ``NIL'' value."
  (declare (type string code))
  (let ((position 0)
        (memory   (make-array 30000 :element-type 'trit :initial-element 0))
        (pointer  0)
        (i-cell   0))
    (declare (type fixnum          position))
    (declare (type (vector trit *) memory))
    (declare (type fixnum          pointer))
    (declare (type integer         i-cell))
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
            (declare (type fixnum start-cell-index))
            (declare (type fixnum end-cell-index))
            (if (> start-cell-index end-cell-index)
              (error "The start index ~d must be less than or equal to ~
                      the end index ~d."
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
              (incf pointer number-of-steps))
              
              (when (>= pointer (length memory))
                (setf memory (adjust-array code (1+ pointer)))))
          
          (#\l
            (incf position)
            (let ((number-of-steps (read-integer)))
              (declare (type integer number-of-steps))
              (decf pointer number-of-steps)))
          
          (#\s
            (incf position)
            (let ((new-cell-value (read-integer)))
              (declare (type integer new-cell-value))
              (setf new-cell-value (mod new-cell-value 3))
              (setf (aref memory pointer) new-cell-value)))
          
          (#\o
            (expect-token "o_")
            (format T "~a" (aref memory pointer)))
          
          (#\i
            (expect-token "i_")
            (let ((input (read)))
              (declare (type T input))
              (setf input (mod input 3))
              (setf (aref memory pointer) input)))
          
          (#\x
            (expect-token "x_")
            (setf (aref memory pointer) (random 3)))
          
          (#\^
            (incf position)
            (let ((pointer-offset (read-integer)))
              (declare (type integer pointer-offset))
              (setf (aref memory pointer)
                    (logxor
                      (aref memory pointer)
                      (aref memory (+ pointer pointer-offset))))))
          
          (#\+
            (let ((next-character (peek-next-character)))
              (declare (type character next-character))
              (case next-character
                ;; Command: ``+*''.
                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                  (incf position)
                  (let ((pointer-offset (read-integer)))
                    (declare (type integer pointer-offset))
                    (setf (aref memory pointer)
                          (logand
                            (aref memory pointer)
                            (aref memory (+ pointer pointer-offset))))))
                ;; Command: ``++''.
                (#\+
                  (expect-token "++")
                  (incf (aref memory pointer) 1))
                (otherwise
                  (error "Invalid command: +~c." next-character)))))
          
          (#\-
            (expect-token "--")
            (decf (aref memory pointer) 1))
          
          (#\c
            (expect-token "c_")
            (case (aref memory pointer)
              (0 (setf (aref memory pointer) 1))
              (1 (setf (aref memory pointer) 0))
              (T NIL)))
          
          (#\2
            (incf position)
            (let ((new-cell-value (read-integer)))
              (declare (type integer new-cell-value))
              (declare (ignorable    new-cell-value))
              (when (= (aref memory pointer) 2)
                (setf (aref memory pointer)
                      (mod new-cell-value 3)))))
          
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
                  (format T "~c" (code-char I-cell))
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
                    (declare (type fixnum  start-cell-index))
                    (declare (type fixnum  end-cell-index))
                    (declare (type integer new-i-cell-value))
                    
                    (setf start-cell-index (read-integer))
                    (expect-token "::")
                    (setf end-cell-index (read-integer))
                    
                    (check-cell-indices start-cell-index end-cell-index)
                    
                    (loop
                      for cell-index of-type integer from end-cell-index downto start-cell-index
                      for cell-value of-type trit    = (aref memory cell-index)
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
            (let ((start-cell-index 0)
                  (end-cell-index   0))
              (declare (type fixnum start-cell-index))
              (declare (type fixnum end-cell-index))
              
              (expect-token "::")
              (setf start-cell-index (read-integer))
              (expect-token "::")
              (setf end-cell-index (read-integer))
              
              (check-cell-indices start-cell-index end-cell-index)
              
              (loop
                for cell-index of-type fixnum
                               from    start-cell-index
                               to      end-cell-index
                for i-cell-bit-index of-type bit from 0 by 1
                do  (setf (aref memory cell-index)
                          (if (logbitp i-cell-bit-index i-cell)
                              1
                              0)))))
          
          
          ;; -- Iteration constructs. ------------------------------- ;;
          
          (#\(
            (if (zerop (aref memory pointer))
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
            (if (= (aref memory pointer) 2)
              (incf position)
              (move-past-section-terminator #\/ #\\)))
          
          (#\\
            (move-to-section-initiator #\/ #\\))
          
          (#\[
            (if (or (= (aref memory pointer) 2)
                    (= (random 2) 1))
              (incf position)
              (move-past-section-terminator #\[ #\])))
          
          (#\]
            (move-to-section-initiator #\[ #\]))
          
          (otherwise
            (error "Invalid command: ~s at position ~d."
                   character position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
;; ---
;; Note that this eventually causes an error because of the finite size
;; of the MEMORY vector.
;; 
(parse-rcem "s2[r1s2]")

(parse-rcem "s2o_")

;; Random loop.
;; ---
;; Does not print anything.
;; 
(parse-rcem "x_[r1x_]")

;; Version of the above which prints the current cell.
;; 
(parse-rcem "x_[r1x_o_]")

;; <!!!> ERROR: CAUSES A NEGATIVE ARRAY INDEX ERROR. <!!!>
;; 
(parse-rcem "x_r9([r1][l2]x_)")

;; Prints the character 'A'.
;; 
(parse-rcem "r65s1l65(m+r1)mo")

;; Prints the decimal number 5.
;; 
(parse-rcem "s0r1s1r1s0r1s1l3m::0::3mp")

;; Prints the binary digit sequence '101'.
;; 
(parse-rcem "m+m+m+m+m+z::0::2o_r1o_r1o_")

;; Prints either 0 or 1.
;; 
(parse-rcem "x_/x_\\o_")

;; Sets the I-Cell to 0.
;; ---
;; Does not print anything.
;; 
(parse-rcem "<m->")

;; Version of the above which prints the I-Cell value.
;; 
(parse-rcem "<m->mp")
