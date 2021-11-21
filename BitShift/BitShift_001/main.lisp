;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "BitShift", invented by the Esolang user "Bas".
;; 
;; Concept
;; =======
;; The language's concept originates from a nostalgic evocation of
;; erstwhile times in which programming, immersed in its infancy,
;; imposed upon the programmer a perforce exposure to the low-level bit
;; representation of his trade. A corollary of this, BitShift programs
;; are designed as sequences composed of bits only, represented strictly
;; by zeroes (0) and ones (1). Bits of alternating value,
;; indiscriminative regarding the order, and demarcated by the
;; occurrence of two equal bits, define a group, the tally of its
;; members, being the count of alternations, acting in the identity of
;; the respective command. Lacking control structures, these
;; instructions operate merely on a single octet value.
;; 
;; == ALTERNATING BITS FORM A GROUP ==
;; A piece of BitShift source code may only contain the characters "0"
;; and "1". Bits of alternating value in adjacency compose a group,
;; terminated either by the occurrence of two bits of equal content or
;; by the end of the program. If the demarcation concurs with the
;; confrontation of two identical characters, the left bit is tallied
;; among the closing group, while its peer accounts for the first member
;; of the subsequent compound.
;; 
;; == THE GROUP SIZE DETERMINES THE COMMAND ==
;; Commands in BitShift do not depend on arguments and abstain from
;; identification by agnomination; instead, each bit group forms an
;; instruction, with its number of bits defining the type. The order of
;; bits carries no significance at all; this means that the sequence
;; "0101" is paregal to "1010", with both alluding to the same
;; instruction designated by four bits. Valid group sizes range from
;; inclusive one (1) to inclusive seven (7); any other cardinality
;; incurs a violation and provokes an error.
;; 
;; == DATA IS STORED IN A SINGLE SCALAR VALUE ==
;; All commands operate on the singular data storage object, the
;; "value", a byte composed of eight bits and capable of translating
;; from and to an ASCII character.
;; 
;; 
;; Architecture
;; ============
;; BitShift does not rely on a specific architecture, relaying its
;; memory to a simple 8-bit value, that is, an integer confined to the
;; range [0, 255].
;; 
;; 
;; Data Types
;; ==========
;; The purposefully assumed low-level nature of BitShift accommodates
;; exclusively bits and bytes as objects of currency.
;; 
;; The source code itself encompasses bits in the form of "0" and "1"
;; entities as its sole constituents. The only data storage, the scalar
;; datum known by a generic nomenclature as "value", manifests as an
;; octet, a composition itself of eight bits, and sufficiently potent to
;; occupy the integer range [0, 255].
;; 
;; The operations provided as manipulators upon the value are themselves
;; members of the logical category, including bit shifts, the exclusive
;; or (XOR), and the resetting to zero. Direct handles for such basic
;; actions as incrementing and decrementing elude BitShift's primitive
;; philosophy.
;; 
;; The value's byte type does not account for an accident, as it
;; responds to the ASCII character repertoire, itself encoded in eight
;; bits of a compatible range, and patently involved in interaction when
;; printing the value as a character and converting a user input from
;; the latter to the prior realm.
;; 
;; 
;; Syntax
;; ======
;; BitShift relies upon an utterly homogeneous and simple syntax, with
;; each program composed of zero or more bits, manifesting in the
;; characters "0" and "1". The language form can be expressed in the
;; following Extended Backus-Naur Form (EBNF) description:
;; 
;;   program : { bit } ;
;;   bit     : "0" | "1" ;
;; 
;; 
;; Instructions
;; ============
;; Instructions in BitShift entail solely niladic operations,
;; addressable by the cardinality of a group of bits, and restrained to
;; the integer range [1, 7]. The command type enumerate as follows,
;; where each alternation, or group size, is juxtaposed with the
;; resulting effect.
;; 
;;   Group size | Effect
;;   -----------+------------------------------------------------------
;;    1         | Shifts the value one bit to the left.
;;              | If the value tallies more than eight bits following
;;              | this operation, the adscititious digits are discarded.
;;   ..................................................................
;;    2         | Shifts the value one bit to the right.
;;              | If the value tallies more than eight bits following
;;              | this operation, the adscititious digits are discarded.
;;   ..................................................................
;;    3         | Sets the value to (value XOR 1).
;;   ..................................................................
;;    4         | Sets the value to (value XOR 128).
;;   ..................................................................
;;    5         | Sets the value to zero (0).
;;   ..................................................................
;;    6         | Prints to the standard output the ASCII character
;;              | associated with the value.
;;   ..................................................................
;;    7         | Prompts the user for an ASCII character and stores
;;              | its character code into the value.
;; 
;; 
;; Implementation
;; ==============
;; This Common Lisp implementation of BitShift is designed with the
;; intention of maximum simplicity. While the esoteric language's
;; components torely inflict predicaments, the value storage must be
;; handled with a modicum of caution in order to align with type safety.
;; Being an octet, its type specification may naturally be declared as
;; an eight-bit unsigned byte --- in Common Lisp's parlance:
;; ``(unsigned-byte 8)''. Such a declaration, however, cannot be
;; preserved in every case without detours. Bit shift operations may
;; temporarily export the value outside of its interval [0, 255]; if,
;; for instance, a left shift is applied to the bit sequence "10000010",
;; its content is computed as "100000100", a datum compact of nine
;; digits and by this in conflict with the declared species. Existing in
;; this state, even with its evanescent mode, the octet range is
;; violated. Needlings, the superfluous most significant bit "1" must
;; be curtailed manually, done so using ``(ldb (byte 8 0) value)''.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-11-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BitShift"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BitShift (code)
  "Interprets the piece of BitShift CODE and returns no value."
  (declare (type string code))
  (let ((value        0)
        (alternations 0))
    (declare (type (unsigned-byte 8) value))
    (declare (type (integer 0 *)     alternations))
    (flet ((evaluate-alternations ()
            "Indagates the current ALTERNATIONS and executes a command
             based upon its state, returning no value.
             ---
             An error is signaled if the ALTERNATIONS does not associate
             with any valid command."
            (case alternations
              (1 (setf value (ldb (byte 8 0) (ash value  1))))
              (2 (setf value (ldb (byte 8 0) (ash value -1))))
              (3 (setf value (logxor value 1)))
              (4 (setf value (logxor value 128)))
              (5 (setf value 0))
              (6 (write-char (code-char value)))
              (7 (format T "~&Please input a character: ")
                 (setf value (char-code (read-char)))
                 (clear-input))
              (T (error "Invalid number of alternations: ~d."
                   alternations)))
            (values)))
      (loop
        for previous-bit
          of-type (or null character)
          =       NIL
          then    current-bit
        for current-bit
          of-type (or null character)
          across  code
        for position
          of-type fixnum
          from    0
        do
          (cond
            ;; The CURRENT-BIT is an invalid character?
            ;; => Signal an error.
            ((not (find current-bit "01" :test #'char=))
              (error "Invalid character ~s at position ~d."
                current-bit position))
            ;; No PREVIOUS-BIT exists?
            ;; => Start the first group.
            ((null previous-bit)
              (incf alternations))
            ;; The PREVIOUS-BIT and CURRENT-BIT differ?
            ;; => Increase the group size.
            ((char/= previous-bit current-bit)
              (incf alternations))
            ;; The PREVIOUS-BIT and CURRENT-BIT are equal?
            ;; => Evaluate the current group, then start a new one.
            (T
              (evaluate-alternations)
              (setf alternations 1)))
        ;; Evaluate the desinent group, not terminated by two identical
        ;; bits in succession, but by the end of the CODE.
        finally
          (evaluate-alternations))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello,_World!".
(interpret-BitShift
  "0100000100000101011010011011010100110100110100110101001010110010001001010111100110011110101001000000010101101111011110111011101101010001000101100101011111011101001010110111101110111101010001000010101111101101010")

;;; -------------------------------------------------------

;; Cat program which executes a single time.
(interpret-BitShift "0101010010101")
