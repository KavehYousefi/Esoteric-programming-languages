;; Author: Kaveh Yousefi
;; Date:   2022-03-15
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Implieses"
;;   -> "https://en.wikipedia.org/wiki/Material_conditional"
;;       o Describes the implication, or logical "imply" operator, also
;;         known as "material condition" or "material implication".
;;   -> "https://en.wikipedia.org/wiki/Boolean_algebra#Secondary_operations"
;;       o Mentions the derivation of the implication "x -> y" by
;;           x -> y = not(x) OR y



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each of
   which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun parse-bit-string (bit-string)
  "Parses the BIT-STRING, expected to be composed of binary digits and
   whitespaces only, returning a ``simple-bit-vector'' representation
   thereof."
  (declare (type string bit-string))
  (let ((digits NIL))
    (declare (type (list-of bit) digits))
    (loop for character of-type character across bit-string do
      (cond
        ((digit-char-p character 2)
          (push (digit-char-p character 2) digits))
        ((whitespace-character-p character)
          NIL)
        (T
          (error "Invalid character '~c' detected in the bit string ~s."
            character bit-string))))
    (the simple-bit-vector
      (coerce (nreverse digits) 'simple-bit-vector))))

;;; -------------------------------------------------------

(defun print-bits (bits &key (start-position 0))
  "Beginning with the START-POSITION, interprets any octet of adjacent
   digits inside the BITS as an ASCII character code, contingently
   padding insufficient content with trailing zero-bits, prints the
   corresponding ASCII character to the standard output, and finally
   returns the BITS."
  (declare (type bit-vector bits))
  (declare (type fixnum     start-position))
  (let ((end-position 0))
    (declare (type fixnum end-position))
    (flet
        ((read-ASCII-code ()
          "Reads the next maximum of eight bits from the BITS sequence,
           parses this binary portion as a decimal integer, updates the
           START-POSITION, and returns the parsed decimal value."
          (setf end-position (min (+ start-position 8) (length bits)))
          (the (unsigned-byte 8)
            (prog1
              (parse-integer
                (format NIL "~8,1,0,'0@<~a~>"
                  (with-output-to-string (digits)
                    (declare (type string-stream digits))
                    (loop
                      for bit-position
                        of-type fixnum
                        from    start-position
                        below   end-position
                      for bit
                        of-type bit
                        =       (bit bits bit-position)
                      do
                        (write bit :stream digits))))
                :radix 2)
              (setf start-position end-position)))))
      (loop while (< end-position (length bits)) do
        (write-char (code-char (read-ASCII-code))))))
  (the bit-vector bits))

;;; -------------------------------------------------------

(defun append-zero-bit (bits)
  "Creates and returns a new ``simple-bit-vector'' based upon the BITS,
   extended by a single zero-bit as its appendix.
   ---
   The BITS will not be modified."
  (declare (type bit-vector bits))
  (the simple-bit-vector (concatenate 'simple-bit-vector bits #*0)))

;;; -------------------------------------------------------

(defun implication-of-bits (bits)
  "Calculates the material implication of the BITS, inducing each two
   consecutive bits as operands into the logical operation and gathering
   the results in a new ``bit-vector'', which is subsequently returned.
   ---
   Please heed the prerequisite that the length of the BITS shall be
   even.
   ---
   Given the pairwise working of this function, the resulting bit
   vector will always be exactly of half the length of the input BITS."
  (declare (type bit-vector bits))
  (flet ((logical-imply (bit-1 bit-2)
          "Calculates the material implication of BIT-1 and BIT-2,
           returning a new ``bit'' representing the result."
          (declare (type bit bit-1))
          (declare (type bit bit-2))
          (the bit
            (if (and (= bit-1 1)
                     (= bit-2 0))
              0
              1))))
    (the simple-bit-vector
      (coerce
        (loop
          for bit-index of-type fixnum from 0 below (length bits) by 2
          collect
            (logical-imply
              (bit bits bit-index)
              (bit bits (1+ bit-index))))
        'simple-bit-vector))))

;;; -------------------------------------------------------

(defun interpret-Implieses (initial-state)
  "Interprets the Implieses program defined by the INITIAL-STATE and
   returns as its result the last state as a ``bit-vector''.
   ---
   The INITIAL-STATE must resolve to a bit string whose tolerance
   exclusively admits the binary digits '0' and '1', as well as spaces,
   tabs, and newlines for the purpose of augmented readability."
  (declare (type string initial-state))
  (let ((state (parse-bit-string initial-state)))
    (declare (type simple-bit-vector))
    (loop do
      (cond
        ;; First bit is zero?
        ;; => Print bit sequence as ASCII characters and terminate.
        ((zerop (bit state 0))
          (print-bits state :start-position 1)
          (loop-finish))
        ;; Odd number of bits?
        ;; => Append zero-bit.
        ((oddp (length state))
          (setf state (append-zero-bit state)))
        ;; Even number of bits?
        ;; => Set STATE to implication of its own bits.
        (T
          (setf state (implication-of-bits state)))))
    (the simple-bit-vector state)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-Implieses "0 01001000 01100101 01101100 01101100 01101111 00101100 00100000 01110111 01101111 01110010 01101100 01100100 00100001")

;;; -------------------------------------------------------

;; Return the XOR combination of the bits 0 and 0.
;; Pattern = 11111111 11111110 ab101110 1111ba10
;; a = 0, b = 0
;; a XOR b = 0 (= 00)
(interpret-Implieses "11111111 11111110 00101110 11110010")

;;; -------------------------------------------------------

;; Return the XOR combination of the bits 0 and 1.
;; Pattern = 11111111 11111110 ab101110 1111ba10
;; a = 0, b = 1
;; a XOR b = 1 (= 01)
(interpret-Implieses "11111111 11111110 01101110 11111010")

;;; -------------------------------------------------------

;; Return the XOR combination of the bits 1 and 0.
;; Pattern = 11111111 11111110 ab101110 1111ba10
;; a = 1, b = 0
;; a XOR b = 1 (= 01)
(interpret-Implieses "11111111 11111110 10101110 11110110")

;;; -------------------------------------------------------

;; Return the XOR combination of the bits 1 and 1.
;; Pattern = 11111111 11111110 ab101110 1111ba10
;; a = 1, b = 1
;; a XOR b = 0 (= 00)
(interpret-Implieses "11111111 11111110 11101110 11111110")
