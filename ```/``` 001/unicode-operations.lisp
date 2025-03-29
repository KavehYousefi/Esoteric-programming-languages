;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the Unicode encoding and decoding operations in
;; relation to the ``` program memory.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Unicode encoding/decoding operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-unicode-code-point-to-memory (memory
                                           start-address
                                           code-point)
  "Stores the Unicode CODE-POINT in the MEMORY, commencing from the
   inclusive START-ADDRESS and whence expanding across the next 20
   cells, each of these a single bits' recipient from the CODE-POINTS'
   binary representation, the assignment proceeding from the most
   significant bit (MSB) towards the least significant one (LSB), and
   returns no value."
  (declare (type Memory             memory))
  (declare (type integer            start-address))
  (declare (type (unsigned-byte 21) code-point))
  (loop
    for current-address
      of-type integer
      from    start-address
      by      1
    and bit-position
      of-type (integer -1 20)
      from    20
      downto  0
    do
      (setf (cell-value memory current-address)
        (ldb (byte 1 bit-position) code-point)))
  (values))

;;; -------------------------------------------------------

(defun read-unicode-code-point-from-memory (memory start-address)
  "Reads from the MEMORY, proceeding from the inclusive START-ADDRESS
   and extending across the next 20 cells, a Unicode code point's
   binary representation, assembling thilk from the most significant
   bit (MSB), concurring with the START-ADDRESS, towards the least
   significant one (LSB), this being the 21st partaking of the catena,
   and returns the 21-bit unsigned byte representation."
  (declare (type Memory  memory))
  (declare (type integer start-address))
  (let ((restored-code-point #b000000000000000000000))
    (declare (type (unsigned-byte 21) restored-code-point))
    (loop
      for current-address
        of-type integer
        from    start-address
        by      1
      and bit-position
        of-type (integer -1 20)
        from    20
        downto  0
      do
        (setf (ldb (byte 1 bit-position) restored-code-point)
          (cell-bit-value memory current-address)))
    (the (unsigned-byte 21) restored-code-point)))
