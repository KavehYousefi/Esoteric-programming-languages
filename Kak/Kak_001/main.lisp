;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Kak", invented by the Esolang user "ChuckEsoteric08" and
;; presented in the year 2022, founded upon the manipulation of an
;; infinite bit tape by mediation of a minimalistic syntax that embraces
;; a scant treble membership.
;; 
;; 
;; Concept
;; =======
;; The Kak programming language ostends a stark niggardliness in its
;; operational facilities, enumerated already by a treble account, for
;; the indagation and manipulation of an infinite tape of bits.
;; 
;; == AGNOMINATION ==
;; The nevening of the language as "Kak" translates to the Russian term
;; for "how".
;; 
;; == KAK OPERATES ON A BIT TAPE ==
;; Kak programs accomplish their tasks by inquiry and manipulation of a
;; tape of bits, infinite in its tally and enumerated starting with the
;; index one (1); the current storage unit of which, the cell, answers
;; to a cursor, the cell pointer.
;; 
;; == KAK'S INSTRUCTION SET: A CURTAILED SPECIMEN ==
;; Its composition proceeding from a trinity of participants only, Kak
;; can only flip the current bit's value and translate the cell pointer
;; in a gradual fashion. These twain of basic operations may, depending
;; upon the concrete case, either manifest as a epiphenomenon or a
;; conditional ultimity.
;; 
;; 
;; Architecture
;; ============
;; A Kak program operates on a theoretically infinitely large tape of
;; bit-valued cells, amenable to subscripts that, commencing with the
;; minimum index of one (1), are enumerated accordingly. At the
;; program's inchoation, every cell is initialized to zero (0).
;; 
;; A cell pointer, initially empighted on the first cell at the index
;; one, at any instant denotes the currently active member. Commands
;; exist for poco a poco sinistral and dextral translations.
;; 
;; 
;; Data Types
;; ==========
;; Kak's type system represents utter homogeneity by employing as its
;; singly expression bit values, that is, the integer zero (0) and one
;; (1).
;; 
;; 
;; Instructions
;; ============
;; Three members already exhaust Kak's instruction set, which offers,
;; beside the navigation across the tape and its rudimentary
;; manipulative warklumes, no further actuators.
;; 
;; == OVERVIEW ==
;; The following apercu shall entalent the reader with a cursory
;; apprehension anenst the language's available commands:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   !       | Moves the cell pointer one step to the right and flips
;;           | the bit at the new location.
;;   ..................................................................
;;   ?       | If the current cell value equals zero (0), skips the
;;           | immediately following instruction; aliter proceeds as
;;           | usual.
;;   ..................................................................
;;   <       | If the cell pointer's index is greater than one (1),
;;           | moves the cell pointer one step to the left; aliter
;;           | remains ineffectuous.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-13
;; 
;; Sources:
;;   [esolang2022Kak]
;;   The Esolang contributors, "Kak", 2022
;;   URL: "https://esolangs.org/wiki/Kak"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Kak (code)
  "Interprets the piece of Kak CODE and returns a bit vector
   representation of the internally managed tape."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((ip      0)
          (token   (char code 0))
          (memory  (make-array 1
                     :element-type    'bit
                     :adjustable      T
                     :fill-pointer    1
                     :initial-element 0))
          (pointer 0))
      (declare (type fixnum              ip))
      (declare (type (or null character) token))
      (declare (type bit-vector          memory))
      (declare (type fixnum              pointer))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the CODE, if possible, updates the current TOKEN, and
             returns no value."
            (setf token
              (when (array-in-bounds-p code (1+ ip))
                (char code (incf ip))))
            (values))
           
           (move-ip-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION in the
             CODE, updates the current TOKEN, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf token
              (when (array-in-bounds-p code ip)
                (char code ip)))
            (values))
           
           (command-character-p (character)
            "Checks whether the CHARACTER represents a command,
             returning on confirmation a ``boolean'' value of ``T'',
             otherwise ``NIL''."
            (declare (type character character))
            (the boolean
              (not (null (find character "!?<" :test #'char=)))))
           
           (active-cell ()
            "Returns the bit stored in the active cell."
            (the bit (bit memory pointer)))
           
           ((setf active-cell) (new-value)
            "Sets the bit in the active cell to the NEW-VALUE and
             returns no value."
            (declare (type bit new-value))
            (setf (bit memory pointer) new-value)
            (values))
           
           (move-pointer-right ()
            "Moves the memory POINTER one cell to the right and returns
             no value."
            (incf pointer)
            (when (>= pointer (length memory))
              (vector-push-extend 0 memory))
            (values))
           
           (move-pointer-left ()
            "Moves the memory POINTER one cell to the left, if not
             located in the first cell, and returns no value."
            (when (plusp pointer)
              (decf pointer))
            (values)))
        
        (loop do
          (case token
            ((NIL)
              (cond
                ;; Active cell contains a zero bit?
                ;; => Terminate the program.
                ((zerop (active-cell))
                  (loop-finish))
                ;; Active cell contains a one bit?
                ;; => Print the memory and repeat the program.
                (T
                  (print memory)
                  (move-ip-to 0))))
            
            (#\!
              (move-pointer-right)
              (setf (active-cell)
                    (- 1 (active-cell)))
              (advance))
            
            (#\?
              (cond
                ((zerop (active-cell))
                  (advance)
                  (loop
                    until (or (null token)
                              (command-character-p token))
                    do (advance))
                  (advance))
                (T
                  (advance))))
            
            (#\<
              (move-pointer-left)
              (advance))
            
            (otherwise
              (advance)))))
      
      (the bit-vector memory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text generator.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-string-bits (string)
  "Returns a simple bit vector containing the STRING's binary
   representation."
  (declare (type string string))
  (the simple-bit-vector
    (coerce
      (loop for character of-type character across string append
        (loop
          for bit-position of-type (integer -1 8) from 7 downto 0
          collect
            (ldb (byte 1 bit-position)
                 (char-code character))))
      'simple-bit-vector)))

;;; -------------------------------------------------------

(defun generate-Kak-text-program (text
                                  &key (destination T))
  "Generates the Kak program necessary to reproduce the TEXT in its
   binary form in the memory, writes the thus produced source code to
   the DESTINATION, and either returns ``NIL'' for a non-``NIL''
   DESTINATION or a fresh string containing the Kak code otherwise.
   ---
   As the Kak programming language does not permit the modification of
   the first bit, every memory output starts with a zero bit. For the
   sake of ascertaining the generated program's termination, a desinent
   zero bit is appended, even in the case of its natural state already
   accommodating for such."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((bits (get-string-bits text)))
        (declare (type simple-bit-vector bits))
        (flet ((append-zero-bit (destination)
                "Writes to the destination the instructions necessary to
                 append a zero-valued bit to its memory and returns no
                 value."
                (declare (type destination destination))
                (format destination "!<!")
                (values))
               
               (append-one-bit (destination)
                "Writes to the destination the instruction necessary to
                append a one-valued bit to its memory and returns no
                value."
                (declare (type destination destination))
                (format destination "!")
                (values)))
          (loop
            for bit of-type bit across bits
            do
              (case bit
                (0         (append-zero-bit destination))
                (1         (append-one-bit  destination))
                (otherwise (error "Invalid bit character: ~d." bit)))
            finally
              (append-zero-bit destination))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-Kak-text-program text :destination output)))))

;;; -------------------------------------------------------

(defun convert-bits-to-string (bits
                               &key (destination T)
                                    (start       1)
                                    (end         (1- (length bits))))
  "Starting at the element at the START position and ceasing before that
   at the END index, interprets each eight adjacent elements of the bit
   vector BITS as an ASCII character code, converts the same to a
   character, writes it to the DESTINATION, and returns either ``NIL''
   for a non-``NIL'' DESTINATION or a fresh string with the thus
   generated output if supplied with the ``NIL'' value.
   ---
   By default, the process starts with the second bit (START = 1) and
   ceases at the penultimate (END = length of BITS - 1), seeking
   conformance with the ``generate-Kak-text-program'' function's rule
   of normalizing the memory content by adhibition of one zero bit at
   each tape march."
  (declare (type bit-vector bits))
  (the (or null string)
    (if destination
      (loop for bit-position from start below end by 8 do
        (let ((character-code 0))
          (declare (type (unsigned-byte 8) character-code))
          (setf (ldb (byte 1 7) character-code)
                (bit bits (+ bit-position 0)))
          (setf (ldb (byte 1 6) character-code)
                (bit bits (+ bit-position 1)))
          (setf (ldb (byte 1 5) character-code)
                (bit bits (+ bit-position 2)))
          (setf (ldb (byte 1 4) character-code)
                (bit bits (+ bit-position 3)))
          (setf (ldb (byte 1 3) character-code)
                (bit bits (+ bit-position 4)))
          (setf (ldb (byte 1 2) character-code)
                (bit bits (+ bit-position 5)))
          (setf (ldb (byte 1 1) character-code)
                (bit bits (+ bit-position 6)))
          (setf (ldb (byte 1 0) character-code)
                (bit bits (+ bit-position 7)))
          (format destination "~a" (code-char character-code))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-bits-to-string bits :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinite loop.
(interpret-Kak "!?")

;;; -------------------------------------------------------

;; Otiose attempt at moving left when being located in the first cell.
(interpret-Kak "<")

;;; -------------------------------------------------------

;; Reproduce in the memory the bits for the ASCII character codes of the
;; text "Hello, World!", on each end of the occupied tape section
;; surrounded by a zero bit, the latter of which ascertains the
;; program's termination.
(interpret-Kak "!!<!!<!!!<!!<!!<!!<!!!!<!!<!!!<!!!<!!!!<!!!!<!!<!!<!!!!<!!!!<!!<!!<!!!!<!!!!!!<!!<!!!<!!!!<!!<!!<!!<!!!<!!<!!<!!<!!<!!<!!!<!!!<!!!!!<!!!!<!!!!!!<!!!!!<!!<!!!<!!<!!!!<!!!!<!!<!!<!!!!<!!<!!!<!!<!!<!!<!!!<!!<!!<!!<!!!<!")

;;; -------------------------------------------------------

;; Produce and write to the standard output the Kak code requisite for
;; generating the bits of the text "Hello, World!" in the program
;; memory:
;;   !<!!!<!!<!!!<!!<!!<!!<!!!!<!!<!!!<!!!<!!!!<!!!!<!!<!!<!!!!<!!!!<!
;;   !<!!<!!!!<!!!!!!<!!<!!!<!!!!<!!<!!<!!<!!!<!!<!!<!!<!!<!!<!!!<!!!<!
;;   !!!!<!!!!<!!!!!!<!!!!!<!!<!!!<!!<!!!!<!!!!<!!<!!<!!!!<!!<!!!<!!<!
;;   !<!!<!!!<!!<!!<!!<!!!<!
(generate-Kak-text-program "Hello, World!")

;;; -------------------------------------------------------

;; Produce and write to a fresh string the Kak code requisite for
;; generating the bits of the text "Hello, World!" in the program
;; memory:
;;   !<!!!<!!<!!!<!!<!!<!!<!!!!<!!<!!!<!!!<!!!!<!!!!<!!<!!<!!!!<!!!!<!
;;   !<!!<!!!!<!!!!!!<!!<!!!<!!!!<!!<!!<!!<!!!<!!<!!<!!<!!<!!<!!!<!!!<!
;;   !!!!<!!!!<!!!!!!<!!!!!<!!<!!!<!!<!!!!<!!!!<!!<!!<!!!!<!!<!!!<!!<!
;;   !<!!<!!!<!!<!!<!!<!!!<!
;; Then interpret the same using the Kak interpreter, and print the
;; resulting bit vector as a string, restoring the original message.
(convert-bits-to-string
  (interpret-Kak
    (generate-Kak-text-program "Hello, World!" :destination NIL)))

;;; -------------------------------------------------------

;; Convert the normalized bit representation of the text "Hello, World!"
;; into a string. Please note that an adscititious zero-valued bit is
;; prepended and appended to the sequence.
(convert-bits-to-string #*0010010000110010101101100011011000110111100101100001000000101011101101111011100100110110001100100001000010)

;;; -------------------------------------------------------

;; Generate in the Kak program memory the bit representation of the
;; ASCII text "Hello, World!" and convert it into the text string.
;; Please note that the resulting bit vector does not carry the
;; normalizing incipient and desinent zero bits; thus, the conversion
;; must start at the first cell.
(convert-bits-to-string
  (interpret-Kak "!!<!!<!!!<!!<!!<!!<!!!!<!!<!!!<!!!<!!!!<!!!!<!!<!!<!!!!<!!!!<!!<!!<!!!!<!!!!!!<!!<!!!<!!!!<!!<!!<!!<!!!<!!<!!<!!<!!<!!<!!!<!!!<!!!!!<!!!!<!!!!!!<!!!!!<!!<!!!<!!<!!!!<!!!!<!!<!!<!!!!<!!<!!!<!!<!!<!!<!!!<!!<!!<!!<!!!<!")
  :start 0)
