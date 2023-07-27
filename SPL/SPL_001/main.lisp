;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "SPL", invented by the Esolang user "StelK" in the year
;; 2005, designed as an extension of Urban Mueller's "brainfuck" in
;; terms of its architecture, by an adminicular accumulator's
;; introduction, as well as variations on extant and supplementation of
;; new commands.
;; 
;; 
;; Conept
;; ======
;; A language perambulating along brainfuck's rut, while augmenting its
;; inspiration's facilities, SPL employs a preponderance of
;; single-character commands, complemented by an arbitrary-length string
;; literal syntax, in its pursuit to manipulate an infinite tape of
;; unsigned bytes and an accumulator of a scalar element from the same
;; set.
;; 
;; == SPL: A BRAINFUCK ENHANCEMENT ==
;; The SPL programming language exists as an extension of the very
;; simplistic entheus brainfuck, enhancing the conceptual ancestor's
;; octuple instruction set by a variety of utile competences, while
;; concomitantly ordaining the services of a scalar byte accumulator as
;; a compernage to the also acquired infinite octet tape.
;; 
;; == AN EQUIPARATION OF SPL AND BRAINFUCK ==
;; A cursory tabular illustration shall serve to juxtapose the basic
;; properties of SPL and brainfuck.
;; 
;;   ------------------------------------------------------------------
;;   Subject      | Equiparation
;;   -------------+----------------------------------------------------
;;   Memory       | In SPL the infinite tape of unsigned bytes acts in
;;                | coefficacy with an octet-valued accumulator.
;;                |----------------------------------------------------
;;                | brainfuck accommodates exclusively the first moeity
;;                | via the tape-like byte sequence.
;;   ..................................................................
;;   Arithmetics  | SPL extends the tape's incrementing and
;;                | decrementing commands with a set of binary
;;                | operations operating in champarty with the
;;                | accumulator.
;;                |----------------------------------------------------
;;                | brainfuck abstains from such supererogation,
;;                | embracing only the gradual augmentation and
;;                | deduction instructions.
;;   ..................................................................
;;   Control flow | Both languages are paregal in regards to the
;;                | control flow concept, differing, however, in their
;;                | activation criteria.
;;                | SPL employs for such purpose its dioristic
;;                | accumulator state.
;;                |----------------------------------------------------
;;                | brainfuck relies on the current cell value in order
;;                | to sanction or disregard the jumping facility.
;;   ..................................................................
;;   Input/Ouput  | SPL employs only integers in the reception of
;;                | input and the display of output.
;;                | Input can be committed to the tape or the
;;                | accumulator; output only issues from the former
;;                | provenance.
;;                |----------------------------------------------------
;;                | brainfuck queries for ASCII characters and prints
;;                | its output in the same format, its reliance
;;                | exhausted by the tape-like memory.
;;   ..................................................................
;;   Comments     | SPL introduces a dedicated syntax for the
;;                | accommodation of descants; forinsecal tokens
;;                | exterior to this section are prohibited.
;;                |----------------------------------------------------
;;                | brainfuck only recognizes command tokens, ignoring
;;                | any other characters, which, in consectary, may be
;;                | reappropriated for commentary purposes.
;;   ------------------------------------------------------------------
;; 
;; 
;; Architecture
;; ============
;; SPL's architecture employs a bivial structuring of data castaldy
;; agents, imprimis a bilaterally infinite tape composed of cells that
;; store an unsigned byte each, amenable to their active instance's
;; selection by a cell pointer. An adminiculum of this entirety, an
;; accumulator endowed with a scalar octet's capacity partakes of a
;; parhedral duty.
;; 
;; 
;; Data Types
;; ==========
;; An aefauld token of currency is admitted to the language in the form
;; of the unsigned byte species, one whose woning, as a consectary,
;; constitutes a tantamount of the closed integer range [0, 255].
;; 
;; A counterdistinguishing emblem, brainfuck's parergetic deployment of
;; characters for the input/output intercourse designate a lacuna in
;; SPL.
;; 
;; 
;; Instructions
;; ============
;; SPL's instruction tallies eighteen members, an octuple allotment of
;; which constituting an acquisition from its brainfuck cleronomy,
;; partially being accommodated for the divergence in the recipient's
;; diorism.
;; 
;; == OVERVIEW ==
;; A cursory ilk of nortelry anenst the language's instruction set shall
;; be adhibited in the following tabular illustration.
;; 
;; Please note that variable portions, intended to be substituted by
;; actual SPL code, are underlined with asterisks ("*"); any other
;; content's appropriation is intended to be retained ipsissima verba.
;; 
;;   ------------------------------------------------------------------
;;   Command   | Effect
;;   ----------+-------------------------------------------------------
;;   >         | Moves the cell pointer one cell to the right.
;;             | This command constitutes an autochthonous brainfuck
;;             | member.
;;   ..................................................................
;;   <         | Moves the cell pointer one cell to the left.
;;             | This command constitutes an autochthonous brainfuck
;;             | member.
;;   ..................................................................
;;   +         | Increments the current cell value by one.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an autochthonous brainfuck
;;             | member.
;;   ..................................................................
;;   -         | Decrements the current cell value by one.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an autochthonous brainfuck
;;             | member.
;;   ..................................................................
;;   .         | Prints the curent cell value in its verbatim numeric
;;             | form to the standard output.
;;             | This command constitutes a variation on an extant
;;             | brainfuck member.
;;   ..................................................................
;;   ,         | Queries the user for an integer number and stores the
;;             | same in the current cell.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the input is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes a variation on an extant
;;             | brainfuck member.
;;   ..................................................................
;;   [         | If the accumulator equals zero (0), relocates the
;;             | instruction pointer (IP) to the position immediately
;;             | succeeding the matching "]" command. Otherwise
;;             | proceeds as usual.
;;             | This command constitutes a variation on an extant
;;             | brainfuck member.
;;   ..................................................................
;;   ]         | If the accumulator does not equal zero (0), relocates
;;             | the instruction pointer (IP) to the position
;;             | immediately succeeding the matching "]" command.
;;             | Otherwise proceeds as usual.
;;             | This command constitutes a variation on an extant
;;             | brainfuck member.
;;   ==================================================================
;;   ^         | Copies the current cell value into the accumulator.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   $         | Queries the standard input for an integer value and
;;             | stores it in the accumulator.
;;             | If violating the accumulator capacity defined by the
;;             | byte range [0, 255], the input is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   %         | Divides the current cell value by the accumulator and
;;             | stores the remainder (modulus) in the current cell.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   m         | Multiplies the current cell value by the accumulator
;;             | and stores the product in the current cell.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   a         | Increments the current cell value by the accumulator
;;             | and stores the result in the current cell.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   s         | Reduces the current cell value by the accumulator and
;;             | stores the result in the current cell.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   d         | Divides the current cell value by the accumulator and
;;             | stores the quotient in the current cell.
;;             | If violating the cell capacity defined by the byte
;;             | range [0, 255], the new value is wrapped around to
;;             | accommodate the constraint.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   &         | Terminates the program immediately.
;;             | This command constitutes an advenient addition.
;;   ..................................................................
;;   "text"    | Prints the {text} message to the standard output.
;;    ****     | This command constitutes an advenient addition.
;;   ..................................................................
;;   #comment# | Defines a comment containing the {COMMENT} message.
;;    *******  | Comments cannot be nested.
;;             | This command constitutes an advenient addition.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; SPL's modesty in the exposition of information, as well as the fact
;; of its original implementation's deprivation, encroaches the protolog
;; with a number of incertitudes, a subset of which shall be the coming
;; sections' cynosure.
;; 
;; == HOW DO THE MEMORY CELLS RESPOND TO BOUNDARY TRANSGRESSIONS? ==
;; Whereas the memory cells' capacity must be elicited by derivation
;; from the language standard's instruction table perusal, appertaining
;; to unsigned bytes in the range [0, 255], the deportment in relation
;; to a transgression of the lower or upper extremum registers the
;; necessity of personal effort for an eisegesis' obtention.
;; 
;; It has been adjudged, proceeding from one of brainfuck's widespread
;; conceptions, to postulate an amenability to value wrapping upon
;; overflows: If a cell's value descends alow the minimum of zero (0),
;; it is wrapped around to the maximum of 255; in a fashion similiter
;; the transcendence aboon the upper bourne of 255 resumes the
;; incrementation at the lower march of zero (0).
;; 
;; == WHICH DATA TYPE DOES THE ACCUMULATOR ASSUME? ==
;; SPL's official description relates of the only admissive type in the
;; language as the integer species; concomitantly, the memory cells are
;; always mentioned in a vinculum with byte values. The accumulator, on
;; the other hand, does not treat of this explicit mete of patration.
;; 
;; It has been adjudged to restrict the accumulator to an unsigned byte
;; scalar, in corollary partaking of a consanguinity with the memory
;; items by its restriction to the integer range [0, 255]. Additionally,
;; the accumulator mimics the cell value's wrapping on overflow
;; principle, relapsing upon a descent below the minimum of zero (0) to
;; the maximum of 255, and, siclike, resuming the augmentation beyond
;; the upper bourne of 255 at the lower extremum of zero (0).
;; 
;; == ARE NON-COMMAND CHARACTERS HOMOLOGATED? ==
;; SPL's expressed similitude to brainfuck redes the question's issuing
;; whether, as in the referenced language, tokens not identified as
;; command names are ignored, acquiring the contingency of comments. The
;; SPL specification does not explicitly answer to this subject,
;; natheless, a dedicated comment syntax exists.
;; 
;; It has been adjudged to apply to any non-whitespace character that
;; cannot be construed as part of the SPL language's syntaxis an
;; imposition as interdicted, empighting the etiology of an error of an
;; unspecified type.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has materialized in the programming
;; language Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date: 2022-03-25
;; 
;; Sources:
;;   [esolang2020SPL]
;;   The Esolang contributors, "SPL", 2020
;;   URL: "https://esolangs.org/wiki/SPL"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits,
   which renders its commorancy the closed integer range [0, 255].
   ---
   Objects desumed from this species are employed in the memory cells
   and the accumulator."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the SPL program memory, conceptually a
   bilaterally extent of infinitely many unsigned-byte-valued cells, as
   a hash table which associates with each integer cell index an octet
   datum."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized directions for
   traversing a piece of SPL code.
   ---
   This dichotomy imposes a requirement in the context of forward and
   back jump operations, as string literal and comment sections may be
   skipped in both sinistrodextral and dextrosinistral airts, any of
   these necessitate an accommodated handling policy."
  '(member :forward :backward))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-SPL (code)
  "Interprets the piece of SPL CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position    0)
          (character   (char code 0))
          (memory      (make-hash-table :test #'eql))
          (pointer     0)
          (accumulator 0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type memory              memory))
      (declare (type integer             pointer))
      (declare (type octet               accumulator))
      
      (labels
          ((advance ()
            "Moves the POSITION cursor to the next character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION cursor to the previous character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (array-in-bounds-p code (1- position))
                (char code (decf position))))
            (values))
           
           (skip-comment (direction)
            "Starting at the current POSITION and expecting to be
             commorant at the start of a comment, skips the comment
             while moving in the DIRECTION, relocates the POSITION
             cursor to the first character following the comment
             portion, and returns no value."
            (declare (type direction direction))
            (case direction
              (:forward
                (advance)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated comment."))
                    (#\#
                      (advance)
                      (loop-finish))
                    (otherwise
                      (advance)))))
              (:backward
                (recede)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated comment."))
                    (#\#
                      (recede)
                      (loop-finish))
                    (otherwise
                      (recede)))))
              (otherwise
                (error "Invalid direction: ~s." direction)))
            (values))
           
           (skip-string (direction)
            "Starting at the current POSITION and expecting to be
             commorant at the start of a string, skips the string while
             moving in the DIRECTION, relocates the POSITION cursor to
             the first character following the string portion portion,
             and returns no value."
            (declare (type direction direction))
            (case direction
              (:forward
                (advance)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (advance)
                      (loop-finish))
                    (otherwise
                      (advance)))))
              (:backward
                (recede)
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (recede)
                      (loop-finish))
                    (otherwise
                      (recede)))))
              (otherwise
                (error "Invalid direction: ~s." direction)))
            (values))
           
           (read-string ()
            "Starting at the current POSITION and expecting to be
             commorant at the start of a string, consumes and returns
             its content, relocating the POSITION cursor to the
             character immediately following the terminating quote."
            (advance)
            (the string
              (with-output-to-string (characters)
                (declare (type string-stream characters))
                (loop do
                  (case character
                    ((NIL)
                      (error "Unterminated string."))
                    (#\"
                      (advance)
                      (loop-finish))
                    (otherwise
                      (write-char character characters)
                      (advance)))))))
           
           (prompt-input ()
            "Reads from the standard input an integer input and returns
             the same."
            (the integer
              (loop
                for     input of-type T = (read)
                until   (integerp input)
                finally (return input))))
           
           (current-cell ()
            "Returns the current cell's byte value."
            (the octet
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE, contingently wrapped around to match
             the unsigned byte range of [0, 255], in the current cell
             and returns no value."
            (declare (type integer new-value))
            (setf (gethash pointer memory 0)
                  (mod new-value 256))
            (values)))
        
        (loop do
          (case character
            ;; End of program. => Terminate program.
            ((NIL)
              (loop-finish))
            
            ;; Skip whitespaces.
            ((#\Newline #\Space #\Tab)
              (advance))
            
            ;; Move the memory pointer one cell to the right.
            (#\>
              (incf pointer)
              (advance))
            
            ;; Move the memory pointer one cell to the left.
            (#\<
              (decf pointer)
              (advance))
            
            ;; Output the current cell's integer value.
            (#\.
              (format T "~d"
                (current-cell))
              (advance))
            
            ;; Input an integer and store it in the current cell.
            (#\,
              (let ((input (prompt-input)))
                (declare (type integer input))
                (clear-input)
                (setf (current-cell) input))
              (advance))
            
            ;; Jump past the matching "]" if the accumulator is zero.
            (#\[
              (cond
                ((zerop accumulator)
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated '['."))
                      (#\[
                        (incf level)
                        (advance))
                      (#\]
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (#\"
                        (skip-string :forward))
                      (#\#
                        (skip-comment :forward))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Jump back past the matching "[" if the accumulator does
            ;; not equal zero.
            (#\]
              (cond
                ((zerop accumulator)
                  (advance))
                (T
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated ']'."))
                      (#\]
                        (incf level)
                        (recede))
                      (#\[
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#\"
                        (skip-string :backward))
                      (#\#
                        (skip-comment :backward))
                      (otherwise
                        (recede)))))))
            
            ;; Increase the current cell value by one.
            (#\+
              (incf (current-cell))
              (advance))
            
            ;; Decrease the current cell value by one.
            (#\-
              (decf (current-cell))
              (advance))
            
            ;; Load the current cell value into the accumulator.
            (#\^
              (setf accumulator (current-cell))
              (advance))
            
            ;; Store the user input in the accumulator.
            (#\$
              (let ((input (prompt-input)))
                (declare (type integer input))
                (clear-input)
                (setf accumulator
                      (mod input 256)))
              (advance))
            
            ;; Set the current cell value to its remainder when divided
            ;; by the accumulator.
            (#\%
              (setf (current-cell)
                    (rem (current-cell) accumulator))
              (advance))
            
            ;; Multiply the current cell value by the accumulator.
            (#\m
              (setf (current-cell)
                    (* (current-cell) accumulator))
              (advance))
            
            ;; Add the accumulator to the current cell value.
            (#\a
              (incf (current-cell) accumulator)
              (advance))
            
            ;; Subtract the accumulator from the current cell value.
            (#\s
              (decf (current-cell) accumulator)
              (advance))
            
            ;; Divide the current cell value by the accumulator.
            (#\d
              (setf (current-cell)
                    (round (current-cell)
                           accumulator))
              (advance))
            
            ;; Terminate the program.
            (#\&
              (loop-finish))
            
            ;; Print a string literal.
            (#\"
              (let ((string (read-string)))
                (declare (type string string))
                (format T "~a" string)))
            
            ;; Skip a comment.
            (#\#
              (skip-comment :forward))
            
            (otherwise
              (error "Invalid character '~a' at position ~d."
                character position)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World", followed by a linebreak.
(interpret-SPL "\"Hello World
\"")

;;; -------------------------------------------------------

;; Ask the user for a number and print the numbers counting down to
;; zero.
(interpret-SPL "\"n: \",^[.\" \"-^].\"
\"")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-SPL ",.^[\"
\",.^]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-SPL ",.^[.]")

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-SPL
"
# Please note that the initial tally of bottles is set to 97 instead   #
# of 99 because of the necessity to terminate the accumulator-based    #
# iteration before reaching the stanza relating of two (2) bottles     #
# of beer. This requisitum is founded upon the desire to print         #
# following the correct orthography of the singular \"bottle\" in lieu #
# of the unconditional usance of \"bottles\".                          #
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++++

^
[
++

.\" bottles of beer on the wall.
\"
. \" bottles of beer.
\"
\"Take one down, pass it around,
\"
-
. \" bottles of beer on the wall.

\"

--
^
]

\"2 bottles of beer on the wall.
2 bottles of beer.
Take one down, pass it around,
1 bottle of beer on the wall.

1 bottle of beer on the wall.
1 bottle of beer.
Take one down, pass it around,
No bottles of beer on the wall.
\"
")
