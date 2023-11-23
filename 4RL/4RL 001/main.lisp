;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "4RL", invented by the Esolang user "Frogstair" in the year
;; 2020 and presented on April 17th, 2020, the entheus of which relates
;; to Urban Mueller's "brainfuck", modulated, however, by a new
;; provenance as the symbols' donors, reproducing the phrase "4 real",
;; while, as a concomitant, the stock-father's octuple instruction set
;; enjoys an augmentation by an aefauld command dedicated to the
;; program's premature termination.
;; 
;; 
;; Concept
;; =======
;; The 4RL programming language defines a brainfuck derivative diverging
;; from the original by a twain of instances, namely, the identifier
;; tokens and an additional command utilized for halting the program.
;; 
;; == 4RL: "FOR REAL" ==
;; The 4RL language's agnomination replicates in its constituents the
;; phrase "for real".
;; 
;; == 4RL: A NEW DICTION FOR A PROVEN CONCEPT ==
;; For the preponderance of its haecceity it accounts that 4RL
;; establishes a verbatim appropriation of its brainfuck inspiration,
;; altered merely in the identifiers associated with the octuple
;; instruction set. A kenspeckle enhancement's installation, an
;; adscititious operation for the immediate program termination has been
;; vouchsafed.
;; 
;; == THE MEMORY: 65536 UNSIGNED BYTES ==
;; As counterdistinguished from Urban Mueller's incipient brainfuck
;; creation, 4RL relies on 2^16, that is, 65536, cells of unsigned
;; bytes, rather than a mere 30000.
;; 
;; 
;; Architecture
;; ============
;; 4RL's memory enumerates a tally of 2^16 (= 65536) cells, each such an
;; unsigned byte scalar's salvatory. Alteration which carry the cell
;; value beyond the admissible bournes of [0, 255] instigate a wrapping
;; around to the obverse extremum.
;; 
;; At any instant during the program's
;; execution, the cell pointer references the currently active member,
;; the aefauld entity entalented with an amenability to perquisitions
;; and modifications.
;; 
;; 
;; Instructions
;; ============
;; Acquring the brainfuck cleronomy as its substrate, 4RL ostends in its
;; foundational status the octuple instruction set, reformulated merely
;; by a new mode of diction. An expression of the language's
;; supererogation, a ninth member, ordained to the wike of a program's
;; immediate conclusion, is extended.
;; 
;; == OVERVIEW ==
;; A cursory species of gnarity's communication anenst 4RL's operative
;; competences shall limn the following apercu's dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   4       | Increments the current cell value by one (1).
;;           | If the new cell value transcends the upper march of 255,
;;           | its state is reset to the minimum of zero (0).
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "+".
;;   ..................................................................
;;   r       | Decrements the current cell value by one (1).
;;           | If the new cell value violates the lower march of zero
;;           | (0), its state wraps around to the maximum of 255.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "-".
;;   ..................................................................
;;   e       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ">".
;;   ..................................................................
;;   a       | Translates the cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "<".
;;   ..................................................................
;;   R       | Queries the standard input for a character and stores
;;           | its ASCII code in the current cell.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ",".
;;   ..................................................................
;;   E       | Prints the character whose ASCII code equals the current
;;           | cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's ".".
;;   ..................................................................
;;   l       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matcing "L" command.
;;           | Otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "[".
;;   ..................................................................
;;   L       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "l" command.
;;           | Otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This operation corresponds to brainfuck's "]".
;;   ..................................................................
;;   A       | Terminates the program with immediate effect.
;;           |---------------------------------------------------------
;;           | This operation does not correspond to any capability
;;           | inherent to brainfuck.
;;   ------------------------------------------------------------------
;; 
;; == 4RL TRANSLATES TO BRAINFUCK ==
;; The consanguinity partaken of by 4RL and its stock-father brainfuck
;; homologates, in a preponderance of its aspects, an equiparation
;; betwixt the cognates:
;; 
;;   ---------------
;;   4RL | brainfuck
;;   ----+----------
;;   4   | +
;;   ...............
;;   r   | -
;;   ...............
;;   e   | >
;;   ...............
;;   a   | <
;;   ...............
;;   R   | ,
;;   ...............
;;   E   | .
;;   ...............
;;   l   | [
;;   ...............
;;   L   | ]
;;   ...............
;;   A   | (None.)
;;   ---------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-11-23
;; 
;; Sources:
;;   [esolang2022_4RL]
;;   The Esolang contributors, "4RL", May 21st, 2022
;;   URL: "https://esolangs.org/wiki/4RL"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, which, as a corollary, occupies the integral range
   spanning [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-array ()
  "The ``cell-array'' type defines the program memory's cells in terms
   of a one-dimensional simple array composed of 2^16 (= 65536) unsigned
   byte-valued cells."
  '(simple-array octet (65536)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which answers to the ELEMENT-TYPE, its
   default specified by the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type '*)
                                        (value-type     '*))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each key, or indicator, of
   which conforms to the INDICATOR-TYPE and associates with a value of
   the VALUE-TYPE, both defaulting to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element `(cons ,indicator-type ,value-type)))
              (the list candidate)))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Table
  (:constructor make-empty-jump-table ()))
  "The ``Jump-Table'' class connects the forward jump and back jump
   commands in a 4RL program by mediation of their positions inside the
   same's instruction sequence."
  (connections NIL :type (association-list-of fixnum fixnum)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table start-point end-point)
  "Connects the jump START-POINT and END-POINT in a bilateral fashion
   in the JUMP-TABLE and returns no value.."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (push
    (cons start-point end-point)
    (jump-table-connections jump-table))
  (push
    (cons end-point start-point)
    (jump-table-connections jump-table))
  (values))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table source-position)
  "Returns the position of the jump point obverse to the SOURCE-POSITION
   in the JUMP-TABLE, or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source-position))
  (the fixnum
    (or (cdr
          (assoc source-position
            (jump-table-connections jump-table)
            :test #'=))
        (error "No destination associated with the jump point ~d."
          source-position))))

;;; -------------------------------------------------------

(defun compute-jump-table (code)
  "Computes and returns for the piece of 4RL source CODE a jump table
   which maps the forward and back jump points in a bidirectional
   manner."
  (declare (type string code))
  (let ((jump-table   (make-empty-jump-table))
        (start-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for token    of-type character across code
      and position of-type fixnum    from 0 by 1
      if (char= token #\l) do
        (push position start-points)
      else if (char= token #\L) do
        (if start-points
          (connect-jump-points jump-table
            (pop start-points)
            position)
          (error "Unmatched back jump at position ~d." position))
      end
      finally
        (when start-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points)
            (nreverse start-points))))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class models the 4RL program memory as a vector
   enumerating 2^16 (= 65536) unsigned byte-valued cells, operated upon
   by a mobile cell pointer, the onus of which wones in the currently
   active cell's castaldy."
  (cells   (make-array 65536
             :element-type   'octet
             :initial-element 0
             :adjustable      NIL
             :fill-pointer    NIL)
           :type (simple-array octet (65536)))
  (pointer 0
           :type integer))

;;; -------------------------------------------------------

(defun current-cell (memory)
  "Returns the current MEMORY cell's byte value."
  (declare (type Memory memory))
  (the octet
    (aref
      (memory-cells   memory)
      (memory-pointer memory))))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceding the transfer by a wrapping of the argument into the valid
   unsigned byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (aref
      (memory-cells   memory)
      (memory-pointer memory))
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-4RL (code)
  "Interprets the piece of 4RL source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (compute-jump-table code))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\4 (incf (current-cell   memory)))
        (#\r (decf (current-cell   memory)))
        (#\e (incf (memory-pointer memory)))
        (#\a (decf (memory-pointer memory)))
        (#\R (format T "~&>> ")
             (finish-output)
             (setf (current-cell memory)
               (char-code
                 (read-char)))
             (clear-input))
        (#\E (write-char
               (code-char
                 (current-cell memory))))
        (#\l (when (zerop (current-cell memory))
               (setf ip
                 (get-jump-destination jump-table ip))))
        (#\L (unless (zerop (current-cell memory))
               (setf ip
                 (get-jump-destination jump-table ip))))
        (#\A (loop-finish))
        (otherwise
          NIL))
      (incf ip)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!".
(interpret-4RL "44444444le4444le44e444e444e4aaaarLe4e4eree4laLarLeeEerrrE4444444EE444EeeEarEaE444ErrrrrrErrrrrrrrEee4Ee44E")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a user input of the
;; "null character" (ASCII code of zero (0)).
(interpret-4RL "RlERL")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-4RL "RElrre4leeLalELaaL")

;;; -------------------------------------------------------

;; Demonstrate the novel program termination command "A" by printing the
;; letter "B" once, and terminating ere a second output could be
;; produced.
(interpret-4RL
  "444444444444444444444444444444444444444444444444444444444444444444
   EAE")
