;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Addbig", invented by the Esolang user "Joaozin003" and
;; presented on September 7th, 2022, the kenspeckle attribute of which,
;; subsuming into the species of "one instruction set computers" (OISC),
;; resides in its conflation of instructions and data in the memory,
;; with the operative moeity, expecting a triple of operands, based upon
;; the augmentation of the first cell by the second one, and the
;; conditional relocation of the program counter (PC) to the position
;; specified in the third argument, if the incremented cell value
;; exceeds the threshold of zero (0).
;; 
;; 
;; Concept
;; =======
;; The Addbig programming language subsumes into the OISC ("one
;; instruction set computer") ilk, appropriating both its operations and
;; data from a memory by selecting a triple of consecutive integers,
;; (a, b, c), from this provenance, incrementing the memory cell at the
;; location "a" by the value of the cell at the address "b", and, if the
;; new value of the cell at "a" is greater than zero (0), relocating the
;; program to the memory address "c"; thus ultimately applying the rule
;; 
;;   memory(a) <- memory(a) + memory(b)
;;   
;;   if memory(a) > 0 then
;;     goto(c)
;;   end if
;; 
;; == [Add][big]: [ADD] AND JUMP IF [BIG]GER THAN ZERO ==
;; The agnomination apportioned to the Addbig language involves an
;; allusion to the rather well-known extant companion "Subleq", which,
;; succeeding a cell twain's subtraction, activates the goto facility
;; if the difference is less than or equal to zero (0) --- Addbig's
;; addition and greater-than antecedent relocates it to the overthwart
;; laterality of the same foundational principle.
;; 
;; == ADDBIG: AN OISC --- ONE INSTRUCTION, SEVERAL REACTIONS ==
;; The Addbig programming language's notion is desumed from the
;; "one instruction set computer" (OISC) realm, the proprium acquired by
;; such species of supputating machine wones in the accommodation of an
;; aefauld instruction only for a contingent multitude of causata,
;; the tally and or actual arguments to this singular routine elicits
;; the diversity of the system's response.
;; 
;; In the case of Addbig, the participation in an operative context is
;; established in a fixed manner for a triple of signed integer
;; operands, norned usually "a", "b", "c", whence is begotten the
;; mathematical formulation as a 3-tuple (a, b, c).
;; 
;; The central component to this mete of compendiousness is furnished by
;; the program memory, a points of confluency betwixt both instructions
;; and data.
;; 
;; == ADDBIG'S MEMORY CONTAINS BOTH THE INSTRUCTIONS AND DATA ==
;; The Addbig programming language's execution principle founds upon the
;; conflation of its usually stringently segregated program memory's
;; bailiwicks twissel, the instruction sequence on one hand, the data
;; castaldy on the other, into a singular salvatory.
;; 
;; == THE MEMORY: A UNILATERAL INFINITE VECTOR OF SIGNED INTEGERS ==
;; This exceptionally competent object's diorism proceeds from a vector
;; of signed integer-valued cells, indexed commencing with the subscript
;; zero (0) and advancing towards a hypothetically infinite dispansion
;; along the positive axis. The cells, siclike, do not wist of any
;; impediment in their polarity nor the magnitude.
;; 
;; == INSTRUCTIONS OCCUPY A TRIAD OF MEMORY CELLS ==
;; Empight on the first memory address zero (0), the program counter
;; (PC), also nevened the instruction pointer (IP), instigates each
;; cycle's efficacy by selecting the catena of the three consecutive
;; cell items, in this language's context consistently acquiring the
;; agnomination "a", "b", and "c", and thus forming a triple (a, b, c).
;; As a variable of the program counter position "pc", the arguments
;; thus assume:
;; 
;;   --------------------------------
;;   Argument name | Memory location
;;   --------------+-----------------
;;   a             | memory(pc + 0)
;;   ................................
;;   b             | memory(pc + 1)
;;   ................................
;;   c             | memory(pc + 2)
;;   --------------------------------
;; 
;; == MEMORY-MAPPED I/O: INPUT AND OUTPUT RESIDE IN THE MEMORY ==
;; An essential ramification of the memory-mapped input/output principle
;; appertains to the requisitum of these conduit's addressing. In the
;; Addbig programming language, the respective memory location reserved
;; for both purposes constitutes the physically impossible, but
;; virtually conceited subscript negative subscripts starting at
;; inclusive -1.
;; 
;; Given the argument triplet (a, b, c), covering the subsequent memory
;; addressed commencing at the program counter (PC) position, the first
;; operand a, if assuming this negative sentinel responds with an output
;; of the character whose ASCII code answers to the memory element
;; memory(a).
;; 
;; The second operand, b, committed in a negative mode supersedes the
;; actual subtrahend, memory(b), by the user input character's ASCII
;; code.
;; 
;; A parlecue's adhibition shall entalent the treatise on the negative
;; address specifier's agency with a more lucrative lucidity:
;; 
;;   ------------------------------------------------------------------
;;   Parameter | Effect if negative
;;   ----------+-------------------------------------------------------
;;   a         | Prints the character whose ASCII code equals the
;;             | value of the memory cell memory(a) to the standard
;;             | output. The value at memory(a) is subsequently reset
;;             | to the initial state of zero (0).
;;   ..................................................................
;;   b         | Queries the standard input for a character and stores
;;             | its ASCII code in the memory cell memory(b).
;;   ..................................................................
;;   c         | If a goto relocation shall apply, immediately
;;             | terminates the program.
;;   ------------------------------------------------------------------
;; 
;; == THE ADDBIG OPERATION ALGORITHM ==
;; The adscititious convolutions inflicted upon the language by the
;; participation of sentinels for input and output facilities tholes the
;; wite of begetting a more intricate algorithm for an Addbig program's
;; execution.
;; 
;; The following pseudocode formulation shall elucidate the concept with
;; amplified stringency in its diction, while imparting the requisite
;; gnarity anenst the operation's entirety:
;; 
;;   procedure execute (memory)
;;     Input:
;;       memory: The program memory as a zero-indexed, infinite vector
;;               of signed integer cells, encompassing both the
;;               instructions and the data.
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       { The program counter (PC) or instruction pointer (IP), which }
;;       { designates the position of the next instruction triple      }
;;       { (a, b, c) in the memory to process.                         }
;;       let pc <- 0
;;       
;;       repeat
;;         let a   <- memory(pc)
;;         let b   <- memory(pc + 1)
;;         let c   <- memory(pc + 2)
;;         let sum <- 0
;;         
;;         { Parameter b is negative?         }
;;         { => Increment memory(a) by input. }
;;         if (b < 0) then
;;           let inputCharacter <- readCharacter()
;;           let inputASCIICode <- getCharacterCode(inputCharacter)
;;           memory(a) <- memory(a) + inputASCIICode
;;         { Parameter b is zero or positive?     }
;;         { => Increment memory(a) by memory(b). }
;;         else
;;           memory(a) <- memory(a) + memory(b)
;;         end if
;;         
;;         sum <- memory(a)
;;         
;;         { Parameter a is negative? => Output memory(a). }
;;         if (a < 0) then
;;           let outputCharacter <- getCharacterByCode(memory(a))
;;           print(outputCharacter)
;;           memory(a) <- 0
;;         end if
;;         
;;         { Cell at address a is positive? => Goto or terminate. }
;;         if (sum > 0) then
;;           { Jumping to a negative address terminates the program. }
;;           if (c < 0)  then
;;             terminate program
;;           else
;;             goto(c)
;;           end if
;;         { Cell at address a is negative?         }
;;         { => Advance to next instruction triple. }
;;         else
;;           pc <- pc + 3
;;         end if
;;       end repeat
;;   end procedure
;; 
;; 
;; Implementation
;; ==============
;; This interpreter at hand has been realized in the programming
;; language Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-26
;; 
;; Sources:
;;   [datta2024memorymapped]
;;   Subham Datta, "Memory-Mapped vs. Isolated I/O", March 18th, 2024;
;;     published on "Baeldung"
;;   URL: "https://www.baeldung.com/cs/memory-mapped-vs-isolated-io"
;;   
;;   [esolang2023Addbig]
;;   The Esolang contributors, "Addbig", February 25th, 2023
;;   URL: "https://esolangs.org/wiki/Addbig"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and maps to
   a value of the VALUE-TYPE, for both holds the comprehensive default
   value of ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype index ()
  "The ``index'' type defines a signed integer of no natural bournes,
   nuncupated to the construe as a memory subscript.
   ---
   While non-negative memory locations, incepting from inclusive zero
   (0), reference the effective instruction and data space, the negative
   moiety in this memory-mapped I/O architecture accommodates the
   signification of input and output facilities, rather than material
   causata."
  '(integer * *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   signed integer cells, amenable to signed integer indices, and
   realized as a hash table whose cells accommodate the addresses, while
   the values assume the associated memory content."
  '(hash-table-of index integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (memory index) integer)
                cell-value-at))
(declaim (ftype (function (integer memory index) (values))
                (setf cell-value-at)))

;;; -------------------------------------------------------

(defun make-empty-memory ()
  "Creates and returns a ``memory'' object whose entries are all
   initialized to zero (0)."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun make-memory-from (&rest initial-elements)
  "Creates and returns a new ``memory'' object whose first N cells are
   initialized via the INITIAL-ELEMENTS, where N equals the tally of
   initial-elements specified."
  (declare (type list initial-elements))
  (let ((memory (make-empty-memory)))
    (declare (type memory memory))
    (loop
      for initial-element of-type integer in   initial-elements
      and position        of-type fixnum  from 0 by 1
      do  (setf (cell-value-at memory position) initial-element))
    (the memory memory)))

;;; -------------------------------------------------------

(defun cell-value-at (memory index)
  "Returns the integer value stored in the MEMORY cell located at the
   specified INDEX."
  (declare (type memory memory))
  (declare (type index  index))
  (the integer
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY cell located at the INDEX and
   returns no value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type index   index))
  (setf (gethash index memory 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun select-triple (memory start-index)
  "Returns the three accolent MEMORY elements commencing from the
   START-INDEX as multiple values:
     (1) The value of the MEMORY cell at the position START-INDEX.
     (2) The value of the MEMORY cell at the position START-INDEX + 1.
     (3) The value of the MEMORY cell at the position START-INDEX + 2."
  (declare (type memory memory))
  (declare (type index  start-index))
  (the (values integer integer integer)
    (values
      (cell-value-at memory (+ start-index 0))
      (cell-value-at memory (+ start-index 1))
      (cell-value-at memory (+ start-index 2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (char= candidate #\Newline)
          (char= candidate #\Space)
          (char= candidate #\Tab))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position into
   the SOURCE immediately succeeding the matching parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, returns the
   location immediately succeeding the detected word's end."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-next-word (source start)
  "Proceeding from the START position into the SOURCE, and skipping a
   contingency of zero or more accolent whitespaces in the prevenient
   locations, extracts the next word, and returns two values:
     (1) A string representation of the detected word, which may be the
         empty word.
     (2) The position into the SOURCE immediately succeeding the
         extracted word's occupied parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (let* ((word-start (skip-whitespaces   source start))
         (word-end   (locate-end-of-word source word-start)))
    (declare (type fixnum word-start))
    (declare (type fixnum word-end))
    (the (values string fixnum)
      (values
        (subseq source word-start word-end)
        word-end))))

;;; -------------------------------------------------------

(defun parse-memory (source)
  "Parses the SOURCE, expected to comprehend a sequence of zero or more
   whitespace-separated signed or unsigned integer numbers, and returns
   a ``memory'' representation of the extracted data."
  (declare (type string source))
  (let ((position (skip-whitespaces source 0)))
    (declare (type fixnum position))
    (the memory
      (apply #'make-memory-from
        (loop while (< position (length source)) collect
          (multiple-value-bind (next-word new-position)
              (read-next-word source position)
            (declare (type string next-word))
            (declare (type fixnum new-position))
            (setf position
              (skip-whitespaces source new-position))
            (parse-integer next-word)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input facility.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Input-Conduit
  "The ``Input-Conduit'' interface offers a common foundry for all
   classes in a pursuit to provide an input conduit for the interaction
   with an Addbig program.")

;;; -------------------------------------------------------

(defgeneric get-input (conduit)
  (:documentation
    "Queries and returns the next signed integer input from the input
     CONDUIT."))

;;; -------------------------------------------------------

(defstruct (Interactive-Input-Conduit
  (:include   Input-Conduit)
  (:conc-name NIL))
  "The ``Interactive-Input-Conduit'' class establishes an input provider
   which queries the standard input for a character and returns its
   numeric ASCII code.
   ---
   The ``considers-newline-as-eof-p'' flag may be adjusted in order to
   specify whether a newline input shall be equiparated with the end
   of file (EOF) marker, and thus generate a zero (0) response.
   ---
   Via the ``displays-prompt-p'' adjustment the system's prompt message,
   \">> \" is activated or, for a ``NIL'' configuration, suppressed."
  (considers-newline-as-eof-p T :type boolean :read-only NIL)
  (displays-prompt-p          T :type boolean :read-only NIL))

;;; -------------------------------------------------------

(defun get-character-code (conduit character)
  "Returns the character code represent the CHARACTER in accordance with
   the input CONDUIT's configurations."
  (declare (type Interactive-Input-Conduit conduit))
  (declare (type character                 character))
  (the integer
    (or (and (char= character #\Newline)
             (considers-newline-as-eof-p conduit)
             0)
        (char-code character))))

;;; -------------------------------------------------------

(defmethod get-input ((conduit Interactive-Input-Conduit))
  "Queries the standard input for the next character, depending upon the
   input CONDUIT's configuration, preceded by a prompt message, and
   returns its ASCII code."
  (declare (type Interactive-Input-Conduit))
  (when (displays-prompt-p conduit)
    (format T "~&>> "))
  (finish-output)
  (the integer
    (prog1
      (get-character-code conduit
        (read-char NIL NIL #\Null))
      (clear-input))))

;;; -------------------------------------------------------

(defstruct (String-Input-Conduit
  (:include     Input-Conduit)
  (:constructor make-string-input-conduit
    (content
     &aux (buffer (make-string-input-stream content))))
  (:conc-name   input-))
  "The ``String-Input-Conduit'' class implements an input conduit whose
   effective input's provenance is accommodated by a predefined string
   object.
   ---
   Internally, this class does not operate, nor store a reference, to
   the transmitted string content, rather relaying siccan bailiwick to
   a input string stream's efforts in order to enjoy an amplification in
   its character handling's comfort. This hid stream closes with the
   desinent data item's acquisition."
  (buffer (error "Missing buffer.") :type string-stream :read-only T))

;;; -------------------------------------------------------

(defun close-buffer-if-exhausted (conduit)
  "Determines whether the input CONDUIT's buffer is open but
   concomitantly exhausted, upon confirmation closing the same,
   otherwise accompassing no effect, in any case returns no value."
  (declare (type String-Input-Conduit conduit))
  (when (and (open-stream-p (input-buffer conduit))
             (not (peek-char NIL (input-buffer conduit) NIL)))
    (close
      (input-buffer conduit)))
  (values))

;;; -------------------------------------------------------

(defmethod get-input ((conduit String-Input-Conduit))
  "Returns the ASCII code of the input CONDUIT buffer's next character,
   or, upon its exhaustion, responds with the end-of-file (EOF) sentinel
   zero (0)."
  (declare (type String-Input-Conduit conduit))
  (close-buffer-if-exhausted conduit)
  (the integer
    (or (and (open-stream-p (input-buffer conduit))
             (char-code
               (read-char
                 (input-buffer conduit))))
        0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program (memory
                        &key (input (make-interactive-input-conduit)))
  "Executes the program commorant in the program MEMORY and returns no
   value."
  (declare (type Memory        memory))
  (declare (type Input-Conduit input))
  (let ((pc 0))
    (declare (type index pc))
    
    (loop do
      (multiple-value-bind (a b c)
          (select-triple memory pc)
        (declare (type index a))
        (declare (type index b))
        (declare (type index c))
        
        (symbol-macrolet
            ((memory[a] (cell-value-at memory a))
             (memory[b] (cell-value-at memory b)))
          (declare (type integer memory[a]))
          (declare (ignorable    memory[a]))
          (declare (type integer memory[b]))
          (declare (ignorable    memory[b]))
          
          ;; Either increment memory[a] by memory[b] or by an input.
          (incf memory[a]
            (or (and (minusp b)
                     (get-input input))
                memory[b]))
          
          ;; The supputated sum ought to be stored, forecause the value
          ;; of memory(a), succeeding an output operation, is always
          ;; reset to zero (0), and as such would inevitably evite the
          ;; docimasy with respect to a goto eligiblity.
          (let ((sum memory[a]))
            (declare (type integer sum))
            
            (when (minusp a)
              (format T "~c"
                (code-char memory[a]))
              (setf memory[a] 0))
            
            (cond
              ;; Terminate program?
              ((and (plusp sum)
                    (minusp c))
                (loop-finish))
              ;; Goto(c).
              ((plusp sum)
                (setf pc c))
              ;; Advance three cells forward to the next instruction.
              (T
                (incf pc 3))))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello".
(execute-program
  (make-memory-from
    -1 15 3
    -1 16 6
    -1 17 9
    -1 18 12
    -1 19 -1
    72 101 108
    108 111))

;;; -------------------------------------------------------

;; Repeating cat program which terminates on an input of the
;; "null character".
(execute-program
  (parse-memory
    "-1 -1  0
      2  3 -1"))

;;; -------------------------------------------------------

;; Repeating cat program which terminates on an input of the
;; "null character".
(execute-program
  (make-memory-from
    -1 -1  0
     2  3 -1)
  :input (make-string-input-conduit "Kiwi"))

;;; -------------------------------------------------------

;; Truth-machine.
;; 
;; Memory layout:
;;   0 -1 3
;;   0 [-48] 9
;;  -1 [48] -1
;;  -1 [49] repeatThisLine
;;  -48 48 49
(execute-program
  (parse-memory
    " 0  -1  3
      0   12  9
     -1   13 -1
     -1   14  9
     -48  48  49"))
