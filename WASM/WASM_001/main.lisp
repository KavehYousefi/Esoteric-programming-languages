;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "WASM", presented by the Esolang user "jan Gamecuber" in the
;; year 2022, and designed as a derivative of Urban Mueller's brainfuck
;; in the guise of both a low-level byte code as well as an assembly
;; language, additionally supplying enhancements in the perimeter of the
;; available instructions.
;; 
;; 
;; Concept
;; =======
;; WASM, naming an abbreviation for *W*orst *AS*sembly, subsumes into
;; the brainfuck derivatives' kinship, its kenspeckle attribute
;; realized in the provision of two lower-level syntactial designs,
;; one as bytecode, the other as an assembly language, pursuing its
;; stock-father's competence.
;; 
;; == WASM: LOW-LEVEL BRAINFUCK ==
;; WASM's syntactical diorism bifurcates into its two manifestations of
;; low-level programming, both endowed with equipollence:
;; 
;;   (a) A bytecode design which allots to each of the 16 instructions
;;       a four-bit (nybble) code.
;;   (b) An assembly language variant that represents the same
;;       operational set by mnemonics.
;; 
;; == WASM: OPERATIONAL DUPLICATION ==
;; The legatee supererogates in the sense of replicating its brainfuck
;; octuple set and expanding it into twice its cardinality via numeric
;; input/output facilities, an "if" construct, and cell value and cell
;; pointer modifications by steps of six (6).
;; 
;; == THE PROGRAM MEMORY: AN INFINITE TAPE OF SIGNED INTEGERS ==
;; Concording with a most liberal construe of the brainfuck memory
;; architecture, WASM programs are granted adit to a tape of cells,
;; extending bilaterally into infinity, with each cell a salvatory to
;; a signed integer of unbounded mickleness.
;; 
;; A mobile cell pointer, or simply "pointer", maintains the currently
;; active cell, exclusively amenable to indagations and modifications.
;; 
;; 
;; Instructions
;; ============
;; WASM's instruction set is compact of 16 members, incorporating the
;; complete array of its brainfuck clerenomy's capacitations,
;; complemented into a twifold tally by numeric input and output
;; conduits, a conditional "if" construct, and variants of the
;; traditional increment/decrement and cell pointer translation commands
;; which apply an offset of six (6) in lieu of one (1).
;; 
;; == OVERVIEW ==
;; An apercu shall be a steadable source for a cursory understanding of
;; the language's facilities, collocating each four-bit bytecode, its
;; hexadecimal equivalent and the assembly language mnemonic to the
;; operation's effect.
;; 
;;   ------------------------------------------------------------------
;;   Bytecode | Hex. | Mnemonic   | Effect
;;   ---------+------+------------+------------------------------------
;;   0000     |  0   | Right      | Moves the cell pointer one (1) step
;;            |      |            | to the right.
;;   ..................................................................
;;   0001     |  1   | Left       | Moves the cell pointer one (1) step
;;            |      |            | to the left.
;;   ..................................................................
;;   0010     |  2   | Inc        | Increments the current cell value
;;            |      |            | by one (1).
;;   ..................................................................
;;   0011     |  3   | Dec        | Decrements the current cell value
;;            |      |            | by one (1).
;;   ..................................................................
;;   0100     |  4   | OutUnicode | Prints the Unicode character
;;            |      |            | associated with the current cell
;;            |      |            | value in the role of a code point
;;            |      |            | to the standard output.
;;   ..................................................................
;;   0101     |  5   | InUnicode  | Queries the user for a Unicode
;;            |      |            | character input and stores the
;;            |      |            | associated code point in the
;;            |      |            | current cell.
;;   ..................................................................
;;   0110     |  6   | While      | While the current cell value equals
;;            |      |            | zero (0), repeats the instructions
;;            |      |            | betwixt this "While" and the
;;            |      |            | matching "EndWhile" marker.
;;   ..................................................................
;;   0111     |  7   | EndWhile   | Demarcates the end of the matching
;;            |      |            | "While" instruction.
;;   ..................................................................
;;   1000     |  8   | Right6     | Moves the cell pointer six (6)
;;            |      |            | steps to the right.
;;   ..................................................................
;;   1001     |  9   | Left6      | Moves the cell pointer six (6)
;;            |      |            | steps to the left.
;;   ..................................................................
;;   1010     |  A   | Inc6       | Increments the current cell value
;;            |      |            | by six (6).
;;   ..................................................................
;;   1011     |  B   | Dec6       | Decrements the current cell value
;;            |      |            | by six (6).
;;   ..................................................................
;;   1100     |  C   | OutNum     | Prints the current cell value
;;            |      |            | verbatim as an integer number.
;;   ..................................................................
;;   1101     |  D   | InNum      | Queries the user for an integer
;;            |      |            | number input and stores the same in
;;            |      |            | the current cell.
;;   ..................................................................
;;   1110     |  E   | If         | If the current cell value equals
;;            |      |            | zero (0), executes the instructions
;;            |      |            | betwixt this "If" and the matching
;;            |      |            | "EndIf" marker once.
;;            |      |            | Otherwise skips the section.
;;   ..................................................................
;;   1111     |  F   | EndIf      | Demarcates the end of the matching
;;            |      |            | "If" instruction.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Date: 2023-02-11
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Wasm"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype nybble ()
  "The ``nybble'' type defines a sequence of four consecutive bits,
   forming the moeity of a byte, commonly agnominated a \"nybble\"."
  '(unsigned-byte 4))

;;; -------------------------------------------------------

(deftype nybble-vector ()
  "The ``nybble-vector'' type defines a vector of zero or more nybbles."
  '(vector nybble *))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized instruction variants."
  '(member
     :right
     :left
     :inc
     :dec
     :out-unicode
     :in-unicode
     :while
     :end-while
     :right-6
     :left-6
     :inc-6
     :dec-6
     :out-num
     :in-num
     :if
     :end-if))

;;; -------------------------------------------------------

(deftype wasm-program ()
  "A ``wasm-program'' defines an executable WASM program as either a
   vector of commands or a one-dimensional simple array of such."
  '(or (vector command        *)
       (simple-array command (*))))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype cell-table ()
  "The ``cell-table'' type defines a contingently sparse vector of
   signed integer cells amenable to signed integer subscripts, modeled
   as a hash table which associates arbitrary-valued integer keys to
   values of the same ilk."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype magnitude ()
  "The ``magnitude'' type defines a non-negative integer without upper
   bourne, thus occupying the range [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines an affiliation of jump start
   positions in an instruction vector to the matching end locations,
   and vice versa, manifesting in the form of a hash table which
   associates fixnum keys to fixnum values.
   ---
   Each element of the underlying instruction sequence expected to be
   unambiguously identified by its position, the jump table itself does
   not distinguish betwixt the actual language constructs thus modeled,
   that is, whether an entry refers to a \"While\" loop or an \"If\"
   conditional."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for printing operations,
   such as ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of instruction table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-instruction-for-bytecode (bytecode)
  "Returns the instruction corresponding to the BYTECODE, or signals an
   error of an unspecified in the case of its disrespondency."
  (declare (type nybble bytecode))
  (the command
    (case bytecode
      (#b0000 :right)
      (#b0001 :left)
      (#b0010 :inc)
      (#b0011 :dec)
      (#b0100 :out-unicode)
      (#b0101 :in-unicode)
      (#b0110 :while)
      (#b0111 :end-while)
      (#b1000 :right-6)
      (#b1001 :left-6)
      (#b1010 :inc-6)
      (#b1011 :dec-6)
      (#b1100 :out-num)
      (#b1101 :in-num)
      (#b1110 :if)
      (#b1111 :end-if)
      (otherwise
        (error "No command associated with the bytecode ~4,'0b."
          bytecode)))))

;;; -------------------------------------------------------

(defun get-instruction-for-identifier (identifier)
  "Returns the instruction corresponding to the IDENTIFIER, or signals
   an error of an unspecified type in the case of its disrespondency."
  (declare (type string identifier))
  (flet ((probe-identifier (expected-identifier instruction)
          "Checks whether the IDENTIFIER matches the
           EXPECTED-IDENTIFIER, returning on confirmation the
           INSTRUCTION, otherwise ``NIL''."
          (declare (type string  expected-identifier))
          (declare (type command instruction))
          (the (or null command)
            (when (string= identifier expected-identifier)
              instruction))))
    (the command
      (or
        (probe-identifier "Right"      :right)
        (probe-identifier "Left"       :left)
        (probe-identifier "Inc"        :inc)
        (probe-identifier "Dec"        :dec)
        (probe-identifier "OutUnicode" :out-unicode)
        (probe-identifier "InUnicode"  :in-unicode)
        (probe-identifier "While"      :while)
        (probe-identifier "EndWhile"   :end-while)
        (probe-identifier "Right6"     :right-6)
        (probe-identifier "Left6"      :left-6)
        (probe-identifier "Inc6"       :inc-6)
        (probe-identifier "Dec6"       :dec-6)
        (probe-identifier "OutNum"     :out-num)
        (probe-identifier "InNum"      :in-num)
        (probe-identifier "If"         :if)
        (probe-identifier "EndIf"      :end-if)
        (error "No recognized instruction name: ~s." identifier)))))

;;; -------------------------------------------------------

(defun get-bytecode-for-instruction (instruction)
  "Returns the bytecode corresponding to the INSTRUCTION, or signals an
   error of an unspecified type upon its disrespondency."
  (declare (type command instruction))
  (the nybble
    (case instruction
      (:right       #b0000)
      (:left        #b0001)
      (:inc         #b0010)
      (:dec         #b0011)
      (:out-unicode #b0100)
      (:in-unicode  #b0101)
      (:while       #b0110)
      (:end-while   #b0111)
      (:right-6     #b1000)
      (:left-6      #b1001)
      (:inc-6       #b1010)
      (:dec-6       #b1011)
      (:out-num     #b1100)
      (:in-num      #b1101)
      (:if          #b1110)
      (:end-if      #b1111)
      (otherwise
        (error "Invalid instruction: ~s." instruction)))))

;;; -------------------------------------------------------

(defun get-mnemonic-for-instruction (instruction)
  "Returns the assembly language mnemonic corresponding to the
   INSTRUCTION, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type command instruction))
  (the simple-string
    (case instruction
      (:right       "Right")
      (:left        "Left")
      (:inc         "Inc")
      (:dec         "Dec")
      (:out-unicode "OutUnicode")
      (:in-unicode  "InUnicode")
      (:while       "While")
      (:end-while   "EndWhile")
      (:right-6     "Right6")
      (:left-6      "Left6")
      (:inc-6       "Inc6")
      (:dec-6       "Dec6")
      (:out-num     "OutNum")
      (:in-num      "InNum")
      (:if          "If")
      (:end-if      "EndIf")
      (otherwise
        (error "Invalid instruction: ~s." instruction)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of bytecode parser.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-bytecode (chunks)
  "Extracts from the bytecode CHUNKS the entailed instructions and
   returns these as a one-dimensional simple array of commands."
  (declare (type nybble-vector chunks))
  (the wasm-program
    (map '(simple-array command (*))
      #'get-instruction-for-bytecode chunks)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of assembler.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identifier-character-p (candidate)
  "Checks whether the CANDIDATE represents an identifier constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alphanumericp candidate)))))

;;; -------------------------------------------------------


(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION in the SOURCE, or ``NIL'' upon
   the location specifier's transgression of the SOURCE's boundaries."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position in the SOURCE, skips a sequence
   of zero or more adjacent whitespaces and returns the position in the
   SOURCE immediately succeeding the last skipped whitespace."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for position
        of-type fixnum
        from    start
        by      1
      for character
        of-type (or null character)
        =       (get-character-at source position)
      while (and character (whitespace-character-p character))
      finally
        (return position))))

;;; -------------------------------------------------------

(defun expect-whitespaces (source start)
  "Checks whether the character at the START position in the SOURCE
   constitutes a whitespace, on confirmation skipping all adjacent
   whitespaces and finally returning the position in the SOURCE
   immediately following the last consumed instance; otherwise
   signals an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-character (get-character-at source start)))
    (declare (type (or null character) current-character))
    (the fixnum
      (cond
        ((null current-character)
          (error "Expected whitespace, but encountered EOF at ~
                  position ~d."
            start))
        ((whitespace-character-p current-character)
          (skip-whitespaces source start))
        (T
          (error "Expected whitespaces, but encountered \"~c\" ~
                  at position ~d."
            current-character start))))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position in the SOURCE, reads a word
   composed of zero or more identifier characters, and returns two
   values:
     (1) a string comprehending the consumed word
     (2) the position in the SOURCE immediately following the last word
         character."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end start))
    (declare (type fixnum end))
    (the (values string fixnum)
      (values
        (with-output-to-string (word)
          (declare (type string-stream word))
          (loop
            for position
              of-type fixnum
              from    start
            for character
              of-type (or null character)
              =       (get-character-at source position)
            while (and character (identifier-character-p character))
            do
              (write-char character word)
              (incf       end)))
        end))))

;;; -------------------------------------------------------

(defun read-identifier (source start)
  "Proceeding from the START position in the SOURCE, reads an
   identifier and returns two values:
     (1) the instruction associated with the consumed identifier
     (2) the position in the SOURCE immediately following the last
         identifier character."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (word end)
      (read-word source start)
    (declare (type string word))
    (declare (type fixnum end))
    (the (values command fixnum)
      (values
        (get-instruction-for-identifier word)
        end))))

;;; -------------------------------------------------------

(defun parse-assembly (source)
  "Parses the WASM assembly language SOURCE and returns a
   one-dimensional simple array of the extracted instructions."
  (declare (type string source))
  (let ((instructions NIL)
        (position     0))
    (declare (type (list-of command) instructions))
    (declare (type fixnum            position))
    (flet
        ((end-of-p ()
          "Checks whether the POSITION cursor has reached the end of the
           SOURCE, returning on confirmation a ``boolean'' value of
           ``T'', otherwise ``NIL''."
          (the boolean
            (not (null
              (>= position (length source))))))
         
         (expect-sepiment ()
          "Checks whether, proceeding from the current POSITION, either
           one or more whitespaces follow, or the end of the SOURCE has
           been reached, in the former case skipping the whitespaces,
           in the latter simply exiting this function, in both
           situations returning no value.
           ---
           If a non-whitespace character follows, an error of an
           unspecified type is signaled."
          (let ((current-character (get-character-at source position)))
            (declare (type (or null character) current-character))
            (when current-character
              (setf position (expect-whitespaces source position))))
          (values))
         
         (collect-instruction (instruction end)
          "Proceeding from the current POSITION, reads an identifier,
           determines its affiliated instruction, inserts the same at
           the INSTRUCTIONS list head, updates the POSITION cursor, and
           returns no value."
          (declare (type command instruction))
          (declare (type fixnum  end))
          (push instruction instructions)
          (setf position    end)
          (values)))
      
      (setf position (skip-whitespaces source position))
      
      (loop until (end-of-p) do
        (multiple-value-call #'collect-instruction
          (read-identifier source position))
        (expect-sepiment)))
    
    (the wasm-program
      (coerce
        (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class models a sparse vector of cells, answering to
   signed integer indices and storing values of the same type, with the
   currently active cell amenable to a motile cell pointer."
  (cells   (make-hash-table :test #'eql) :type cell-table)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun memory-move-right (memory distance)
  "Translates the MEMORY's cell pointer the DISTANCE of steps to the
   right and returns the modified MEMORY."
  (declare (type Memory    memory))
  (declare (type magnitude distance))
  (incf (memory-pointer memory) distance)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory distance)
  "Translates the MEMORY's cell pointer the DISTANCE of steps to the
   left and returns the modified MEMORY."
  (declare (type Memory    memory))
  (declare (type magnitude distance))
  (decf (memory-pointer memory) distance)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (gethash (memory-pointer memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Sets the value of the MEMORY's current cell to the NEW-VALUE and
   returns the modified MEMORY."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory) (memory-cells memory) 0)
        new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory amount)
  "Increments the MEMORY's current cell value by the AMOUNT and returns
   the modified MEMORY."
  (declare (type Memory    memory))
  (declare (type magnitude amount))
  (incf (memory-current-cell memory) amount)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-decrement (memory amount)
  "Decrements the MEMORY's current cell value by the AMOUNT and returns
   the modified MEMORY."
  (declare (type Memory    memory))
  (declare (type magnitude amount))
  (decf (memory-current-cell memory) amount)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-cell-zero-p (memory)
  "Determines whether the MEMORY's current cell contains the value
   zero, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Memory memory))
  (the boolean
    (not (null
      (zerop (memory-current-cell memory))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Computes and returns for the WASM INSTRUCTIONS a jump table which
   associates each forward jump position in the sequence with the
   respective back jump location, and vice versa.
   ---
   Forward jumps are represented by the \"While\" and \"If\"
   operations, whereas back jumps entail \"EndWhile\" and \"EndIf\"."
  (declare (type wasm-program instructions))
  (let ((jump-table            (make-hash-table :test #'eql))
        (while-start-positions NIL)
        (if-start-positions    NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) while-start-positions))
    (declare (type (list-of fixnum) if-start-positions))
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0 by 1
      do
        (case instruction
          (:while
            (push position while-start-positions))
          (:end-while
            (if while-start-positions
              (let ((start-position (pop while-start-positions))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (setf (gethash start-position jump-table)
                      end-position)
                (setf (gethash end-position   jump-table)
                       start-position))
              (error "Unmatched \"EndWhile\" instruction ~
                      at position ~d."
                position)))
          (:if
            (push position if-start-positions))
          (:end-if
            (if if-start-positions
              (let ((start-position (pop if-start-positions))
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (setf (gethash start-position jump-table)
                      end-position)
                (setf (gethash end-position jump-table)
                      start-position))
              (error "Unmatched \"IfEnd\" instruction ~
                      at position ~d."
                position)))
          (otherwise
            NIL)))
    (when while-start-positions
      (error "Unmatched \"While\" start instructions ~
              at positions ~{~d~^, ~}."
        while-start-positions))
    (when if-start-positions
      (error "Unmatched \"If\" start instructions ~
              at positions ~{~d~^, ~}."
        if-start-positions))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the WASM INSTRUCTIONS and returns no value."
  (declare (type wasm-program instructions))
  (when (plusp (length instructions))
    (let ((ip         0)
          (jump-table (build-jump-table instructions))
          (memory     (make-memory)))
      (declare (type fixnum     ip))
      (declare (type jump-table jump-table))
      (declare (type Memory     memory))
      (flet
          ((has-more-instructions-p ()
            "Determines whether one or more instructions remain to
             be executed starting at the instruction pointer IP's
             location, returning upon confirmation a ``boolean'' value
             of ``T'', otherwise ``NIL''."
            (the boolean
              (not (null
                (< ip (length instructions))))))
           
           (get-current-instruction ()
            "Returns the instruction at the instruction pointer (IP)
             position, or ``NIL'' upon its transgression of the
             INSTRUCTIONS' bounds."
            (the (or null command)
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip))))
           
           (advance ()
            "Advances the instruction pointer IP to the next position
             and returns no value."
            (incf ip)
            (values))
           
           (jump-to-opposite-boundary ()
            "Expecting the instruction pointer IP to currently reside
             on an iteration or conditional boundary, relocates it to
             the opposite march and returns no value.
             ---
             In the case of a missing corresponding with the IP
             position in the JUMP-TABLE, an error of an unspecified
             type is signaled."
            (setf ip
              (or (gethash ip jump-table)
                  (error "No jump target defined for the position ~d."
                    ip)))
            (values)))
        
        (loop while (has-more-instructions-p) do
          (case (get-current-instruction)
            ((NIL)
              (loop-finish))
            
            (:right
              (memory-move-right memory 1))
            
            (:left
              (memory-move-left memory 1))
            
            (:inc
              (memory-increment memory 1))
            
            (:dec
              (memory-decrement memory 1))
            
            (:out-unicode
              (format T "~c"
                (code-char
                  (memory-current-cell memory))))
            
            (:in-unicode
              (format T "~&Please input a Unicode character: ")
              (setf (memory-current-cell memory)
                    (char-code (read-char)))
              (clear-input))
            
            (:while
              (when (memory-current-cell-zero-p memory)
                (jump-to-opposite-boundary)))
            
            (:end-while
              (unless (memory-current-cell-zero-p memory)
                (jump-to-opposite-boundary)))
            
            (:right-6
              (memory-move-right memory 6))
            
            (:left-6
              (memory-move-left memory 6))
            
            (:inc-6
              (memory-increment memory 6))
            
            (:dec-6
              (memory-decrement memory 6))
                        
            (:out-num
              (format T "~d"
                (memory-current-cell memory)))
            
            (:in-num
              (format T "~&Please enter an integer number: ")
              (setf (memory-current-cell memory)
                    (parse-integer (read-line)))
              (clear-input))
            
            (:if
              (when (memory-current-cell-zero-p memory)
                (jump-to-opposite-boundary)))
            
            (:end-if
              NIL)
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                (get-current-instruction) ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-WASM-bytecode (chunks)
  "Interprets the WASM bytecode CHUNKS and returns no value."
  (declare (type nybble-vector chunks))
  (process-instructions
    (parse-bytecode chunks))
  (values))

;;; -------------------------------------------------------

(defun interpret-WASM-assembly (assembly-code)
  "Interprets the piece of WASM ASSEMBLY-CODE and returns no value."
  (declare (type string assembly-code))
  (process-instructions
    (parse-assembly assembly-code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of WASM code generator.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun encode-instructions-as-bytecode (instructions)
  "Generates and returns for the WASM INSTRUCTIONS a vector of
   equivalent bytecodes."
  (declare (type wasm-program instructions))
  (the nybble-vector
    (map 'nybble-vector #'get-bytecode-for-instruction instructions)))

;;; -------------------------------------------------------

(defun encode-instructions-as-assembly (instructions
                                        &key (destination NIL))
  "Generates for the WASM INSTRUCTIONS the equivalent assembly language
   mnemonics and writes these to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responds with
   a fresh string comprehending the result."
  (declare (type wasm-program instructions))
  (declare (type destination  destination))
  (the (or null string)
    (if destination
      (loop for instruction of-type command across instructions do
        (format destination "~&~a"
          (get-mnemonic-for-instruction instruction)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (encode-instructions-as-assembly instructions
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of bytecode-to-assembly converter.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-bytecode-to-mnemonic (bytecode)
  "Returns for the four-bit BYTECODE the corresponding assembly
   language mnemonic, or signals an error of an unspecified type upon
   its disrespondency."
  (the simple-string
    (case bytecode
      (#b0000 "Right")
      (#b0001 "Left")
      (#b0010 "Inc")
      (#b0011 "Dec")
      (#b0100 "OutUnicode")
      (#b0101 "InUnicode")
      (#b0110 "While")
      (#b0111 "EndWhile")
      (#b1000 "Right6")
      (#b1001 "Left6")
      (#b1010 "Inc6")
      (#b1011 "Dec6")
      (#b1100 "OutNum")
      (#b1101 "InNum")
      (#b1110 "If")
      (#b1111 "EndIf")
      (otherwise
        (error "Invalid bytecode: ~4,'0b." bytecode)))))

;;; -------------------------------------------------------

(defun compile-bytecode-to-assembly (chunks &key (destination NIL))
  "Generates for the bytecode CHUNKS the corresponding sequence of
   assembly language mnemonics to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type nybble-vector chunks))
  (declare (type destination   destination))
  (the (or null string)
    (if destination
      (loop for bytecode of-type nybble across chunks do
        (format destination "~&~a"
          (translate-bytecode-to-mnemonic bytecode)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (compile-bytecode-to-assembly chunks :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of assembly-to-bytecode converter.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-mnemonic-to-bytecode (mnemonic)
  "Returns for the assembly language MNEMONIC the corresponding
   four-bit bytecode, or signals an error of an unspecified type upon
   its disrespondency."
  (declare (type string mnemonic))
  (flet
      ((probe-mnemonic (expected-mnemonic bytecode)
        "Determines whether the MNEMONIC matches the EXPECTED-MNEMONIC,
         on confirmation returning the BYTECODE, otherwise ``NIL''."
        (declare (type string expected-mnemonic))
        (declare (type nybble bytecode))
        (the (or null nybble)
          (when (string= mnemonic expected-mnemonic)
            bytecode))))
    (the nybble
      (or
        (probe-mnemonic "Right"      #b0000)
        (probe-mnemonic "Left"       #b0001)
        (probe-mnemonic "Inc"        #b0010)
        (probe-mnemonic "Dec"        #b0011)
        (probe-mnemonic "OutUnicode" #b0100)
        (probe-mnemonic "InUnicode"  #b0101)
        (probe-mnemonic "While"      #b0110)
        (probe-mnemonic "EndWhile"   #b0111)
        (probe-mnemonic "Right6"     #b1000)
        (probe-mnemonic "Left6"      #b1001)
        (probe-mnemonic "Inc6"       #b1010)
        (probe-mnemonic "Dec6"       #b1011)
        (probe-mnemonic "OutNum"     #b1100)
        (probe-mnemonic "InNum"      #b1101)
        (probe-mnemonic "If"         #b1110)
        (probe-mnemonic "EndIf"      #b1111)
        (error "Invalid mnemonic: ~s." mnemonic)))))

;;; -------------------------------------------------------

(defun compile-assembly-to-bytecode (mnemonics)
  "Generates and returns for the list of MNEMONICS the equivalent vector
   of bytecode nybbles."
  (declare (type (list-of string) mnemonics))
  (the nybble-vector
    (map 'nybble-vector #'translate-mnemonic-to-bytecode mnemonics)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance functions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-nybble-vector (&rest nybbles)
  "Creates and returns for the NYBBLES a fresh nybble vector containing
   the input elements."
  (declare (type (list-of nybble) nybbles))
  (the nybble-vector
    (coerce nybbles 'nybble-vector)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-WASM converter.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-instruction-for-brainfuck-token (token)
  "Returns the WASM instruction corresponding to the brainfuck TOKEN,
   or signals an error of an unspecified type upon its disrespondency."
  (declare (type character token))
  (the command
    (case token
      (#\> :right)
      (#\< :left)
      (#\+ :inc)
      (#\- :dec)
      (#\. :out-unicode)
      (#\, :in-unicode)
      (#\[ :while)
      (#\] :end-while)
      (otherwise
        (error "No brainfuck instruction: ~s." token)))))

;;; -------------------------------------------------------

(defun brainfuck-instruction-token-p (token)
  "Determines whether the TOKEN represents a brainfuck instruction,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token "><+-.,[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun parse-brainfuck (brainfuck-code)
  "Extracts from BRAINFUCK-CODE a one-dimensional simple array of WASM
   instructions and returns it."
  (declare (type string brainfuck-code))
  (the wasm-program
    (coerce
      (loop
        for     token of-type character across brainfuck-code
        when    (brainfuck-instruction-token-p token)
        collect (get-instruction-for-brainfuck-token token))
      '(simple-array command (*)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello World!" using WASM bytecode.
(interpret-WASM-bytecode
  (make-nybble-vector
    #b0000
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0110
    #b0011
    #b0001
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0010
    #b0000
    #b0111
    #b0001
    #b0100
    #b0000
    #b0000
    #b0010
    #b0000
    #b0011
    #b0110
    #b0010
    #b0111
    #b0010
    #b0010
    #b0000
    #b0010
    #b0010
    #b0000
    #b0010
    #b0010
    #b0010
    #b0110
    #b0000
    #b0110
    #b0011
    #b0000
    #b0010
    #b0010
    #b0010
    #b0001
    #b0001
    #b0010
    #b0010
    #b0010
    #b0000
    #b0111
    #b0001
    #b0001
    #b0111
    #b0000
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0100
    #b0000
    #b0011
    #b0000
    #b0010
    #b0010
    #b0010
    #b0100
    #b0100
    #b0010
    #b0010
    #b0010
    #b0100
    #b0000
    #b0011
    #b0100
    #b0001
    #b0001
    #b0010
    #b0110
    #b0000
    #b0110
    #b0010
    #b0000
    #b0010
    #b0111
    #b0000
    #b0000
    #b0111
    #b0001
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0100
    #b0000
    #b0000
    #b0100
    #b0010
    #b0010
    #b0010
    #b0100
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0100
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0011
    #b0100
    #b0000
    #b0010
    #b0100
    #b0000
    #b0010
    #b0100))

;;; -------------------------------------------------------

;; Print "Hello World!" using WASM mnemonics.
(interpret-WASM-assembly
  "
  Right
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  While
  Dec
  Left
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Inc
  Right
  EndWhile
  Left
  OutUnicode
  Right
  Right
  Inc
  Right
  Dec
  While
  Inc
  EndWhile
  Inc
  Inc
  Right
  Inc
  Inc
  Right
  Inc
  Inc
  Inc
  While
  Right
  While
  Dec
  Right
  Inc
  Inc
  Inc
  Left
  Left
  Inc
  Inc
  Inc
  Right
  EndWhile
  Left
  Left
  EndWhile
  Right
  Dec
  Dec
  Dec
  Dec
  Dec
  OutUnicode
  Right
  Dec
  Right
  Inc
  Inc
  Inc
  OutUnicode
  OutUnicode
  Inc
  Inc
  Inc
  OutUnicode
  Right
  Dec
  OutUnicode
  Left
  Left
  Inc
  While
  Right
  While
  Inc
  Right
  Inc
  EndWhile
  Right
  Right
  EndWhile
  Left
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  OutUnicode
  Right
  Right
  OutUnicode
  Inc
  Inc
  Inc
  OutUnicode
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  OutUnicode
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  Dec
  OutUnicode
  Right
  Inc
  OutUnicode
  Right
  Inc
  OutUnicode
  ")

;;; -------------------------------------------------------

;; Infinitely repeating bytecode cat program in hexadecimal notation.
(interpret-WASM-bytecode
  (make-nybble-vector
    #x2
    #x6
    #x5
    #x4
    #x7))

;;; -------------------------------------------------------

;; Infinitely repeating assembly language cat program.
(interpret-WASM-assembly
  "Inc
   While
   InUnicode
   OutUnicode
   EndWhile")

;;; -------------------------------------------------------

;; Truth-machine in bytecode.
(interpret-WASM-bytecode
  (make-nybble-vector
    #b1101
    #b1100
    #b0110
    #b1100
    #b0111))

;;; -------------------------------------------------------

;; Truth-machine in assembly language.
(interpret-WASM-assembly
  "InNum
   OutNum
   While
     OutNum
   EndWhile")

;;; -------------------------------------------------------

;; Query the user for a numeric input and print it only if it does not
;; equal zero (0).
(interpret-WASM-assembly
  "InNum
   If
     OutNum
   EndIf")

;;; -------------------------------------------------------

;; Convert the cat program from its assembly form into bytecode and
;; execute it.
(interpret-WASM-bytecode
  (compile-assembly-to-bytecode
    (list
      "Inc"
      "While"
      "InUnicode"
      "OutUnicode"
      "EndWhile")))
