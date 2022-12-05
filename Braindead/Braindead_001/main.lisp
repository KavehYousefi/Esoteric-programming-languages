;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Braindead", presented by the Esolang user "Leothetechguy"
;; in the year 2021, and designed as a confluence of Urban Mueller's
;; "brainfuck" with the quantative circumference of Jonathan Todd
;; Skinner's "Deadfish", in particular combining the cell pointer
;; translation and cell value manipulation operations, retaining the
;; output faculty, concomitant to a restricted input principle, as well
;; as permitting random access to instructions by their position in the
;; code.
;; 
;; 
;; Concept
;; =======
;; The Braindead programming language constitutes an esoteric example,
;; the purpose of its existence a curtailment of brainfuck's eight
;; commands to the quadruple tally of Deadfish, with the medium of this
;; chevisance the combination of several bequeathed faculties, as well
;; as the relocation of the input inquisition to an interpreter
;; parameter.
;; 
;; == FRUGALITY IN INSTRUCTIONS ==
;; The Braindead programming language's operative compass enumerates
;; four members --- precisely a moeity of its brainfuck cleronomy's
;; octuple account.
;; 
;; The warklume of its entelechy is comprised of several fendy
;; excogitations:
;; 
;;   (1) The brainfuck operation sequence ">" and "+" is coalesced into
;;       Braindead's ">".
;;   (2) Likewise, the original "<" and "-" combination is
;;       reappropriated into a simple "<".
;;   (3) The brainfuck jump command jumelle "[" and "]" has been
;;       superseded by a random-access "\" instruction locator.
;;   (4) While the output facility's (".") role retained its verbatim
;;       effect, concomitant to a neoterised agnomination into "/",
;;       the input survives in the form of an afferently induced
;;       parameter into the interpreter.
;; 
;; 
;; Instructions
;; ============
;; Braindead furnishes a quadruple array of instructions, consisting in
;; an amalgam of its inherited brainfuck memory pointer and cell value
;; manipulation instructions, as well as an output facility, and a
;; random-access jump navigator.
;; 
;; == OVERVIEW ==
;; An apercu shall accommodate the reader with a cursory nortelry anenst
;; Braindead's capacities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Moves the cell pointer one step to the right and
;;           | concomitantly increments the new current cell by one.
;;           | This constitutes an amalgam of brainfuck's commands ">"
;;           | and "+" in this exact order.
;;   ..................................................................
;;   <       | Moves the cell pointer one step to the left and
;;           | concomitantly decrements the new current cell by one.
;;           | This constitutes an amalgam of brainfuck's commands "<"
;;           | and "-" in this exact order.
;;   ..................................................................
;;   /       | Outputs the character corresponding to the current cell
;;           | value when interpreted as the character code.
;;           | This constitutes an exact equivalent of brainfuck's
;;           | command ".".
;;   ..................................................................
;;   \       | Relocates the instruction pointer (IP) to the command
;;           | whose zero-based index in the program equals the current
;;           | cell value.
;;           | This command does not correspond directly to any
;;           | brainfuck equivalent.
;;   ------------------------------------------------------------------
;; 
;; == INPUT ==
;; In counterdistinguishment from its output facility, input in
;; Braindead deviates from brainfuck's principle.
;; 
;; In lieu of a dedicated instruction, the user input is expected as the
;; second argument to the interpreter's invocation, succeeding the
;; program name or source code as the prime datum, and persisted in the
;; first memory cell prior to the actual instructions' execution.
;; 
;; == JUXTAPOSITION OF BRAINDEAD AND BRAINFUCK INSTRUCTIONS ==
;; With a partial equivalence betwixt Braindead and its stock-father
;; being a present fact, the following tabular description shall educate
;; regarding the operative vinculum:
;; 
;;   ------------------------------------------------------------------
;;   Braindead command | brainfuck equivalent
;;   ------------------+-----------------------------------------------
;;   >                 | >+
;;   ..................................................................
;;   <                 | <-
;;   ..................................................................
;;   /                 | .
;;   ..................................................................
;;   \                 | None
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The language's proterotype, having rather retained, at the moment of
;; this writ's creation, a caract of curtailment and inchoation, is
;; inflicted with a set of ambivalent properties, of whose exhaustion
;; the following listing cannot adduce claim; natheless, it shall
;; accoutre the most conspicuous items.
;; 
;; == HOW DOES THE JUMP INSTRUCTION RESPOND TO INVALID POSITIONS? ==
;; Its supersession of the brainfuck jumelle "[" and "]" for iterative
;; purposes serves as a particular encumbrance to the sole jump
;; instruction's ("\"), role. A consequence of its supererogative
;; requirement, the operations proceed by mediation of a random access
;; to the instruction sequence, employing the current cell value as an
;; index in the vein of a goto construct.
;; 
;; The reaction to an invalid subscript, such violates the range
;; spanning [0, N), where N equals the number of instructions, has not
;; be expressed in lucid terms. Three possible options exist:
;; 
;;   (1) An error is signaled, whence ensues the program termination
;;       with immediacy.
;;   (2) The transgression is construed as a behest to halt the program
;;       in a non-critical manner, similar to the advance of the
;;       instruction pointer from the desinent position.
;;   (3) The jump command is silently ignored, progressing to the next
;;       instruction in lieu of a goto relocation.
;; 
;; The tenability of invalid accesses as a commonality in Braindead
;; programs, embraced in champarty with the ludibund haecceity of
;; esoteric programming languages, has been chosen as an index to the
;; second choice (2).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-26
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Braindead"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized Braindead commands."
  '(member
    :move-right
    :move-left
    :output
    :jump))

;;; -------------------------------------------------------

(deftype instruction-vector ()
  "The ``instruction-vector'' type defines a vector of Braindead
   commands representing a program in this language."
  '(vector command *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory as an association of
   cell indices to cell values, manifesting as a hash table of signed
   integer keys to values of the same category."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype file-source ()
  "The ``file-source'' type defines a source when a Braindead program
   can be loaded."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of character command) +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+ (make-hash-table :test #'eql)
  "Associates with the recognized Braindead tokens the respective
   command objects.")

;;; -------------------------------------------------------

;; Register the recognized command tokens.
(setf (gethash #\> +COMMANDS+) :move-right)
(setf (gethash #\< +COMMANDS+) :move-left)
(setf (gethash #\/ +COMMANDS+) :output)
(setf (gethash #\\ +COMMANDS+) :jump)

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Checks whether the TOKEN represents a valid Braindead command,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash token +COMMANDS+))))))

;;; -------------------------------------------------------

(defun get-command-for-token (token)
  "Returns the Braindead command associated with the TOKEN, or signals
   an error of an unspecified type upon an absence of any correlation."
  (declare (type character token))
  (the command
    (or (gethash token +COMMANDS+)
        (error "No command token: ~s." token))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts and returns from the Braindead SOURCE a one-dimensional
   simple array of commands."
  (declare (type stream source))
  (the (simple-array command (*))
    (coerce
      (loop
        for token
          of-type (or null character)
          =       (read-char source NIL NIL)
        while token
        when (command-token-p token)
          collect (get-command-for-token token))
      '(simple-array command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions memory)
  "Processes the Braindead INSTRUCTIONS, operating on the MEMORY, and
   returns no value."
  (declare (type instruction-vector instructions))
  (declare (type memory             memory))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (pointer     0))
      (declare (type fixnum            ip))
      (declare (type (or null command) instruction))
      (declare (type integer           pointer))
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS vector, if possible, updates the current
             INSTRUCTION, and returns no value."
            (setf instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to (new-position)
            "Moves the instruction pointer IP to the NEW-POSITION,
             updates the current INSTRUCTION, and returns no value."
            (declare (type fixnum new-position))
            (setf ip new-position)
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (valid-command-index-p (index)
            "Checks whether the INDEX represents a valid position into
             the INSTRUCTIONS vector, returning on confirmation a
             ``boolean'' value of ``T'', otherwise ``NIL''."
            (declare (type fixnum index))
            (the boolean
              (not (null
                (array-in-bounds-p instructions index)))))
           
           (current-cell ()
            "Returns the current cell value."
            (the integer
              (gethash pointer memory 0)))
           
           ((setf current-cell) (new-value)
            "Stores the NEW-VALUE in the current cell and returns no
             value."
            (setf (gethash pointer memory 0) new-value)
            (values)))
        
        (loop while instruction do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:move-right
              (incf pointer)
              (incf (current-cell))
              (advance))
            
            (:move-left
              (decf pointer)
              (decf (current-cell))
              (advance))
            
            (:output
              (write-char (code-char (current-cell)))
              (advance))
            
            (:jump
              (if (valid-command-index-p (current-cell))
                (jump-to (current-cell))
                (loop-finish)))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip)))))))
  (values))

;;; -------------------------------------------------------

(defun build-memory (input)
  "Creates and returns a new memory object, its first cell being
   initialized with the INPUT character's code."
  (declare (type character input))
  (let ((memory (make-hash-table :test #'eql)))
    (declare (type memory memory))
    (setf (gethash 0 memory 0) (char-code input))
    (the memory memory)))

;;; -------------------------------------------------------

(defun load-Braindead-file (program-name input)
  "Loads the Braindead program from the file specified by the
   PROGRAM-NAME, utilizing the INPUT for its memory, and returns no
   value."
  (declare (type file-source program-name))
  (declare (type character   input))
  (with-open-file (source-stream program-name
                    :direction         :input
                    :element-type      'character
                    :if-does-not-exist :error)
    (declare (type file-stream source-stream))
    (process-instructions
      (extract-instructions source-stream)
      (build-memory input)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Braindead-string (program input)
  "Interprets the Braindead PROGRAM which utilizes the INPUT for its
   memory, and returns no value."
  (declare (type string    program))
  (declare (type character input))
  (with-input-from-string (source program)
    (declare (type string-stream source))
    (process-instructions
      (extract-instructions source)
      (build-memory input)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Braindead-string "/"
  (prog2
    (format T "~&Please enter a character: ")
    (read-char)
    (clear-input)))

;;; -------------------------------------------------------

;; One-time cat program loaded from a file.
;; Please ensure the availability of the source
;; "one-time-cat-program.bd".
(load-Braindead-file "one-time-cat-program.bd"
  (prog2
    (format T "~&Please enter a character: ")
    (read-char)
    (clear-input)))
