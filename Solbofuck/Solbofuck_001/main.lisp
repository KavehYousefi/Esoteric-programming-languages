;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Solbofuck", presented by the Esolang user "Ganondork" in
;; the year 2017, based upon Urban Mueller's brainfuck whose
;; single-character instructions are substituted by a donat based upon
;; the pattern "BWEE", with the code always preceded by the "ROCK SOLBO"
;; preamble. As an act of supererogation converters betwixt the two
;; specimens are supplied.
;; 
;; 
;; Concept
;; =======
;; Solbofuck represents a derivation of brainfuck, distinguished merely
;; by a renamed instruction set, equinumerant and equipollent in any
;; regard, and, as an adscititious characteristic, always introduced
;; employing the "ROCK SOLBO" preamble.
;; 
;; == VARIANTS OF "BWEE" ENCODE THE COMMANDS ==
;; The language's donat obtains its entheus from the webcomic "Sweet Bro
;; and Hella Jeff", homologating adit, besides the introducing keywords
;; "ROCK SOLBO", exclusively to command identifiers conformant to the
;; forbisen "BWEE", with the dioristic attribute realized in the
;; quantity of "E"s appended to this prefix.
;; 
;; == THE PROGRAM MEMORY: A TAPE OF CELLS ==
;; Solbofuck programs operate on a linear ordonnance of arbitrary
;; integer-valued cells, the structure of which extends along both
;; lateralities into infinity.
;; 
;; A cell pointer selects at any instant the currently active cell, the
;; sole entity amenable to perquisitions and manipulations. Instructions
;; exist to translate this pointer sinistrally and dextrally in
;; graduation.
;; 
;; 
;; Syntax
;; ======
;; Solbofuck code is composed of the introducing sentinel "ROCK SOLBO"
;; and a potentially empty list of space-separated "BWEE" variants.
;; 
;; == INSTRUCTIONS ==
;; A Solbofuck program is always introduced using the keyword twain
;; "ROCK SOLBO", succeeded by zero or more command tokens that assume
;; the form of "BWEE", with betwixt zero and seven additional "E"s
;; directly adhibited. Any two tokens must be separated by at least one
;; whitespace.
;; 
;; == WHITESPACES ==
;; All tokens' interstices must be defined in terms of one or more
;; whitespaces, which encompass spaces, horizontal tabs, and newline
;; characters. Their occurrence at any other location constitutes a fact
;; of tolerance without signification.
;; 
;; == COMMENTS ==
;; No provisions for comments are accommodated in the current language
;; iteration.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form's description shall
;; administer a syntactical nortelry regarding the language:
;; 
;;   program        := optionalSpaces
;;                  ,  prologue
;;                  ,  { separator , command }
;;                  ,  optionalSpaces
;;                  ;
;;   prologue       := "ROCK" , separator , "SOLBO" ;
;;   command        := "BWEE"
;;                  |  "BWEEE"
;;                  |  "BWEEEE"
;;                  |  "BWEEEEE"
;;                  |  "BWEEEEEE"
;;                  |  "BWEEEEEEE"
;;                  |  "BWEEEEEEEE"
;;                  |  "BWEEEEEEEEE"
;;                  ;
;;   optionalSpaces := { whitespace } ;
;;   separator      := whitespace , { whitespace } ;
;;   whitespace     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Its foundment as a trivial brainfuck substitution accommodates
;; Solbofuck with the unmodified octuple instruction set constituting
;; its ancestor's cleronomy.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall empower the reader with a basic
;; comprehension concerning the language's operative facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command     | # of "E"s | Effect
;;   ------------+-----------+-----------------------------------------
;;   BWEE        |     2     | Moves the cell pointer one step to the
;;               |           | right.
;;   ..................................................................
;;   BWEEE       |     3     | Moves the cell pointer one step to the
;;               |           | left.
;;   ..................................................................
;;   BWEEEE      |     4     | Increments the current cell value by
;;               |           | one.
;;   ..................................................................
;;   BWEEEEE     |     5     | Decrements the current cell value by
;;               |           | one.
;;   ..................................................................
;;   BWEEEEEE    |     6     | Prints the character associated with the
;;               |           | current cell value as its ASCII code.
;;   ..................................................................
;;   BWEEEEEEE   |     7     | Queries for an input ASCII character and
;;               |           | stores its character code in the current
;;               |           | cell.
;;   ..................................................................
;;   BWEEEEEEEE  |     8     | If the current cell value equals zero
;;               |           | (0), relocates the instruction pointer
;;               |           | forward to the position immediately
;;               |           | succeeding the matching "BWEEEEEEEEE".
;;               |           | Otherwise, proceeds as usual.
;;   ..................................................................
;;   BWEEEEEEEEE |     9     | If the current cell value does not equal
;;               |           | zero (0), relocates the instruction
;;               |           | pointer back to the position immediately
;;               |           | succeeding the matching "BWEEEEEEEE".
;;               |           | Otherwise, proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == COMMANDS IN SOLBOFUCK AND BRAINFUCK ==
;; The equipollence shared by Solbofuck and brainfuck concludes in the
;; unambiguous association of both following a bidirectional
;; transcription. The respective mapping shall be the coming table's
;; cynosure:
;; 
;;   -----------------------------------
;;   Solbofuck   | # of "E"s | brainfuck
;;   ------------+-----------+----------
;;   BWEE        |     2     | >
;;   ...................................
;;   BWEEE       |     3     | <
;;   ...................................
;;   BWEEEE      |     4     | +
;;   ...................................
;;   BWEEEEE     |     5     | -
;;   ...................................
;;   BWEEEEEE    |     6     | .
;;   ...................................
;;   BWEEEEEEE   |     7     | ,
;;   ...................................
;;   BWEEEEEEEE  |     8     | [
;;   ...................................
;;   BWEEEEEEEEE |     9     | ]
;;   -----------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-25
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Solbofuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized Solbofuck instruction
   variants."
  '(member
    :BWEE
    :BWEEE
    :BWEEEE
    :BWEEEEE
    :BWEEEEEE
    :BWEEEEEEE
    :BWEEEEEEEE
    :BWEEEEEEEEE))

;;; -------------------------------------------------------

(deftype token ()
  "The ``token'' type enumerates the valid tokens expected to be
   encountered during a Solbofuck program's execution.
   ---
   This constitutes a superset of the ``command'' type, which
   additionally embraces the keywords ``:ROCK'', ``:SOLBO'', and the
   end-of-file marker ``:eof''."
  '(or command
       (member :ROCK :SOLBO :eof)))

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

(deftype solbofuck-program ()
  "The ``solbofuck-program'' type defines a program in the Solbofuck
   programming language as a vector of zero or more commands."
  '(vector command *))

;;; -------------------------------------------------------

(deftype parse-state ()
  "The ``parse-state'' type enumerates the states which the process
   responsible for the production of a Solbofuck program from a sequence
   of tokens may assume.
   ---
   A treble distinction justifies the apportionment of membership:
   
     ------------------------------------------------------------------
     Parse state | Description
     ------------+-----------------------------------------------------
     :ROCK       | The token ``:ROCK'' is expected.
     ..................................................................
     :SOLBO      | The token ``:SOLBO'' is expected.
     ..................................................................
     :BWEE       | One of the octuple commands conforming to the
                 | forbisen ``BWEE[...]'' is expected.
     ------------------------------------------------------------------"
  '(member :ROCK :SOLBO :BWEE))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
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

(deftype jump-table ()
  "The ``jump-table'' type defines a mapping of forward jump locations
   to the matching back jump positions in a Solbofuck program, and vice
   versa, manifesting as a hash table whose keys and values both assume
   the fixnum set."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a linear cell arrangement in a sparse
   fashion by associating in a hash table the integer-valued keys with
   values of the same set, the former representing the cell indices, the
   latter the cell values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Checks whether the CANDIDATE represents an identifier constituent,
   that is, an alphabetic entity, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alphanumericp candidate)))))

;;; -------------------------------------------------------

(defun peek-next-character (stream)
  "Returns the next character from the STREAM without its removal, or
   responds with ``NIL'' upon its exhaustion."
  (declare (type stream stream))
  (the (or null character)
    (peek-char NIL stream NIL NIL)))

;;; -------------------------------------------------------

(defun read-next-character (stream)
  "Consumes, removes and returns the next character from the STREAM, or
   responds with ``NIL'' upon its exhaustion."
  (declare (type stream stream))
  (the (or null character)
    (read-char stream NIL NIL)))

;;; -------------------------------------------------------

(defun whitespace-follows-p (stream)
  "Checks whether the next character from the STREAM, obtained without
   its removal, represents a whitespace, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type stream stream))
  (let ((next-character (peek-next-character stream)))
    (declare (type (or null character) next-character))
    (the boolean
      (not (null
        (and next-character
             (whitespace-character-p next-character)))))))

;;; -------------------------------------------------------

(defun identifier-follows-p (stream)
  "Checks whether the next character from the STREAM, obtained without
   its removal, represents an identifier token's constituent, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type stream stream))
  (let ((next-character (peek-next-character stream)))
    (declare (type (or null character) next-character))
    (the boolean
      (not (null
        (and next-character
             (identifier-character-p next-character)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (stream)
  "Skips a sequence of zero or more adjacent whitespace characters in
   the STREAM and returns the contingently modified STREAM."
  (declare (type stream stream))
  (loop while (whitespace-follows-p stream) do
    (read-next-character stream))
  (the stream stream))

;;; -------------------------------------------------------

(defun read-identifier (stream)
  "Consumes an identifier, being a composite of one or more alphabetic
   characters, from the STREAM and returns the same."
  (declare (type stream stream))
  (the string
    (with-output-to-string (identifier)
      (declare (type string-stream identifier))
      (loop while (identifier-follows-p stream) do
        (write-char (read-next-character stream) identifier)))))

;;; -------------------------------------------------------

(defun parse-identifier (identifier)
  "Parses the IDENTIFIER string and returns a token representation
   thereof.
   ---
   An error of an unspecified type is signaled if the IDENTIFIER cannot
   be recognized."
  (declare (type string identifier))
  (let ((identifier-keyword (intern identifier :keyword)))
    (declare (type keyword identifier-keyword))
    (the token
      (if (typep identifier-keyword 'token)
        identifier-keyword
        (error "Invalid identifier: ~s." identifier)))))

;;; -------------------------------------------------------

(defun get-next-token (stream)
  "Returns the next token from the STREAM.
   ---
   Upon the STREAM's exhaustion, this operation responds to any further
   request with the ``:eof'' sentinel."
  (declare (type stream stream))
  (let ((next-character (peek-next-character stream)))
    (declare (type (or null character) next-character))
    (the token
      (cond
        ((null next-character)
          :eof)
        ((whitespace-character-p next-character)
          (skip-whitespaces stream)
          (get-next-token stream))
        ((identifier-character-p next-character)
          (parse-identifier
            (read-identifier stream)))
        (T
          (error "Invalid character ~s." next-character))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (stream)
  "Extracts from the STREAM a sequence of zero or more Solbofuck
   instructions and returns these in a one-dimesional simple array of
   ``command'' objects."
  (declare (type stream stream))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (let* ((states        (list :ROCK :SOLBO :BWEE))
           (current-state (first states)))
      (declare (type (list-of parse-state) states))
      (declare (type parse-state           current-state))
      
      (loop for token of-type token = (get-next-token stream) do
        (case current-state
          ;; Expecting the "ROCK" keyword.
          (:ROCK
            (case token
              (:ROCK
                (pop states)
                (setf current-state (first states)))
              (otherwise
                (error "Expected the keyword ROCK, but encountered ~
                        the command ~s."
                  token))))
          
          ;; Expecting the "SOLBO" keyword.
          (:SOLBO
            (case token
              (:SOLBO
                (pop states)
                (setf current-state (first states)))
              (otherwise
                (error "Expected the keyword SOLBO, but encountered ~
                        the command ~s."
                  token))))
          
          ;; Expecting a command conforming to the "BWEE..." pattern.
          (:BWEE
            (case token
              (:eof
                (loop-finish))
              (:ROCK
                (error "Encountered the ROCK keyword at an ~
                        invalid position."))
              (:SOLBO
                (error "Encountered the SOLBO keyword at an ~
                        invalid position."))
              (otherwise
                (push token instructions))))
          
          ;; Invalid parse state.
          (otherwise
            (error "Invalid parse state: ~s." current-state)))))
    
    (the (simple-array command (*))
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class represents a linear ordonnance of theoretically
   infinite cells on a bilaterally unbounded tape, each component of
   which stores a signed scalar integer, initially zero (0), with the
   current instance being designated by a mobile cell pointer."
  (cells   (make-hash-table :test #'eql) :type cell-table)
  (pointer 0                             :type integer))

;;; -------------------------------------------------------

(defun memory-move-right (memory)
  "Moves the MEMORY's cell pointer one step to the right and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-move-left (memory)
  "Moves the MEMORY's cell pointer one step to the left and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-current-cell (memory)
  "Returns the MEMORY's current cell value."
  (declare (type Memory memory))
  (the integer
    (gethash (memory-pointer memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf memory-current-cell) (new-value memory)
  "Stores in the MEMORY's current cell the NEW-VALUE and returns the
   modified MEMORY."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory) (memory-cells memory) 0)
        new-value)
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-increment (memory)
  "Increments the MEMORY's current cell value by one and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (incf (memory-current-cell memory))
  (the Memory memory))

;;; -------------------------------------------------------

(defun memory-decrement (memory)
  "Decrements the MEMORY's current cell value by one and returns the
   modified MEMORY."
  (declare (type Memory memory))
  (decf (memory-current-cell memory))
  (the Memory memory))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Builds and returns a jump table for the Solbofuck INSTRUCTIONS,
   associating each forward jump instruction's location with the
   position of the matching back jump operation, and vice versa."
  (declare (type solbofuck-program instructions))
  (let ((jump-table             (make-hash-table :test #'eql))
        (forward-jump-positions NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) forward-jump-positions))
    (loop
      for instruction of-type command across instructions
      and position    of-type fixnum  from   0 by 1
      do
        (case instruction
          ;; Forward jump.
          (:BWEEEEEEEE
            (push position forward-jump-positions))
          ;; Back jump.
          (:BWEEEEEEEEE
            (cond
              (forward-jump-positions
                (let ((start (pop forward-jump-positions))
                      (end   position))
                  (declare (type fixnum start))
                  (declare (type fixnum end))
                  (setf (gethash start jump-table) end)
                  (setf (gethash end   jump-table) start)))
              (T
                (error "Unmatched back jump at position ~d."
                  position))))
          (otherwise
            NIL)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Executes the Solbofuck INSTRUCTIONS and returns no value."
  (declare (type solbofuck-program instructions))
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0))
          (jump-table          (build-jump-table instructions))
          (memory              (make-memory)))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      (declare (type jump-table        jump-table))
      (declare (type Memory            memory))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next position in
             the INSTRUCTIONS vector, if possible, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (jump-to-opposite-boundary ()
            "Expecting to be located at a forward or back jump
             instruction, relocates the instruction pointer IP to the
             matching opposite boundary, updates the
             CURRENT-INSTRUCTION, and returns no value."
            (setf ip (gethash ip jump-table))
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values)))
        
        (loop while current-instruction do
          (case current-instruction
            ;; End of program.
            ((NIL)
              (loop-finish))
            
            ;; Move cell pointer one step right.
            (:BWEE
              (memory-move-right memory))
            
            ;; Move cell pointer one step left.
            (:BWEEE
              (memory-move-left memory))
            
            ;; Increment the current cell by one.
            (:BWEEEE
              (memory-increment memory))
            
            ;; Decrement the current cell by one.
            (:BWEEEEE
              (memory-decrement memory))
            
            ;; Output current cell value's ASCII character.
            (:BWEEEEEE
              (write-char
                (code-char
                  (memory-current-cell memory))))
            
            ;; Input character and store its ASCII code in current cell.
            (:BWEEEEEEE
              (format T "~&Please input an ASCII character: ")
              (setf (memory-current-cell memory)
                    (char-code (read-char)))
              (clear-input))
            
            ;; Jump forward.
            (:BWEEEEEEEE
              (when (zerop (memory-current-cell memory))
                (jump-to-opposite-boundary)))
            
            ;; Jump back.
            (:BWEEEEEEEEE
              (unless (zerop (memory-current-cell memory))
                (jump-to-opposite-boundary)))
            
            ;; Unrecognized command.
            (otherwise
              (error "Invalid command ~s at position ~d."
                current-instruction ip)))
          
          (advance)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Solbofuck (code)
  "Interprets the piece of Solbofuck source CODE and returns no value."
  (declare (type string code))
  (with-input-from-string (input code)
    (declare (type string-stream input))
    (process-instructions
      (extract-instructions input)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Solbofuck-to-brainfuck converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-Solbofuck-program-to-brainfuck (solbofuck-instructions
                                               &key (destination NIL))
  "Generates for the SOLBOFUCK-INSTRUCTIONS the brainfuck equivalent and
   writes the thus produced source code to the DESTINATION, returning
   for a non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding
   with a fresh string comprehending the result."
  (declare (type solbofuck-program solbofuck-instructions))
  (declare (type destination       destination))
  (the (or null string)
    (if destination
      (loop
        for instruction of-type command across solbofuck-instructions
        and position    of-type fixnum  from   0 by 1
        do
          (format destination "~c"
            (case instruction
              (:BWEE #\>)
              (:BWEEE #\<)
              (:BWEEEE #\+)
              (:BWEEEEE #\-)
              (:BWEEEEEE #\.)
              (:BWEEEEEEE #\,)
              (:BWEEEEEEEE #\[)
              (:BWEEEEEEEEE #\])
              (otherwise
                (error "Invalid Solbofuck instruction ~s at ~
                        position ~d."
                  instruction position)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-Solbofuck-program-to-brainfuck solbofuck-instructions
          :destination output)))))

;;; -------------------------------------------------------

(defun convert-Solbofuck-code-to-brainfuck (solbofuck-code
                                            &key (destination NIL))
  "Generates for the SOLBOFUCK-CODE the brainfuck equivalent and writes
   the thus produced source code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type string      solbofuck-code))
  (declare (type destination destination))
  (the (or null string)
    (convert-Solbofuck-program-to-brainfuck
      (with-input-from-string (input solbofuck-code)
        (declare (type string-stream input))
        (extract-instructions input))
      :destination destination)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Solbofuck converter.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun brainfuck-command-p (token)
  "Checks whether the TOKEN represents a brainfuck command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character token))
  (the boolean
    (not (null
      (find token "><+-.,[]" :test #'char=)))))

;;; -------------------------------------------------------

(defun convert-brainfuck-to-Solbofuck-code (brainfuck-code
                                            &key (destination NIL))
  "Generates for the BRAINFUCK-CODE the Solbofuck equivalent and writes
   the thus produced source code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        initially
          (format destination "ROCK SOLBO")
        for brainfuck-token
          of-type character
          across  brainfuck-code
        with first-command-p
          of-type boolean
          =       T
        do
          (when (brainfuck-command-p brainfuck-token)
            (cond
              (first-command-p
                (setf first-command-p NIL)
                (format destination "~%"))
              (T
                (format destination " ")))
            (format destination "~a"
              (case brainfuck-token
                (#\> "BWEE")
                (#\< "BWEEE")
                (#\+ "BWEEEE")
                (#\- "BWEEEEE")
                (#\. "BWEEEEEE")
                (#\, "BWEEEEEEE")
                (#\[ "BWEEEEEEEE")
                (#\] "BWEEEEEEEEE")))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-Solbofuck-code brainfuck-code
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!".
(interpret-Solbofuck
  "ROCK SOLBO
 BWEEEE BWEEEE BWEEEE BWEEEE BWEEEE BWEEEE BWEEEE BWEEEE BWEEEEEEEE BWEE BWEEEE BWEEEE BWEEEE BWEEEE  BWEEEEEEEE BWEE BWEEEE BWEEEE BWEE BWEEEE BWEEEE BWEEEE BWEE BWEEEE BWEEEE BWEEEE BWEE BWEEEE BWEEE BWEEE  BWEEE BWEEE BWEEEEE BWEEEEEEEEE BWEE BWEEEE BWEE BWEEEE BWEE BWEEEEE BWEE BWEE BWEEEE BWEEEEEEEE BWEEE  BWEEEEEEEEE BWEEE BWEEEEE BWEEEEEEEEE BWEE BWEE BWEEEEEE BWEE BWEEEEE BWEEEEE BWEEEEE BWEEEEEE BWEEEE BWEEEE  BWEEEE BWEEEE BWEEEE BWEEEE BWEEEE BWEEEEEE BWEEEEEE BWEEEE BWEEEE BWEEEE BWEEEEEE BWEE BWEE  BWEEEEEE BWEEE BWEEEEE BWEEEEEE BWEEE BWEEEEEE BWEEEE BWEEEE BWEEEE BWEEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE  BWEEEEE BWEEEEE BWEEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEE BWEEEEEE  BWEE BWEE BWEEEE BWEEEEEE BWEE BWEEEE BWEEEE BWEEEEEE")

;;; -------------------------------------------------------

;; An infinitely repeating cat program.
(interpret-Solbofuck
  "ROCK SOLBO
   BWEEEEEEE BWEEEEEE BWEEEEEEEE BWEEEEEEE BWEEEEEE BWEEEEEEEEE")

;;; -------------------------------------------------------

;; Convert the infinitely repeating cat program from Solbofuck to
;; brainfuck and write the result to the standard output:
;;   ,.[,.]
(convert-Solbofuck-code-to-brainfuck
  "ROCK SOLBO
   BWEEEEEEE BWEEEEEE BWEEEEEEEE BWEEEEEEE BWEEEEEE BWEEEEEEEEE"
  :destination T)

;;; -------------------------------------------------------

;; Generate from the brainfuck source the Solbofuck code for a
;; "Hello World!" printer.
(convert-brainfuck-to-Solbofuck-code
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

;;; -------------------------------------------------------

;; Generate from the brainfuck source the Solbofuck code for a
;; "Hello World!" printer and execute it.
(interpret-Solbofuck
  (convert-brainfuck-to-Solbofuck-code
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."))
