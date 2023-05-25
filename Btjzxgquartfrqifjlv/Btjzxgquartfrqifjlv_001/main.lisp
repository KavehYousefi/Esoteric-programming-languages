;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Btjzxgquartfrqifjlv", presented by the Esolang user
;; denoted by the identification "160.85.232.227" in the year 2015, and
;; intended as a syntactical reformulation of Urban Mueller's
;; "brainfuck" programming language, with the eight instruction tokens
;; substituted by letters from the string "Btjzxgquartfrqifjlv",
;; pursuing an eath transmission via Morse code.
;; 
;; 
;; Concept
;; =======
;; The Btjzxgquartfrqifjlv programming language is founded upon
;; brainfuck, appropriating its entire concepts verbatim, while merely
;; employing a different set of characters for the command tokens'
;; representation, the selection of the same relays to the requirements
;; of Morse code communication.
;; 
;; The command identifier octuple, accompanied by the Morse codes, shall
;; be adduced:
;; 
;;   -----------------------------------
;;   Command token | Morse code sequence
;;   --------------+--------------------
;;   btj           | -... - .---
;;   zxg           | --.. -..- --.
;;   qua           | --.- ..- .-
;;   rtf           | .-. - ..-.
;;   rqi           | .-. --.- ..
;;   f             | ..-.
;;   j             | .---
;;   lv            | .-.. ...-
;;   -----------------------------------
;; 
;; 
;; Architecture
;; ============
;; Btjzxgquartfrqifjlv subscribes to the native tenets of its brainfuck
;; ancestor, maintaining a linear sequence of unsigned-byte-valued
;; cells, admitting the integer range of [0, 255], however, not
;; necessitated to accommodate the fixed 30,000 in tally, nor
;; constrained to non-negative indices.
;; 
;; 
;; Instructions
;; ============
;; Btjzxgquartfrqifjlv's cleronomy apportions to it the exact eight
;; instructions commorant in its inspiration, brainfuck; expressed
;; simply in a more elaborate guise.
;; 
;; == OVERVIEW ==
;; An apercu endowed with compendiousness shall educate about the octet
;; of instructions furnished to the language.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   f       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   rqi     | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   qua     | Increments the current cell value by one.
;;           | If transcending the upper bound of 255, the value is
;;           | wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   rtf     | Decrements the current cell value by one.
;;           | If transcending the lower bound of zero (0), the value
;;           | is wrapped around to the maximum of 255.
;;   ..................................................................
;;   j       | Queries the user for an ASCII character and stores its
;;           | ASCII code in the current cell.
;;   ..................................................................
;;   lv      | Prints to the standard output the character
;;           | corresponding to the current cell value when construed
;;           | an ASCII code.
;;   ..................................................................
;;   btj     | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   zxg     | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "o". Otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK-EQUIVALENCY ==
;; The fact of its direct equivalency with brainfuck permits an
;; unambiguous juxtaposition regarding Btjzxgquartfrqifjlv's and its
;; stock-father's command tokens:
;; 
;;   --------------------------------------------------
;;   Btjzxgquartfrqifjlv command | brainfuck equivalent
;;   ----------------------------+---------------------
;;   btj                         | [
;;   ..................................................
;;   zxg                         | ]
;;   ..................................................
;;   qua                         | +
;;   ..................................................
;;   rtf                         | -
;;   ..................................................
;;   rqi                         | <
;;   ..................................................
;;   f                           | >
;;   ..................................................
;;   j                           | ,
;;   ..................................................
;;   lv                          | .
;;   --------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Btjzxgquartfrqifjlv's perfect congruency with brainfuck ascertains
;; its disencumbrance from most ambiguities. A single instance, however,
;; may be levied to disquisition.
;; 
;; == WHICH CONCRETE DESIGN APPLIES TO THE MEMORY? ==
;; Merely the cell value model, ramose in the variations appertaining to
;; brainfuck's construe in modern contexts, tharfs a more explicit
;; treatise. Its has been adjudged, and ascertained by its aefauld
;; "Hello World" example, that each cell maintains an unsigned octet in
;; the range [0, 255], wrapping its value around to the opposite
;; extremum if transgressing one of its marches. The ensconcing tape
;; is unbounded along both lateralities.
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp ostends its diorism in the
;; employment of macros in constructing a list of S-expressions
;; capacitated with the actual evaluation of the thus generated code by
;; the Lisp interpreter itself.
;; 
;; == THE LEXER AND PARSER ESTABLISH AN S-EXPRESSIONS FACTORY ==
;; Proceeding from the general nature of a parser's output, the project
;; at hand adhibits the offered competences to directly assemble a piece
;; of Common Lisp code, which is subsequently evaluated by the Lisp
;; interpreter itself.
;; 
;; In lieu of an intermediate representation, which most likely assumes
;; the mold of an abstract syntax tree (AST) or a simple vector of
;; instructions, the lexer and parser in their coefficiacy transform the
;; input string into S-expressions, amenable to immediate execution when
;; inducted into a macro's context.
;; 
;; A conceived solution in concord with the traditional concept of a
;; custom interpreter succeeding the lexical analyzation and parsing
;; stages is illustrated below:
;; 
;;   +----------------------------------------+
;;   | Btjzxgquartfrqifjlv source code string |
;;   +----------------------------------------+
;;                       | 
;;                       | Lexical analyzation and parsing
;;                       | 
;;                       V
;;   +----------------------------------------+
;;   | Intermediate representation            |
;;   | (AST or instruction vector)            |
;;   +----------------------------------------+
;;                       | 
;;                       | Interpretation (by custom interpreter)
;;                       | 
;;                       V
;;   +----------------------------------------+
;;   | Executed program                       |
;;   +----------------------------------------+
;; 
;; The bartery of the intermediate representation (stage 2) for direct
;; S-expressions permits the obviation of many efforts accompanying the
;; instructions' evaluation, as the Lisp interpreter itself operates on
;; the few facilities requisite for its acquainted duties.
;; 
;;   +----------------------------------------+
;;   | Btjzxgquartfrqifjlv source code string |
;;   +----------------------------------------+
;;                       | 
;;                       | Lexical analyzation and parsing
;;                       | 
;;                       V
;;   +----------------------------------------+
;;   | Common Lisp S-expressions              |
;;   +----------------------------------------+
;;                       | 
;;                       | Interpretation (by Lisp interpreter itself)
;;                       | 
;;                       V
;;   +----------------------------------------+
;;   | Executed program                       |
;;   +----------------------------------------+
;; 
;; == HOMOICONICITY: CODE = DATA = CODE = ... ==
;; Homoiconicity as a language characteristic refers to the concomitant
;; disposition of a data structure for both data storage purposes and
;; source code representation. In the case of Lisp, the "LISt Processing
;; Language", the linked list partakes of this twifaced nature.
;; 
;; An example shall aide in the principle's illustration: Given the
;; list of three elements "+", "1", and "2", which would be rendered in
;; visual terms as
;; 
;;   (+ 1 2)
;; 
;; a derivation of code from data shall be extended. The graphical mode
;; presented aboon, as already stated, actual refers to a list:
;; 
;;   (list '+ 1 2)
;; 
;; or, as the quoting mechanism homologates an apostrophe ("'") to
;; abbreviate the "quote" special operator, the following, less common,
;; equivalency presides:
;; 
;;   (list (quote +) 1 2)
;; 
;; Alternatively, in this case, the whole expression may be quoted:
;; 
;;   (quote (+ 1 2))
;; 
;; This produces a literal --- an object not intended for modification.
;; Iterum, it holds for the curtailed design:
;; 
;;   '(+ 1 2)
;; 
;; If we now elide the preceding single quote --- or the ensconcing
;; "quote" special operator in the former code fragement ---, the form
;; 
;;   (+ 1 2)
;; 
;; is yielded. The absence of a quoting mark incites in the Lisp
;; interpreter the desire to evaluate this desinent form, producing an
;; addition and ultimately yielding the value 3 (= 1 + 2). The first
;; element, "+", is interpreted as an operation identifier, with any
;; subsequent items contributing its arguments:
;; 
;;    +------ Invoke the operation with the name "+" (addition) ...
;;    |
;;    | +------ ... with the number "1" as the first argument ...
;;    | |
;;    | | +------ ... and the number "2" as the second argument.
;;    | | |
;;    V V V
;;   (+ 1 2)
;; 
;; In very simple terms: An expression without quotation is evaluated as
;; code; a piece of code with quotation is rendered as data. In
;; corollary, code and data "look alike".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-24
;; 
;; Sources:
;;   [esolang2015Btjzxgquartfrqifjlv]
;;   The Esolang contributors, "Btjzxgquartfrqifjlv", 2015
;;   URL: "https://esolangs.org/wiki/Btjzxgquartfrqifjlv"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized Btjzxgquartfrqifjlv
   commands."
  '(member
    :jump-forward
    :jump-back
    :increment
    :decrement
    :move-left
    :move-right
    :input
    :output))

;;; -------------------------------------------------------

(deftype alist-of (&optional (key-type T) (value-type T))
  "The ``alist-of'' type defines an association list, or alist, composed
   of zero or more entries, each of which constitutes a cons whose
   sinistral department entails a key of the KEY-TYPE, associated with a
   value of the VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element `(cons ,key-type ,value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   conforming to the ELEMENT-TYPE, which defaults to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype s-expression ()
  "The ``s-expression'' type defines a Common Lisp S-expression as an
   arbitrary object, committed to the comprehensive ``T'' type."
  'T)

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight adjacent
   bits, thus commorant in the integer range [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
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

(deftype memory ()
  "The ``memory'' type defines the program memory as a sparse vector of
   unsigned-byte-valued cells, amenable to arbitrary integer indices,
   implemented as a hash table that maps integer keys to ``octet''
   values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (alist-of string command) +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+
  '(("btj" . :jump-forward)
    ("zxg" . :jump-back)
    ("qua" . :increment)
    ("rtf" . :decrement)
    ("rqi" . :move-left)
    ("f"   . :move-right)
    ("j"   . :input)
    ("lv"  . :output))
  "Associates the command tokens with identifying keyword symbols.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 7) +COMMAND-PREFIXES+))

;;; -------------------------------------------------------

(defparameter +COMMAND-PREFIXES+ "bfjlqrz"
  "Maintains the characters which might commence a Btjzxgquartfrqifjlv
   command identifier.")

;;; -------------------------------------------------------

(defun command-prefix-p (candidate)
  "Determines whether the CANDIDATE represents the first character of a
   Btjzxgquartfrqifjlv command identifier, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate +COMMAND-PREFIXES+ :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun probe-string (source start subject)
  "Determines whether, commencing at the START position in the SOURCE,
   and anchored at this location, the SUBJECT string occurs, returning
   in dependence on the result two values:
     (1) If the SUBJECT has been located, the ``boolean'' ``T'' value;
         otherwise ``NIL''.
     (2) If the SUBJECT has been located, the position in the SOURCE
         immediately succeeding the matching portion; otherwise the
         START position itself."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type string subject))
  (the (values boolean fixnum)
    (let ((end (+ start (length subject))))
      (declare (type fixnum end))
      (cond
        ((> end (length source))
          (values NIL start))
        ((search subject source :start2 start :end2 end)
          (values T end))
        (T
          (values NIL start))))))

;;; -------------------------------------------------------

(defun probe-command (source start)
  "Determines whether, commencing at the START position in the SOURCE,
   and anchored at this location, a command occurs, returning in
   dependence on the result two value:
     (1) If a command has been located, the command keyword; otherwise
         the ``NIL'' value.
     (2) If a command has been located, the position in the SOURCE
         immediately succeeding the matching portion; otherwise the
         START position itself."
  (the (values (or null command) fixnum)
    (loop
      for command-entry of-type (cons string command) in +COMMANDS+
      do
        (multiple-value-bind (command-found-p end-position)
            (probe-string source start (car command-entry))
          (declare (type boolean command-found-p))
          (declare (type fixnum  end-position))
          (when command-found-p
            (return (values (cdr command-entry) end-position))))
      finally
        (return (values NIL start)))))

;;; -------------------------------------------------------

(defun skip-comments (source start)
  "Commencing at the START position in the SOURCE, returns the position
   of the nearest character which might incite a Btjzxgquartfrqifjlv
   command identifier, or responds with the length of the SOURCE if none
   such occurs in the perquired segment."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'command-prefix-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun extract-commands (source)
  "Extracts and returns from the SOURCE a one-dimensional simple array
   of its Btjzxgquartfrqifjlv commands."
  (declare (type string source))
  (let ((commands NIL)
        (position 0))
    (declare (type list   commands))
    (declare (type fixnum position))
    (flet
        ((source-exhausted-p ()
          "Determines whether the SOURCE is exhausted, that is, its
           POSITION cursor has transcended the valid bounds, returning
           on confirmation a ``boolean'' value of ``T'', otherwise
           ``NIL''."
          (the boolean
            (not (null
              (>= position (length source))))))
         
         (collect-command (command new-position)
          "If the COMMAND is non-``NIL'', inserts the same at the front
           of the COMMANDS list and updates the NEW-POSITION, otherwise
           simply increments the POSITION by one, in any case returning
           no value."
          (declare (type (or null command) command))
          (declare (type fixnum            new-position))
          (cond
            (command
              (push command commands)
              (setf position new-position))
            (T
              (incf position)))
          (values)))
      
      (loop until (source-exhausted-p) do
        (setf position (skip-comments source position))
        (multiple-value-call #'collect-command
          (probe-command source position))))
    
    (the (simple-array command (*))
      (coerce (nreverse commands)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Common Lisp code generator.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (Lisp-Converter) s-expression)
                transform-command))

;;; -------------------------------------------------------

(defstruct (Lisp-Converter
  (:constructor make-lisp-converter (commands)))
  "The ``Lisp-Converter'' class is endowed with the castaldy of a
   Btjzxgquartfrqifjlv command sequence's transformation into Common
   Lisp S-expressions, maintaining a cursor to the currently processed
   operation."
  (commands (error "Missing commands.") :type (vector command *))
  (ip       0                           :type fixnum))

;;; -------------------------------------------------------

(defun lisp-converter-current-command (converter)
  "Returns the command located at the Lisp CONVERTER's current position,
   or ``NIL'' if the same has transcended its command vector's
   boundaries."
  (declare (type Lisp-Converter converter))
  (the (or null command)
    (when (array-in-bounds-p (lisp-converter-commands converter)
            (lisp-converter-ip converter))
      (aref (lisp-converter-commands converter)
        (lisp-converter-ip converter)))))

;;; -------------------------------------------------------

(defun transform-loop (converter)
  "Transforms the forward jump command at the Lisp CONVERTER's current
   position into a piece of Common Lisp code and returns the same."
  (declare (type Lisp-Converter converter))
  ;; Skip the identifying ``:jump-forward'' command.
  (incf (lisp-converter-ip converter))
  (let ((loop-body NIL))
    (declare (type (list-of s-expression) loop-body))
    (loop do
      (case (lisp-converter-current-command converter)
        ((NIL)
          (error "Unterminated loop."))
        
        (:jump-back
          (incf (lisp-converter-ip converter))
          (loop-finish))
        
        (otherwise
          (push (transform-command converter) loop-body))))
    
    (the s-expression
      `(loop until (zerop current-cell) do
         ,@(or (nreverse loop-body)
               '('()))))))

;;; -------------------------------------------------------

(defun transform-command (converter)
  "Transforms the current command maintained by the Lisp CONVERTER into
   an S-expression and returns the same."
  (declare (type Lisp-Converter converter))
  (the s-expression
    (case (lisp-converter-current-command converter)
      ((NIL)
        NIL)
      
      (:increment
        (prog1
          `(incf current-cell)
          (incf (lisp-converter-ip converter))))
      
      (:decrement
        (prog1
          `(decf current-cell)
          (incf (lisp-converter-ip converter))))
      
      (:move-right
        (prog1
          `(incf cell-pointer)
          (incf (lisp-converter-ip converter))))
      
      (:move-left
        (prog1
          `(decf cell-pointer)
          (incf (lisp-converter-ip converter))))
      
      (:input
        (prog1
          `(progn
             (format T "~&>> ")
             (setf current-cell
               (prog1
                 (char-code (read-char))
                 (clear-input))))
          (incf (lisp-converter-ip converter))))
      
      (:output
        (prog1
          `(write-char (code-char current-cell))
          (incf (lisp-converter-ip converter))))
      
      (:jump-forward
        (transform-loop converter))
      
      (:jump-back
        (error "Unmatched back jump instruction."))
      
      (otherwise
        (error "Invalid command ~s at position ~d."
          (lisp-converter-current-command converter)
          (lisp-converter-ip              converter))))))

;;; -------------------------------------------------------

(defun transform-commands (converter)
  "Returns a list of S-expressions obtained by transforming each command
   under the Lisp CONVERTER's castaldy into one or more Common Lisp
   forms."
  (declare (type Lisp-Converter converter))
  (let ((lisp-code NIL))
    (declare (type (list-of s-expression) lisp-code))
    (loop do
      (case (lisp-converter-current-command converter)
        ((NIL)
          (loop-finish))
        
        (:jump-forward
          (push (transform-loop converter) lisp-code))
        
        (:jump-back
          (error "Unmatched back jump instruction."))
        
        (otherwise
          (push (transform-command converter) lisp-code))))
    
    (the (list-of s-expression)
      (nreverse lisp-code))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro interpret-Btjzxgquartfrqifjlv (code)
  "Interprets the piece of Btjzxgquartfrqifjlv source CODE and returns
   no value."
  `(let ((memory       (make-hash-table :test #'eql))
         (cell-pointer 0))
     (declare (type memory  memory))
     (declare (type integer cell-pointer))
     (flet
         ((access-current-cell ()
           "Returns the value of the MEMORY cell at the CELL-POINTER."
           (the octet
             (gethash cell-pointer memory 0)))
          
          ((setf access-current-cell) (new-value)
           "Stores the NEW-VALUE in the MEMORY cell at the CELL-POINTER,
            contingently preceded by a wrapping into the unsigned byte
            range [0, 255], and returns the final value."
           (declare (type integer new-value))
           (setf (gethash cell-pointer memory 0)
                 (mod new-value 256))
           (the octet
             (gethash cell-pointer memory 0))))
       
       (symbol-macrolet
           ((current-cell
             (the integer
               (access-current-cell))))
         (declare (type integer current-cell))
         (declare (ignorable    current-cell))
         
         ,@(transform-commands
             (make-lisp-converter
               (extract-commands code)))))
     
     (values)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Btjzxgquartfrqifjlv
  "quabtjrtfrtffrtfbtjffquafrtfrtfrtfrtfrtfrqirqizxgrqirtfrtfrqirtfrtfrtfzxgfrtflvfffqualvfflvlvquaquaquabtjlvfzxgrqirqirqirqilvquaquaqualvrtfrtfrtfrtfrtfrtflvrqirqirtflvffffqualv")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates if confronted with
;; the null character.
(interpret-Btjzxgquartfrqifjlv "j lv btj j lv zxg")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Btjzxgquartfrqifjlv "jlvbtjrtfrtffquabtjffzxgrqibtjlvzxgrqirqizxg")
