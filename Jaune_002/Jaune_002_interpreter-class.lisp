;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple parser and interpreter for the
;; esoteric programming language "Jaune" invented by "CoffeeHax17".
;; 
;; The Jaune Programming Language
;; ==============================
;; The grammar of the language can be described in the extended
;; Backus-Naur form (EBNF) notation as follows:
;; 
;;    program        := mainProgram, [ subroutines ] ;
;;    mainProgram    := "."
;;                    | command, { command }, "." ;
;;    subroutines    := subroutine, { subroutine } ;
;;    subroutine     := command, { command }, ";" ;
;;    command        := "^"
;;                    | ">"
;;                    | "<"
;;                    | "#"
;;                    | "&"
;;                    | "%"
;;                    | numericCommand ;
;;    numericCommand := number, "+"
;;                    | number, "-"
;;                    | number, ":"
;;                    | number, "?"
;;                    | number, "!"
;;                    | number, "$"
;;                    | number, "@" ;
;;    number         := literalNumber | "v" ;
;;    literalNumber  := [ "+" | "-" ], digit, { digit } ;
;;    digit          := "0" | "1" | "2" | "3" | "4"
;;                    | "5" | "6" | "7" | "8" | "9" ;
;; 
;; Note that the implementation, as well as the aforementioned grammar,
;; are subject to a set of assumptions stemming from some lacunae in the
;; documentation. Especially, but not exclusively, the following
;; premises are imposed:
;;   (1) Any character not explicitly listed in the alphabet is not
;;       tolerated; this includes, last but not least, white spaces.
;;       A constraint as such reserves the capacity for further
;;       extensions in the form of yet unused command tokens.
;;   (2) Characters valid as enumerated may be placed arbitrarily, as
;;       long as their presence is as ineffectuous and negligible as
;;       nondetrimental. For instance, the program
;;         5^.
;;       which bears no significance concerning the literal number 5, is
;;       yet tolerated.
;;   (3) Numeric values are restricted to signed integer numbers,
;;       unbounded in their range. An extension to floating-point
;;       numbers is, at the moment at least, encumbered by the
;;       significance of the dot ('.') character as the main program
;;       terminator.
;; 
;; 
;; Implementation
;; ==============
;; The program operates upon a memory implemented as a fixed-size vector
;; of signed integer numbers, initialized to an arbitrarily default
;; length. At any time, an unsigned integer pointer refers to the
;; currently active cell, or position, in this memory vector. If ordered
;; to transgress the memory's length, the vector is replaced by a new
;; one, scaled by a single cell, while maintaining the already extant
;; content.
;; 
;; This implementation forgoes several error checks, including that
;; concerning the omission of the concluding dot ('.') or
;; semicolon (';'), for the sake of simplicity.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-05-14
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Jaune"
;;       o Documents the Jaune programming language.
;;   -> "https://esolangs.org/wiki/Truth-machine"
;;       o Describes the truth-machine, also listing examples.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-table-of-p (object key-type value-type)
  "Checks whether the OBJECT represents a hash table whose every key
   matches the KEY-TYPE and whose every associated value matches the
   VALUE-TYPE."
  (and (hash-table-p object)
       (loop
         for    key being the hash-keys in (the hash-table object)
         using  (hash-value value)
         always (and (typep key   key-type)
                     (typep value value-type)))))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional key-type value-type)
  "Defines a hash table whose keys all match the KEY-TYPE and whose
   values are of the VALUE-TYPE, both of which default to an unspecified
   ``*'' symbol in order to tolerate any object."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (hash-table-of-p object key-type value-type)))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype stack ()
  "A stack is described as homologous to the native list type, albeit
   explicitly intended to be employed in a last-in-first-out (LIFO)
   mode."
  'list)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 1 *) +INITIAL-MEMORY-SIZE+))

;;; -------------------------------------------------------

(defconstant +INITIAL-MEMORY-SIZE+ 30000
  "The initial size of the memory array. Its magnitude is chosen rather
   arbitrarily to reflect the brainfuck language's default. However, any
   integer value greater or equal to one (1) carries validity. Please
   bear in mind that the underlying memory array can enumerate its
   elements merely inside of the range of the ``fixnum'' type, which
   might be far less than the ``integer'' range. Refer to the
   Common Lisp constant variable ``array-total-size-limit'' to obtain
   the actual maximum number of elements that an array may hold.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((memory
    :initarg       :memory
    :initform      (make-array +INITIAL-MEMORY-SIZE+
                     :element-type    'integer
                     :initial-element 0)
    :type          (vector integer *)
    :documentation "The memory is implemented as a vector of integer
                    number. Not being explicityly resizable, its length
                    can be increased by replacing the array by a larger
                    one.")
   (pointer
    :initarg       :pointer
    :initform      0
    :type          fixnum
    :documentation "The index of the currently active MEMORY cell. If
                    the index transgresses the MEMORY's upper bound,
                    the latter will be resized.")
   (hold-cell
    :initarg       :hold-cell
    :initform      0
    :type          integer
    :documentation "The \"hold\" cell acts in the capacity of an
                    auxiliary storage, similar to a temporary variable.")
   
   ;; Lexer functionality.
   (code
    :initarg       :code
    :initform      NIL
    :type          (or null string)
    :documentation "The Jaune code to parse and interpret.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the Jaune CODE.")
   
   ;; Auxiliary facilities.
   (goto-labels
    :initarg       :goto-labels
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of integer fixnum)
    :documentation "Maps the numeric goto labels, introduced in the
                    code through ``(number):'', to their position in
                    the interpreted Jaune code. An entry with a key of
                    5 and an associated value of 11, for instance,
                    states that a goto label with a name of ``5'' starts
                    at the index 11 of the Jaune CODE string.
                    ---
                    This mapping is maintained as an optimization in
                    lieu of repeated searching for the same labels.")
   (subroutine-labels
    :initarg       :subroutine-labels
    :initform      (make-hash-table :test #'equal)
    :type          (hash-table-of integer fixnum)
    :documentation "Maps the numeric subroutine bodies, introduced in
                    the code through ``(number)@'', to their position in
                    the interpreted Jaune code.
                    ---
                    This mapping is maintained as an optimization in
                    lieu of repeated searching for the same labels.")
   (subroutine-stack
    :initarg       :subroutine-stack
    :initform      NIL
    :type          stack
    :documentation "Pushes upon each invocation of a subroutine,
                    introduced in the code through ``(number)?'' or
                    ``(number)!'', the current position in the Jaune
                    code unto the stack, storing it for a return after
                    the subroutine body has finished.")
   (has-program-ended
    :initarg       :has-program-ended
    :initform      NIL
    :type          boolean
    :documentation "Memorizes whether the main program has ended, which
                    is the case if a dot ('.') has been encountered in
                    the Jaune CODE string."))
  (:documentation
    "The Jaune interpreter maintains all information necessary for
     processing code of this esoteric programming language.
     ---
     Apart from the essentials of the Jaune language specificiation,
     namely the memory, its current cell pointer, and the hold cell,
     additional facilities are maintained:
       - The declaration of goto labels is stored in a hash table, the
         keys of which persist the label names, while each associated
         value memorizes the position in the Jaune CODE string where
         the label declaration has been found. Its discovery actually
         resolves to a linear search for a number followed by a colon
         (':'), which could be repeatedly performed if a request for the
         label occurs, however, this mapping offers an optimization.
     ---
     It is important to stress that, being embedded into a very simple
     implementation, the interpreter partakes of functionality unrelated
     to its scope in a clean architecture, in particular the storage
     of the Jaune code string to interpret and the current position
     therein, which should be the bailiwick of a lexer."))

;;; -------------------------------------------------------

(defun make-interpreter (code)
  "Creates and returns a new ``Interpreter'' used in parsing the Jaune
   CODE."
  (declare (type string code))
  (the Interpreter (make-instance 'Interpreter :code code)))

;;; -------------------------------------------------------

(defun read-number (interpreter start)
  "Reads from the INTERPRETER's code, starting at the START position,
   an signed integer number and returns two values: (1) the parsed
   integer number and (2) the position in the SOURCE of the parsed
   integer number's last digit."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      start))
  (with-slots (code) interpreter
    (let ((is-signed (member (char code start) '(#\+ #\-))))
      (declare (type T is-signed))
      ;; The number either ends immediately before the first non-digit
      ;; character in the SOURCE or at its desinent position.
      (let ((end (or (position-if-not #'digit-char-p code
                       :start (if is-signed (1+ start) start))
                     (length code))))
        (declare (type fixnum end))
        (values (parse-integer code :start start :end end)
                (1- end))))))

;;; -------------------------------------------------------

(defun find-label (interpreter start label-name command)
  "Searches in the INTERPRETER's code, starting at the START position,
   for the label associated with the LABEL-NAME followed by the COMMAND
   character, and returns this character's position in the code."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      start))
  (declare (type integer     label-name))
  (declare (type character   command))
  (with-slots (code) interpreter
    (let ((code-length (length code)))
      (declare (type fixnum code-length))
      (the (or null fixnum)
        (loop
          with  token-index of-type fixnum    = start
          for   character   of-type character = (char code token-index)
          while (< token-index code-length)
          do
          (if (digit-char-p character)
            (multiple-value-bind (number end-position)
                (read-number interpreter token-index) 
              (declare (type integer number))
              (declare (type fixnum  end-position))
              ;; Check whether
              ;;   (a) the LABEL-NAME matches the parsed NUMBER and
              ;;   (b) a character follows the NUMBER and
              ;;   (c) this suffix character matches the COMMAND.
              (if (and (= label-name number)
                       (< end-position (1- code-length))
                       (char= (char code (1+ end-position)) command))
                (return (1+ end-position))
                ;; Either the NUMBER or the command following it does not
                ;; match.
                (setf token-index (1+ end-position))))
            (incf token-index 1)))))))

;;; -------------------------------------------------------

(defun find-subroutine (interpreter start subroutine-number)
  "Searches in the INTERPETER's code, starting at the START position,
   for the subroutine declaration associated with the SUBROUTINE-NUMBER
   and returns the position in the code of the ampersand ('$') symbol
   demarcating the discovered subroutine declaration."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      start))
  (declare (type integer     subroutine-number))
  (the (or null fixnum)
    (find-label interpreter start subroutine-number #\$)))

;;; -------------------------------------------------------

(defun find-goto-declaration (interpreter start goto-number)
  "Searches in the SOURCE, starting at the START position, for the
   goto label declaration associated with the GOTO-NUMBER and
   returns the position in the SOURCE of the colon (':') symbol
   demarcating the discovered goto label declaration."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      start))
  (declare (type integer     goto-number))
  (the (or null fixnum) (find-label interpreter start goto-number #\:)))

;;; -------------------------------------------------------

(defun jump-to-goto-label (interpreter name)
  "Finds the goto label in the INTERPRETER's code, starting at the
   current position and identified by the NAME, and updates the
   INTERPRETER's position to the index of portion following this goto
   label. If the goto label declaration position is not yet stored
   in the GOTO-LABELS hash table, it is entered in the same."
  (declare (type Interpreter interpreter))
  (declare (type integer     name))
  (with-slots (position goto-labels) interpreter
    (multiple-value-bind (label-position contains-label)
        (gethash name goto-labels)
      (declare (type (or null fixnum) label-position))
      (declare (type boolean          contains-label))
      (unless contains-label
        (setf label-position
              (find-goto-declaration interpreter (1+ position) name))
        (setf (gethash name goto-labels) label-position))
      (setf position label-position)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun jump-to-subroutine (interpreter name)
  "Finds the subroutine in the INTERPRETER's code, starting at the
   current POSITION and identified by the NAME, stores the current
   POSITION in the SUBROUTINE stack for later return, and updates the
   POSITION to the index in the CODE where the subroutine body
   commences."
  (declare (type Interpreter interpreter))
  (declare (type integer     name))
  (with-slots (position subroutine-labels subroutine-stack)
      interpreter
    (multiple-value-bind (subroutine-position contains-subroutine)
        (gethash name subroutine-labels)
      (declare (type (or null fixnum) subroutine-position))
      (declare (type boolean          contains-subroutine))
      (unless contains-subroutine
        (setf subroutine-position
              (find-subroutine interpreter (1+ position) name))
        (if subroutine-position
          (setf (gethash name subroutine-labels)
                subroutine-position)
          (error "Cannot find a subroutine with the name ~d." name)))
      (push position subroutine-stack)
      (setf position subroutine-position)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun store-position (interpreter)
  "Memorizes the current position in the INTERPRETER's code for a later
   return, as is necessary if a jump to a subroutine is pending."
  (declare (type Interpreter interpreter))
  (with-slots (position subroutine-stack) interpreter
    (push position subroutine-stack))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun return-to-previous-position (interpreter)
  "Jumps to the position in the INTERPRETER's code immediately occupied
   ere the last subroutine invocation."
  (declare (type Interpreter interpreter))
  (with-slots (position subroutine-stack) interpreter
    (if subroutine-stack
      (setf position (pop subroutine-stack))
      (error "Attempted to return to the position before the last ~
              subroutine call, but found none. Aiblins a superfluous ~
              subroutine termination marker (semicolon) has caused ~
              this anomaly."))))

;;; -------------------------------------------------------

(defun process-numeric-command (interpreter number)
  "Starting at the current position of the INTERPRETER, checks whether
   the NUMBER is succeeded by one of the postfix commands, '+', '-',
   ':', '?', '!', or '@', and if so, processes the command, manipulating
   the INTERPRETER's position in the process."
  (declare (type integer number))
  (with-slots (code position memory pointer goto-labels) interpreter
    (when (< position (1- (length code)))
      (let ((next-character (char code (1+ position))))
        (declare (type character next-character))
        (case next-character
          ;; Add the NUMBER to the current cell.
          (#\+ (incf (aref memory pointer) number)
               (incf position 1))
          ;; Subtract the NUMBER from the current cell.
          (#\- (decf (aref memory pointer) number)
               (incf position 1))
          
          ;; Set goto label.
          (#\: (incf position 1)
               (setf (gethash number goto-labels) position))
          ;; Jump to goto label.
          (#\? (incf position 1)
               (unless (zerop (aref memory pointer))
                 (jump-to-goto-label interpreter number)))
          ;; Jump to goto label.
          (#\! (incf position 1)
               (when (zerop (aref memory pointer))
                 (jump-to-goto-label interpreter number)))
          
          ;; Jump to subroutine.
          (#\@ (incf position 1)
               (jump-to-subroutine interpreter number))
          
          ;; Any other character does not constitute a postfix command.
          ;; => Is ignored.
          (otherwise NIL)))))
  (the Interpreter interpreter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-jaune (jaune-code)
  "Parses and interprets the JAUNE-CODE and returns the ``NIL'' value."
  (declare (type string jaune-code))
  (let ((interpreter (make-instance 'Interpreter :code jaune-code)))
    (declare (type Interpreter interpreter))
    (with-slots (code position
                 memory pointer hold-cell
                 goto-labels subroutine-labels subroutine-stack
                 has-program-ended)
        interpreter
      (loop for char of-type character = (char code position) do
        (cond
          ((null char)
            (loop-finish))
          
          ((char= char #\.)
            (setf has-program-ended T)
            (loop-finish))
          
          ((char= char #\^)
            (format T "~a" (aref memory pointer)))
          
          ((char= char #\v)
            (format T "~&")
            (let ((input (read)))
              (declare (type T input))
              ;; Did the user input an integer number?
              (unless (integerp input)
                (error "Command ``v'' expected an integer number as ~
                        the user input, but received ~s."
                       input))
              (process-numeric-command interpreter input)))
          
          ((char= char #\>)
            (incf pointer 1)
            (when (>= pointer (length memory))
              (setf memory (adjust-array memory (1+ pointer)))))
          
          ((char= char #\<)
            (decf pointer 1))
          
          ((char= char #\#)
            (setf hold-cell (aref memory pointer)))
          
          ((char= char #\&)
            (incf (aref memory pointer) hold-cell))
          
          ((or (digit-char-p char)
               (member char '(#\+ #\-)))
            (multiple-value-bind (number new-position)
                (read-number interpreter position)
              (declare (type integer number))
              (declare (type fixnum  new-position))
              ;; Set the POSITION to the last character of the parsed
              ;; NUMBER.
              (setf position new-position)
              (process-numeric-command interpreter number)))
          
          ((char= char #\;)
            (return-to-previous-position interpreter))
          
          ((char= char #\%)
            (setf (aref memory pointer) 0))
          
          (T
            (error "The command ~s is either invalid or not yet ~
                    implemented."
                   char)))
        
        (incf position 1)
        
        (when has-program-ended
          (loop-finish))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adder in its most simple version.
(parse-jaune "v+v+^.")

;; Adder utilizing the "hold" cell.
(parse-jaune "v+>v+#<&^.")

;; Adder utilizing a subroutine.
(parse-jaune "v+>v+1@^.1$#<&;")

;; Adder utilizing goto labels to implement loops.
;; NOTE: The program does not operate correctly if the second input
;;       constitutes a negative number. The etiology remains yet to be
;;       resolved.
(parse-jaune "v+>v+1:1-<1+>1?<^.")

;; Multiplier.
;; NOTE: The program does not operate correctly if the first input
;;       constitutes a negative number. The etiology remains yet to be
;;       resolved.
(parse-jaune "v+>v+#<1-1?1:>&<1-1?>^.")

;;; -------------------------------------------------------

;; Truth machine.
(parse-jaune "v+1:^1?.")
