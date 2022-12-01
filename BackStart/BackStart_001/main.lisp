;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BackStart", presented by the Esolang user "ChuckEsoteric08"
;; in the year 2022, and intended as a variant of the Minksy machine
;; curtailed to a single counter or register.
;; 
;; Instructions
;; ============
;; BackStart's instruction set comprises four members, a twain's moeity
;; of which relates, either as a concomitant or in a paravaunt capacity,
;; to the relocation of the instruction pointer. Partly coincident with
;; the group, another two elements apply themselves to the counter's
;; manipulation. A single participant constitutes an exclusive output
;; facility, with no counterpart juxtaposed for an input reception.
;; 
;; == OVERVIEW ==
;; A curtailed ilk of nortelry concerning the language's operative
;; circumference shall be administered by the following tabular
;; illustration.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i {x}   | Increments the counter by the value {x}.
;;   ..................................................................
;;   d {x}   | If the counter equals zero, returns to the start of the
;;           | program.
;;           | If the counter does not equal zero, decrements the
;;           | counter by the value {x}. If the deduction produces a
;;           | negative counter, an error is signaled.
;;   ..................................................................
;;   b       | Returns to the start of the program.
;;   ..................................................................
;;   o {x}   | If the value {x} does not equal zero, outputs it to the
;;           | standard output.
;;           | If {x} equals zero, outputs the counter.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BackStart"
;;   -> "https://esolangs.org/wiki/UBLANG"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (loop
              for element of-type T in (the list object)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized BackStart
   command types."
  '(member
    :increment
    :decrement
    :back
    :output))

;;; -------------------------------------------------------

(deftype backStart-program ()
  "The ``backStart-program'' type defines a parsed BackStart program as
   a vector of zero or more ``Instruction''s."
  '(vector Instruction *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (argument))))
  "The ``Instruction'' class describes an BackStart command, optionally
   associated with an ARGUMENT."
  (type
    (error "Missing instruction type.")
    :type instruction-type)
  (argument
    NIL
    :type (or null integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Scanner".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Scanner
  (:constructor make-scanner (source
                              &aux
                                (position 0)
                                (character
                                  (when (plusp (length source))
                                    (char source 0))))))
  "The ``Scanner'' class provides a scanner, or simple lexical analyzer,
   employed in the extraction of significant objects from a piece of
   BackStart source code."
  (source    (error "Missing source.") :type string)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-scanner ((scanner) &body body)
  "Evaluates the SCANNER, binds its slots to the eponymous local symbol
   macros ``source'', ``position'' and ``character'', evaluates the BODY
   forms, and returns the last evaluated form's results.
   ---
   In addition to the symbol macros, a local function of fixed
   designation is established:
   
     ------------------------------------------------------------------
     Local function | Effect
     ---------------+--------------------------------------------------
     advance ()     | Moves the position cursor to the next character
                    | in the source and returns no value.
     ------------------------------------------------------------------"
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Scanner ,evaluated-scanner))
       (symbol-macrolet
           ((source
             (the string
               (scanner-source ,evaluated-scanner)))
            (position
              (the fixnum
                (scanner-position ,evaluated-scanner)))
            (character
              (the (or null character)
                (scanner-character ,evaluated-scanner))))
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         (flet
             ((advance ()
               "Moves the POSITION cursor to the next character in the
                SOURCE, if possible, updates the current CHARACTER, and
                returns no value."
               (setf character
                 (when (array-in-bounds-p source (1+ position))
                   (char source (incf position))))
               (values)))
           ,@body)))))

;;; -------------------------------------------------------

(defun scanner-skip-spaces (scanner)
  "Starting at the current position into the SCANNER's source, skips a
   sequence of zero or more adjacent whitespaces, and returns the
   modified SCANNER."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop while (and character (whitespace-character-p character)) do
      (advance)))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-expect-spaces (scanner)
  "Starting at the current position into the SCANNER's source, expects
   a sequence of one or more adjacent whitespaces, skips these, and
   returns the modified SCANNER.
   ---
   In the absence of any whitespaces, an error of an unspecified type
   will be signaled."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (if (and character (whitespace-character-p character))
      (scanner-skip-spaces scanner)
      (error "Expected a whitespace at position ~d, but encountered ~s."
        position character)))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun scanner-read-number (scanner)
  "Starting at the current position into the SCANNER's source, reads and
   returns an unsigned integer number.
   ---
   Upon the absence of a decimal digit, an error of an unspecified type
   will be signaled."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (the integer
      (if (and character (digit-char-p character))
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (advance))))
        (error "Expected a number at position ~d, but encountered ~s."
          position character)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (scanner)
  "Extracts and returns a one-dimensional simple array of BackStart
   instructions using the SCANNER."
  (declare (type Scanner scanner))
  (let ((instructions NIL))
    (declare (type (list-of Instruction) instructions))
    (flet
        ((collect-instruction (type &optional (argument))
          "Creates a new instruction of the specified TYPE, optionally
           operating on the ARGUMENT, and preprends it to the
           INSTRUCTIONS list."
          (declare (type instruction-type  type))
          (declare (type (or null integer) argument))
          (push (make-instruction type argument) instructions)
          (values)))
      
      (with-scanner (scanner)
        (loop while character do
          (cond
            ((null character)
              (loop-finish))
            
            ((whitespace-character-p character)
              (scanner-skip-spaces scanner))
            
            ((char= character #\i)
              (advance)
              (scanner-expect-spaces scanner)
              (let ((argument (scanner-read-number scanner)))
                (declare (type integer argument))
                (collect-instruction :increment argument)))
            
            ((char= character #\d)
              (advance)
              (scanner-expect-spaces scanner)
              (let ((argument (scanner-read-number scanner)))
                (declare (type integer argument))
                (collect-instruction :decrement argument)))
            
            ((char= character #\b)
              (advance)
              (collect-instruction :back))
            
            ((char= character #\o)
              (advance)
              (scanner-expect-spaces scanner)
              (let ((argument (scanner-read-number scanner)))
                (declare (type integer argument))
                (collect-instruction :output argument)))
            
            (T
              (error "Invalid character ~s at position ~d."
                character position))))))
    
    (the (simple-array Instruction (*))
      (coerce
        (nreverse instructions)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (instructions
                                  &aux
                                    (ip 0)
                                    (current-instruction
                                      (when (plusp (length instructions))
                                        (aref instructions 0))))))
  "The ``Interpreter'' class constitutes the responsible unit for the
   evaluation of a BackStart program."
  (instructions
    (error "Missing instructions.")
    :type backStart-program)
  (ip                  0   :type fixnum)
  (current-instruction NIL :type (or null Instruction))
  (counter             0   :type integer))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots to the eponymous local
   symbol macros ``instructions'', ``ip'', ``current-instruction'' and
   ``counter'', evaluates the BODY forms, and returns the last processed
   form's results.
   ---
   In addition to the symbol macros, two local functions of fixed
   designation will be established:
   
     ------------------------------------------------------------------
     Local function   | Effect
     -----------------+------------------------------------------------
     advance-ip ()    | Moves the instruction pointer to the next
                      | instruction and returns no value.
     ..................................................................
     jump-to-start () | Moves the instruction pointer to the first
                      | instruction and returns no value.
     ------------------------------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (symbol-macrolet
           ((instructions
             (the backStart-program
               (interpreter-instructions ,evaluated-interpreter)))
            (ip
             (the fixnum
               (interpreter-ip ,evaluated-interpreter)))
            (current-instruction
             (the (or null Instruction)
               (interpreter-current-instruction
                 ,evaluated-interpreter)))
            (counter
             (the integer
               (interpreter-counter ,evaluated-interpreter))))
         (declare (type backStart-program     instructions))
         (declare (type fixnum                ip))
         (declare (type (or null Instruction) current-instruction))
         (declare (type integer               counter))
         (flet
             ((advance-ip ()
               "Moves the instruction pointer IP to the next position in
                the INSTRUCTIONS, if possible, updates the
                CURRENT-INSTRUCTION, and returns no value."
               (setf current-instruction
                 (when (array-in-bounds-p instructions (1+ ip))
                   (aref instructions (incf ip))))
               (values))
              
              (jump-to-start ()
               "Relocates the instruction pointer IP to the start of the
                program, updates the CURRENT-INSTRUCTION, and returns no
                value."
               (setf ip 0)
               (setf current-instruction
                 (when (array-in-bounds-p instructions ip)
                   (aref instructions ip)))
               (values)))
           
           ,@body)))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the BackStart program stored in the INTERPRETER and
   returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (loop while current-instruction do
      (case (instruction-type current-instruction)
        ((NIL)
          (loop-finish))
        
        (:increment
          (incf counter
            (instruction-argument current-instruction))
          (advance-ip))
        
        (:decrement
          (cond
            ((zerop counter)
              (jump-to-start))
            (T
              (decf counter
                (instruction-argument current-instruction))
              
              (if (minusp counter)
                (error "Counter has become negative during the ~
                        instruction ~s, located at position ~d."
                  current-instruction ip)
                (advance-ip)))))
        
        (:back
          (jump-to-start))
        
        (:output
          (format T "~d "
            (if (zerop (instruction-argument current-instruction))
              counter
              (instruction-argument current-instruction)))
          (advance-ip))
        
        (otherwise
          (error "Invalid instruction ~s at position ~d."
            current-instruction ip)))))
  
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpret-BackStart (code)
  "Interprets the piece of BackSTART CODE and returns no value."
  (declare (type string code))
  (interpreter-interpret
    (make-interpreter
      (extract-instructions
        (make-scanner code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the ASCII codes of the text "Hello, world!".
(interpret-BackStart
  "
  o 72
  o 101
  o 108
  o 108
  o 111
  o 44
  o 32
  o 119
  o 111
  o 114
  o 108
  o 100
  o 33
  ")

;;; -------------------------------------------------------

;; Count from one (1) up to infinity.
;; This constitutes a translation of the UBLANG program
;;   +./
(interpret-BackStart
  "i 1
   o 0
   b")
