;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Long2", designed by the Esolang user "Yes", and composed of
;; two instructions only whose nimiety anenst their identifying names
;; avails in the conveyance of its ludibundness.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-08-25
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Long2"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tape ()
  "The ``tape'' type defines a hash table of zero or more entries which
   maps integer keys to integer values, the former representing the cell
   indices, the latter the cell values."
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
                (and (typep key   'integer)
                     (typep value 'integer))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized Long2 instructions."
  '(member :print :brainfuck))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (string 151)        +PRINT-IDENTIFIER+))
(declaim (type (string 158)        +BRAINFUCK-IDENTIFIER+))

(declaim (type (or null string)    *source*))
(declaim (type fixnum              *position*))
(declaim (type (or null character) *token*))
(declaim (type tape                *bf-tape*))
(declaim (type integer             *bf-pointer*))

;;; -------------------------------------------------------

(defparameter +PRINT-IDENTIFIER+
  (format NIL "PRINTTHEVALUEHEADEDAFTERTHISCOMMANDASACHARACTERFROMTHE~
               AMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBYTHEWAY~
               ITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION")
  "The identifier associated with the character print command.")

(defparameter +BRAINFUCK-IDENTIFIER+
  (format NIL "INTERPETTHENEXTLINEASCODEFROMTHEESOTERICPROGRAMMING~
               LANGUAGEBRAINF**KBYEURBANMULERINIFORGOTWHICHYEARWITHA~
               TAPEOFTHIRTYTHOUSANDEIGHTBITCELLSALLINITIALLYSETTOZERO")
  "The identifier associated with the brainfuck interpreter invocation
   command.")

;;; -------------------------------------------------------

(defparameter *source*     NIL
  "The processed Long2 source code.")

(defparameter *position*   0
  "The current position into the *SOURCE* string.")

(defparameter *token*      NIL
  "The character at the current *POSITION* into the *SOURCE* string.")

(defparameter *bf-tape*    (make-hash-table :test #'eql)
  "The brainfuck tape modeled by a hash table which maps integer cell
   indices to integer cell values.")

(defparameter *bf-pointer* 0
  "The cell pointer which contains the index of the currently selected
   cell in the brainfuck tape *BF-TAPE*.")

;;; -------------------------------------------------------

;; Accessor to the currently selected cell of the brainfuck tape.
(define-symbol-macro *bf-current-cell*
  (the integer
    (gethash *bf-pointer* *bf-tape* 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of appurtenance operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun newline-character-p (character)
  "Checks whether the CHARACTER represents a linebreak, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER represents an identifier constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alpha-char-p character)
          (char= character #\*))))))

;;; -------------------------------------------------------

(defun update-token ()
  "Updates the *TOKEN* in dependence upon the *SOURCE* and *POSITION*,
   and returns no value."
  (setf *token*
    (when (and *source* (array-in-bounds-p *source* *position*))
      (char *source* *position*)))
  (values))

;;; -------------------------------------------------------

(defun set-source (new-source)
  "Sets the *SOURCE* to the NEW-SOURCE, resets all appertaining lexer
   fields, and returns no value."
  (declare (type string new-source))
  (setf *source*   new-source)
  (setf *position* 0)
  (update-token)
  (values))

;;; -------------------------------------------------------

(defun advance ()
  "Moves to the next character in the *SOURCE* and returns no value."
  (incf *position*)
  (update-token)
  (values))

;;; -------------------------------------------------------

(defun recede ()
  "Moves to the previous character in the *SOURCE* and returns no
   value."
  (decf *position*)
  (update-token)
  (values))

;;; -------------------------------------------------------

(defun skip-whitespaces ()
  "Skips a sequence of zero or more adjacent space characters and
   returns no value."
  (loop while (and *token* (whitespace-character-p *token*)) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun read-identifier ()
  "Reads an identifier from the *SOURCE* and returns it."
  (the string
    (with-output-to-string (identifier)
      (declare (type string-stream identifier))
      (loop while (and *token* (identifier-character-p *token*)) do
        (write-char *token* identifier)
        (advance)))))

;;; -------------------------------------------------------

(defun get-command-for-identifier (identifier)
  "Returns the command associated with the IDENTIFIER, or signals an
   error of an unspecified type if no affiliation exists."
  (declare (type string identifier))
  (the command
    (cond
      ((string= identifier +PRINT-IDENTIFIER+)
        :print)
      ((string= identifier +BRAINFUCK-IDENTIFIER+)
        :brainfuck)
      (T
        (error "No command associated with the identifier ~s."
          identifier)))))

;;; -------------------------------------------------------

(defun read-command ()
  "Reads a command from the *SOURCE* and returns it."
  (the command
    (get-command-for-identifier
      (read-identifier))))

;;; -------------------------------------------------------

(defun expect-spaces ()
  "Expects a sequence of one or more spaces, on confirmation skipping
   the same and returning no value, otherwise signaling an error of an
   unspecified type."
  (if (and *token* (space-character-p *token*))
    (loop while (and *token* (space-character-p *token*)) do
      (advance))
    (error "Expected spaces, but encountered \"~c\" at position ~d."
      *token* *position*))
  (values))

;;; -------------------------------------------------------

(defun expect-linebreak ()
  "Expects a linebreak, on confirmation skipping the same and returning
   no value, otherwise signaling an error of an unspecified type."
  (if (and *token* (newline-character-p *token*))
    (advance)
    (error "Expected a linebreak, but encountered \"~c\" at ~
            position ~d."
      *token* *position*))
  (values))

;;; -------------------------------------------------------

(defun read-ascii-code ()
  "Reads an ASCII character code from the *SOURCE* and returns it.
   ---
   An error of an unspecified type is signaled if no digit could be
   detected at the current *SOURCE* position."
  (the fixnum
    (if (and *token* (digit-char-p *token*))
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (loop while (and *token* (digit-char-p *token*)) do
            (write-char *token* digits)
            (advance))))
      (error "Expected an ASCII character, ~
              but encountered \"~c\" at position ~d."
        *token* *position*))))

;;; -------------------------------------------------------

(defun process-print-command ()
  "Evaluates a print instruction (\"PRINTTHEVALUEHEADEDAFTERTHISCOMMAND-
   ASACHARACTERFROMTHEAMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBY-
   THEWAYITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION\") and returns no
   value."
  (expect-spaces)
  (let ((ascii-code (read-ascii-code)))
    (declare (type fixnum ascii-code))
    (write-char (code-char ascii-code)))
  (values))

;;; -------------------------------------------------------

(defun reset-brainfuck-engine ()
  "Resets the variables appertaining to the brainfuck interpreter and
   returns no value."
  (clrhash *bf-tape*)
  (setf    *bf-pointer* 0)
  (values))

;;; -------------------------------------------------------

(defun process-brainfuck-command ()
  "Process a brainfuck interpreter invocation instruction (\"INTERPET-
   THENEXTLINEASCODEFROMTHEESOTERICPROGRAMMINGLANGUAGEBRAINF**KBY-
   URBANMULERINIFORGOTWHICHYEARWITHATAPEOFTHIRTYTHOUSANDEIGHTBITCELLS-
   ALLINITIALLYSETTOZERO\") and returns no value."
  (expect-linebreak)
  (loop while (and *token* (not (newline-character-p *token*))) do
    (case *token*
      ((NIL) (loop-finish))
      (#\+   (incf *bf-current-cell*))
      (#\-   (decf *bf-current-cell*))
      (#\>   (incf *bf-pointer*))
      (#\<   (decf *bf-pointer*))
      (#\.   (write-char (code-char *bf-current-cell*)))
      (#\,   (setf *bf-current-cell* (read-char))
             (clear-input))
      (#\[
        (when (zerop *bf-current-cell*)
          (advance)
          (loop with level of-type fixnum = 0 do
            (cond
              ((null *token*)
                (error "Unmatched opening bracket \"[\"."))
              ;; Infringed upon the end of the brainfuck code section.
              ((char= *token* #\Newline)
                (error "Unmatched opening bracket \"[\"."))
              ((char= *token* #\[)
                (incf level)
                (advance))
              ((and (char= *token* #\]) (zerop level))
                (loop-finish))
              ((and (char= *token* #\]) (not (zerop level)))
                (decf level)
                (advance))
              (T
                (advance))))))
      (#\]
        (unless (zerop *bf-current-cell*)
          (recede)
          (loop with level of-type fixnum = 0 do
            (cond
              ((null *token*)
                (error "Unmatched closing bracket \"]\"."))
              ;; Infringed upon the start of the brainfuck code section.
              ((char= *token* #\Newline)
                (error "Unmatched opening bracket \"[\"."))
              ((char= *token* #\])
                (incf level)
                (recede))
              ((and (char= *token* #\[) (zerop level))
                (loop-finish))
              ((and (char= *token* #\[) (not (zerop level)))
                (decf level)
                (recede))
              (T
                (recede))))))
      (otherwise NIL))
    (advance))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Long2 (code)
  "Interprets the piece of Long2 CODE and returns no value."
  (declare (type string code))
  
  (set-source code)
  (reset-brainfuck-engine)
  
  (loop while *token* do
    (cond
      ((null *token*)
        (loop-finish))
      
      ((whitespace-character-p *token*)
        (skip-whitespaces))
      
      ((alpha-char-p *token*)
        (let ((command (read-command)))
          (declare (type command command))
          (case command
            (:print     (process-print-command))
            (:brainfuck (process-brainfuck-command))
            (otherwise  (error "Invalid command: ~s." command)))))
      
      (T
        (error "Unexpected character \"~c\" at position ~d."
          *token* *position*))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "A".
(interpret-Long2 "PRINTTHEVALUEHEADEDAFTERTHISCOMMANDASACHARACTERFROMTHEAMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBYTHEWAYITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION 65")

;;; -------------------------------------------------------

;; Interpret the brainfuck program "Hello World!".
(interpret-Long2
  "INTERPETTHENEXTLINEASCODEFROMTHEESOTERICPROGRAMMINGLANGUAGEBRAINF**KBYEURBANMULERINIFORGOTWHICHYEARWITHATAPEOFTHIRTYTHOUSANDEIGHTBITCELLSALLINITIALLYSETTOZERO
  ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

;;; -------------------------------------------------------

;; Print the letter "A", followed by a linebreak, execute the brainfuck
;; program "Hello World!", and finally print the letter "B".
(interpret-Long2
  "PRINTTHEVALUEHEADEDAFTERTHISCOMMANDASACHARACTERFROMTHEAMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBYTHEWAYITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION 65
   
   PRINTTHEVALUEHEADEDAFTERTHISCOMMANDASACHARACTERFROMTHEAMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBYTHEWAYITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION 10
   
   INTERPETTHENEXTLINEASCODEFROMTHEESOTERICPROGRAMMINGLANGUAGEBRAINF**KBYEURBANMULERINIFORGOTWHICHYEARWITHATAPEOFTHIRTYTHOUSANDEIGHTBITCELLSALLINITIALLYSETTOZERO
   ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
   
   PRINTTHEVALUEHEADEDAFTERTHISCOMMANDASACHARACTERFROMTHEAMERICANSTANDARDCODEFPRINFROMATIONEXCHANGEOHANDBYTHEWAYITSTHEEIGHTBITVERSIONNOTTHESEVENBITVERSION 66")
