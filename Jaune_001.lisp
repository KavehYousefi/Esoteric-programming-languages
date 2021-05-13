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
;;    literalNumber  := digit, { digit } ;
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
;;   (3) The minus or hyphen character ('-') not being mentioned in the
;;       original description, negative literal numbers are not
;;       possible, whereas a cell, including the special "hold" cell,
;;       might be reduced to that range by the various operations.
;;       In accordance with this restriction, the input obtained by
;;       querying the user must also relate to an unsigned integer
;;       value.
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
;; One of the few 
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-05-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Jaune"
;;       o Documents the Jaune programming language.
;;   -> "https://esolangs.org/wiki/Truth-machine"
;;       o Describes the truth-machine, also listing examples.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of auxiliary functions.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-number (source start)
  "Reads from the SOURCE, starting at the START position, an unsigned
   integer number and returns two values: (1) the parsed integer number
   and (2) the position in the SOURCE of the parsed integer number's
   last digit."
  (declare (type string source))
  (declare (type fixnum start))
  ;; The number either ends immediately before the first non-digit
  ;; character in the SOURCE or at its desinent position.
  (let ((end (or (position-if-not #'digit-char-p source :start start)
                 (length source))))
    (declare (type fixnum end))
    (values (parse-integer source :start start :end end)
            (1- end))))

;;; -------------------------------------------------------

(defun find-subroutine (source start subroutine-number)
  "Searches in the SOURCE, starting at the START position, for the
   subroutine declaration associated with the SUBROUTINE-NUMBER and
   returns the position in the SOURCE of the ampersand ('$') symbol
   demarcating the discovered subroutine declaration."
  (declare (type string        source))
  (declare (type fixnum        start))
  (declare (type (integer 0 *) subroutine-number))
  (the (or null fixnum)
    (loop
      with  token-index of-type fixnum    = start
      for   character   of-type character = (char source token-index)
      while (< token-index (length source))
      do
      (if (digit-char-p character)
        (multiple-value-bind (number end-position)
            (read-number source token-index) 
          (declare (type (integer 0 *) number))
          (declare (type fixnum        end-position))
          (if (and (= subroutine-number number)
                   (char= (char source (1+ end-position)) #\$))
            (return (+ end-position 1))
            (setf token-index (+ end-position 1))))
        (incf token-index 1)))))

;;; -------------------------------------------------------

(defun find-goto-declaration (source start goto-number)
  "Searches in the SOURCE, starting at the START position, for the
   goto label declaration associated with the GOTO-NUMBER and
   returns the position in the SOURCE of the colon (':') symbol
   demarcating the discovered goto label declaration."
  (declare (type string        source))
  (declare (type fixnum        start))
  (declare (type (integer 0 *) goto-number))
  (the (or null fixnum)
    (loop
      with  token-index of-type fixnum    = start
      for   character   of-type character = (char source token-index)
      while (< token-index (length source))
      do
      (if (digit-char-p character)
        (multiple-value-bind (number end-position)
            (read-number source token-index) 
          (declare (type (integer 0 *) number))
          (declare (type fixnum        end-position))
          (if (and (= goto-number number)
                   (char= (char source (1+ end-position)) #\:))
            (return (+ end-position 1))
            (setf token-index (+ end-position 1))))
        (incf token-index 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 1 *) +INITIAL-MEMORY-SIZE+))

;;; -------------------------------------------------------

(defconstant +INITIAL-MEMORY-SIZE+ 30000
  "The initial size of the memory array. Its magnitude is chosen rather
   arbitrarily to reflect the brainfuck language's default. However, any
   integer value greater or equal to one (1) carries validity. Please
   bear in mind that the underlying memory array can enumerate its
   elements merely insode of the range of the ``fixnum'' type, which
   might be far less than the ``integer'' range. Refer to the
   Common Lisp constant variable ``array-total-size-limit'' to obtain
   the actual maximum number of elements that an array may hold.")

;;; -------------------------------------------------------

(defun parse-jaune (code)
  "Parses and interprets the Jaune CODE and returns the ``NIL'' value."
  (declare (type string code))
  ;; The POSITION stores the index of the current character consumed
  ;; from the CODE.
  (let ((position 0))
    (declare (type fixnum position))
    (let ((memory    (make-array +INITIAL-MEMORY-SIZE+
                       :element-type    'integer
                       :initial-element 0))
          (pointer   0)
          (hold-cell 0)
          
          ;; Goto labels for the commands "(number):" and "(number)?".
          (goto-labels       (make-hash-table :test #'equal))
          ;; Subroutine start positions for the command "(number)@".
          (subroutine-labels (make-hash-table :test #'equal))
          ;; Subroutine jump-back positions, whence a subroutine was
          ;; encountered, used to return to the position in the CODE
          ;; where the subroutine was invoked after its completion.
          ;; ---
          ;; The moment a subroutine is called, the control flow must
          ;; first skip to the subroutine's section in the CODE, and
          ;; then return to the position the call was encountered. This
          ;; position where the program left off is pushed unto the
          ;; SUBROUTINE-STACK.
          (subroutine-stack  NIL)
          ;; Has a dot '.' been found, which ends the main program?
          (has-program-ended NIL))
      
      (declare (type (vector integer *) memory))
      (declare (type fixnum             pointer))
      (declare (type integer            hold-cell))
      
      (declare (type hash-table         goto-labels))
      (declare (type hash-table         subroutine-labels))
      (declare (type list               subroutine-stack))
      (declare (type boolean            has-program-ended))
      
      (labels
          ((jump-to-goto-label (name)
            "Finds the goto label in the CODE, starting at the current
             POSITION and identified by the NAME, and updates the
             POSITION to the index of portion following this goto label.
             If the goto label declaration position is not yet stored
             in the GOTO-LABELS hash table, it is entered in the same."
            (declare (type (integer 0 *) name))
            (multiple-value-bind (label-position contains-label)
                (gethash name goto-labels)
              (declare (type (or null fixnum) label-position))
              (declare (type boolean          contains-label))
              (unless contains-label
                (setf label-position
                      (find-goto-declaration code (1+ position) name))
                (setf (gethash name goto-labels) label-position))
              (setf position label-position)))
           
           (jump-to-subroutine (name)
            "Finds the subroutine in the CODE, starting at the current
             POSITION and identified by the NAME, stores the current
             POSITION in the SUBROUTINE stack for later return, and
             updates the POSITION to the index in the CODE where the
             subroutine body commences."
            (declare (type (integer 0 *) name))
            (multiple-value-bind (subroutine-position contains-subroutine)
                (gethash name subroutine-labels)
              (declare (type (or null fixnum) subroutine-position))
              (declare (type boolean          contains-subroutine))
              (unless contains-subroutine
                (setf subroutine-position
                      (find-subroutine code (1+ position) name))
                (if subroutine-position
                  (setf (gethash name subroutine-labels)
                        subroutine-position)
                  (error "Cannot find a subroutine with the name ~d."
                    name)))
              (push position subroutine-stack)
              (setf position subroutine-position)))
           
           (process-numeric-command (number)
            "Starting at the current POSITION, checks whether the
             NUMBER is succeeded by one of the postfix commands, '+',
             '-', ':', '?', '!', or '@', and if so, processes the
             command, manipulating the POSITION in the process."
            (declare (type integer number))
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
                         (jump-to-goto-label number)))
                  ;; Jump to goto label.
                  (#\! (incf position 1)
                       (when (zerop (aref memory pointer))
                         (jump-to-goto-label number)))
                   
                  ;; Jump to subroutine.
                  (#\@ (incf position 1)
                       (jump-to-subroutine number)))))))
        
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
                  (error "Command ``v'' expected a non-negative ~
                          integer number as the user input, but ~
                          received ~s."
                    input))
                ;; Was the user-supplied integer a positive value?
                (unless (>= input 0)
                  (error "Command ``v'' expected a non-negative ~
                          integer number as the user input, but ~
                          received ~s."
                    input))
                (process-numeric-command input)))
            
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
            
            ((digit-char-p char)
              (multiple-value-bind (number new-position)
                  (read-number code position)
                ;; Set the POSITION to the last character of the parsed
                ;; NUMBER.
                (setf position new-position)
                (process-numeric-command number)))
            
            ((char= char #\;)
              (setf position (pop subroutine-stack)))
            
            ((char= char #\%)
              (setf (aref memory pointer) 0))
            
            (T
              (error "The command ~s is either invalid or not yet ~
                      implemented."
                char)))
          
          (incf position 1)
          
          (when has-program-ended
            (loop-finish)))))))



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
(parse-jaune "v+>v+1:1-<1+>1?<^.")

;; Multiplier.
(parse-jaune "v+>v+#<1-1?1:>&<1-1?>^.")

;;; -------------------------------------------------------

;; Truth machine.
(parse-jaune "v+1:^1?.")
