;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "/gɹəʊguː/", invented by the Esolang user
;; "PythonshellDebugwindow", which operates on two registers of unsigned
;; integer capacity.
;; 
;; Please note that Unicode support imposes upon the processing Common
;; Lisp implementation a requisite, as a ramification of the employment
;; of special characters in the language's name "/gɹəʊguː/". In the case
;; of experiencing lacking support in one's deploying environment, it
;; may avail to replace all statements of "/gɹəʊguː/" by a compatible,
;; usually ASCII-conformant, identifier, such as, for instance,
;; "greugu".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-06-16
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki//g%C9%B9%C9%99%CA%8Agu%CB%90/"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype decimal-digit ()
  "The ``decimal-digit'' type defines a base-10 integer digit in the
   range [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized /gɹəʊguː/ instruction
   types."
  '(member
    :output
    :input
    :jump-back
    :add-digit
    :decrement-nr
    :skip-if-negative
    :print-floor
    :add-nr
    :subtract-nr
    :set-dm-to-nr
    :set-nr-to-dm
    :halt))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type &optional (parameter NIL))))
  "The ``Instruction'' class models an instruction in a /gɹəʊguː/
   program, identified by the command TYPE and an optional decimal digit
   as its PARAMETER."
  (type
    (error "Missing instruction type.")
    :type command)
  (parameter
    NIL
    :type (or null decimal-digit)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of /gɹəʊguː/ CODE a vector of instructions
   and returns the same."
  (declare (type string code))
  
  (let ((instructions NIL))
    (declare (type (list-of Instruction) instructions))
    
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (flet
            ((advance ()
              "Moves the POSITION cursor to the next character in the
               CODE, if possible, updates the current CHARACTER, and
               returns no value."
              (setf character
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (add-instruction (type &optional (parameter NIL))
              "Creates a new ``Instruction'' designated by the TYPE and
               an optional PARAMETER, preprends it to the INSTRUCTIONS
               list, and returns no value."
              (declare (type command                 type))
              (declare (type (or null decimal-digit) parameter))
              (push (make-instruction type parameter) instructions)
              (values)))
          
          (loop while character do
            (case character
              ((NIL)
                (loop-finish))
              
              (#\[
                (add-instruction :output)
                (advance))
              
              (#\;
                (add-instruction :input)
                (advance))
              
              (#\]
                (add-instruction :jump-back)
                (advance))
              
              (#\&
                (advance)
                (cond
                  ((null character)
                    (error "Expected digit to follow \"&\", but ~
                            encountered end of file."))
                  ((digit-char-p character)
                    (add-instruction :add-digit
                      (digit-char-p character))
                    (advance))
                  (T
                    (error "Expected digit to follow \"&\", but ~
                            encountered \"~c\"."
                      character))))
              
              (#\<
                (add-instruction :decrement-nr)
                (advance))
              
              (#\!
                (add-instruction :skip-if-negative)
                (advance))
              
              (#\,
                (add-instruction :print-floor)
                (advance))
              
              (#\+
                (add-instruction :add-nr)
                (advance))
              
              (#\-
                (add-instruction :subtract-nr)
                (advance))
              
              (#\{
                (add-instruction :set-dm-to-nr)
                (advance))
              
              (#\}
                (add-instruction :set-nr-to-dm)
                (advance))
              
              (#\?
                (add-instruction :halt)
                (advance))
              
              (otherwise
                (error "Invalid character \"~c\" at position ~d."
                  character position)))))))
    
    (the (simple-array Instruction (*))
      (coerce (nreverse instructions)
        '(simple-array Instruction (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (instructions)
  "Creates and returns for the INSTRUCTIONS a jump table in the form of
   a mapping, associating with each \"jump-back\" instruction (\"]\")
   position in the INSTRUCTIONS the position immediately following the
   matching \"output\" instruction (\"[\"), or the start index zero (0)
   in the case of a missing symmetry."
  (declare (type (vector Instruction *) instructions))
  (let ((jump-table  (make-hash-table :test #'eql))
        (loop-starts NIL))
    (declare (type (hash-table-of fixnum fixnum) jump-table))
    (declare (type (list-of       fixnum)        loop-starts))
    (loop
      for instruction of-type Instruction across instructions
      and position    of-type fixnum      from   0
      do
        (case (instruction-type instruction)
          (:output
            (push (1+ position) loop-starts))
          (:jump-back
            (setf (gethash position jump-table)
                  (or (pop loop-starts)
                      0)))
          (otherwise
            NIL)))
    (the (hash-table-of fixnum fixnum) jump-table)))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the INSTRUCTIONS and returns no value."
  (declare (type (vector Instruction *) instructions))
  
  (let ((nr     0)              ;; Number Register.
        (dm     0)              ;; Deep Memory.
        (ip     0)              ;; Instruction pointer.
        (labels (build-jump-table instructions)))
    (declare (type integer                       nr))
    (declare (type integer                       dm))
    (declare (type fixnum                        ip))
    (declare (type (hash-table-of fixnum fixnum) labels))
    
    (flet
        ((advance-ip ()
          "Moves the instruction pointer IP to the next position and
           returns no value."
          (incf ip)
          (values))
         
         (move-ip-to (new-position)
          "Relocates the instruction pointer IP to the NEW-POSITION and
           returns no value."
          (declare (type fixnum new-position))
          (setf ip new-position)
          (values))
         
         (read-integer ()
          "Reads from the standard input a signed integer number,
           returning same if the input was valid, otherwise responding
           with the default value of zero (0)."
          (the integer
            (handler-case
              (parse-integer (read-line))
              (error () 0))))
         
         (append-output-instruction ()
          "Creates a new instruction vector by appending to the input
           INSTRUCTIONS a newly generated ``Instruction'' of the
           \"output\" type, updates the INSTRUCTIONS, and returns no
           value."
          (setf instructions
            (concatenate
              '(vector Instruction *)
              instructions
              (vector (make-instruction :output))))
          (values)))
      
      (loop while (< ip (length instructions)) do
        (let ((instruction (aref instructions ip)))
          (declare (type Instruction instruction))
          
          (case (instruction-type instruction)
            (:output
              (format T "[no output]")
              (advance-ip))
            
            (:input
              (format T "~&Please input an integer: ")
              (let ((input (read-integer)))
                (declare (type integer input))
                (clear-input)
                (setf nr input)
                (advance-ip)))
            
            (:jump-back
              (append-output-instruction)
              (move-ip-to (gethash ip labels)))
            
            (:add-digit
              (incf nr (instruction-parameter instruction))
              (advance-ip))
            
            (:decrement-nr
              (decf nr)
              (advance-ip))
            
            (:skip-if-negative
              (cond
                ((minusp nr)
                  (advance-ip)
                  (advance-ip))
                (T
                  (advance-ip))))
            
            (:print-floor
              (format T "~d " (floor nr 2))
              (advance-ip))
            
            (:add-nr
              (incf nr nr)
              (advance-ip))
            
            (:subtract-nr
              (decf nr nr)
              (advance-ip))
            
            (:set-dm-to-nr
              (setf dm nr)
              (advance-ip))
            
            (:set-nr-to-dm
              (setf nr dm)
              (advance-ip))
            
            (:halt
              (advance-ip)
              (loop-finish))
            
            (otherwise
              (error "Unrecognized instruction ~s at instruction ~
                      pointer location ~d."
                instruction ip)))))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-/gɹəʊguː/ (code)
  "Interprets the piece of /gɹəʊguː/ CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-/gɹəʊguː/ ";{-<![}+,}<!]?")

;;; -------------------------------------------------------

;; Integer cat program.
;; This program queries the user until a value of zero (0) or an invalid
;; input is supplied.
(interpret-/gɹəʊguː/ ";{-<![}+,;{<!]?")

;;; -------------------------------------------------------

;; Given a nonnegative integer input N, count down and print the
;; integers from inclusive N down to 0. For a negative input, only the
;; value itself will be printed.
(interpret-/gɹəʊguː/ ";&1{-<![}<{+,-}<!]?")
