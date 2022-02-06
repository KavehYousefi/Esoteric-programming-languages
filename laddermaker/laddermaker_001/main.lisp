;; Date: 2022-02-06
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Laddermaker"



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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' class represents a line of code consumed from a
   laddermaker program and analyzed into its significant constituents."
  (indentation 0   :type fixnum)
  (type        NIL :type (or null keyword))
  (value       NIL :type T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER constitutes a space, returning on
   affirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean (not (null (char= character #\Space)))))

;;; -------------------------------------------------------

(defun empty-string-p (string)
  "Checks whether the STRING is composed of exactly zero characters,
   that is, empty, returning a ``boolean'' value of ``T'' on
   affirmation, otherwise ``NIL''."
  (declare (type string string))
  (the boolean (not (null (zerop (length string))))))

;;; -------------------------------------------------------

(defun blank-string-p (string)
  "Checks whether the STRING is composed of spaces only, returning a
   ``boolean'' value of ``T'' on affirmation, otherwise ``NIL''."
  (declare (type string string))
  (the boolean (not (null (every #'space-character-p string)))))

;;; -------------------------------------------------------

(defun integer-string-p (string)
  "Checks whether the STRING represents an integer number, that is, it
   is composed of at least one decimal digit, potentially surrounded by
   zero or more spaces on its lateralities, and returns a ``boolean''
   value of ``T'' on confirmation, otherwise ``NIL''."
  (declare (type string string))
  (the boolean
    (not (null
      (every
        #'(lambda (character)
            (declare (type character character))
            (or (digit-char-p      character)
                (space-character-p character)))
        string)))))

;;; -------------------------------------------------------

(defun parse-line (line)
  "Parses the LINE and returns a ``Command'' representation of its
   content."
  (declare (type (or null string) line))
  
  (let ((command   (make-command))
        (position  0)
        (character NIL))
    (declare (type Command             command))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character, if possible,
           and returns no value."
          (setf character
            (when (< position (1- (length line)))
              (char line (incf position))))
          (values))
         
         (count-spaces ()
          "Starting at the current POSITION, reads a sequence of zero
           or more spaces, relocates the POSITION cursor to the first
           non-space character, and returns the tally of encountered
           spaces."
          (the fixnum
            (loop
              while (and character (char= character #\Space))
              do    (advance)
              count 1)))
         
         (evaluate-indentation ()
          "Sets the COMMAND's indentation to the number of spaces
           composing the beginning of the LINE, and returns no value."
          (setf (command-indentation command) (count-spaces))
          (values))
         
         (expect-character (expected-character)
          "Checks whether the current CHARACTER matches the
           EXPECTED-CHARACTER, returning no vlaue upon confirmaiton,
           otherwise signaling an error."
          (declare (type character expected-character))
          (unless (and character (char= character expected-character))
            (error "Expected the character ~s at position ~d, but ~
                    encountered ~s."
              expected-character position character))
          (values))
         
         (read-identifier ()
          "Reads the two-character command identifier and returns it as
           a string."
          (expect-character #\+)
          (advance)
          (the string
            (prog1
              (with-output-to-string (identifier)
                (declare (type string-stream identifier))
                (write-char character identifier)
                (advance)
                (write-char character identifier)
                (advance))
              (expect-character #\+)
              (advance))))
         
         (evaluate-identifier ()
          "Sets the COMMAND's type, and contingently its value, in
           concord with its identifier, and returns no value."
          (let ((identifier (read-identifier)))
            (declare (type string identifier))
            (cond
              ((string= identifier "--")
                (setf (command-type command) :create-step))
              
              ((string= identifier "  ")
                (setf (command-type command) :reset-value))
              
              ((string= identifier "++")
                (setf (command-type command) :set-memory))
              
              ((string= identifier "[]")
                (setf (command-type command) :add-to-memory))
              
              ((string= identifier "__")
                (setf (command-type command) :print-character))
              
              ((string= identifier "<>")
                (setf (command-type command) :reset-memory))
              
              ((integer-string-p identifier)
                (setf (command-type  command) :set-value)
                (setf (command-value command) (parse-integer identifier)))
              
              (T
                (setf (command-type  command) :unknown)
                (setf (command-value command) identifier))))
          (values))
         
         (check-dextral-margin ()
          "Checks whether the LINE portion starting at the current
           POSITION is composed of spaces only, returning no value on
           affirmation, otherwise signaling an error."
          (loop while character do
            (case character
              (#\Space
                (advance))
              (otherwise
                (error "The dextral side of the ladder contains the ~
                        invalid character ~s at position ~d."
                  character position))))
          (values)))
    
    (cond
      ((null line)
        (setf (command-type command) :eof))
      ((empty-string-p line)
        (setf (command-type command) :empty))
      ((blank-string-p line)
        (setf (command-type command) :blank))
      (T
        (setf character (char line 0))
        (evaluate-indentation)
        (evaluate-identifier)
        (check-dextral-margin))))
  
  (the Command command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-laddermaker (code)
  "Interprets the piece of laddermaker CODE and returns no value."
  (declare (type string code))
  
  (let ((indentation NIL)
        (value       NIL)
        (memory      0)
        (state       :initialized))
    (declare (type (or null fixnum)        indentation))
    (declare (type (or null (integer 0 *)) value))
    (declare (type (integer 0 *)           memory))
    (declare (type keyword                 state))
    
    (labels
        ((check-indentation (command)
          "Checks whether the COMMAND's indentation equals the global
           INDENTATION requisite to model a straight ladder, returning
           upon confirmation no value, otherwise signaling an error."
          (let ((local-indentation (command-indentation command)))
            (declare (type fixnum local-indentation))
            (unless (= local-indentation indentation)
              (error "The current line's indentation should be ~d, ~
                      but tallies at ~d."
                indentation local-indentation)))
          (values))
         
         (check-if-running ()
          "Checks whether a ladder has been created using the '+--+'
           command, this being a prerequisite for any other operation,
           except the initiating specimen, and returns no value on
           confirmation, otherwise signaling an error."
          (case state
            (:initialized
              (error "The program has not been started yet."))
            (otherwise
              (values))))
         
         (check-if-value-is-reset (&optional
                                    (intention
                                      "Cannot execute the command."))
          "Checks whether the ladder VALUE is currently reset, returning
           no value on confirmation, otherwise signaling an error whose
           message is preceded by the optional INTENTION information."
          (declare (type string intention))
          (when value
            (error "~a The ladder value must be reset." intention))
          (values))
         
         (check-if-value-is-set (&optional
                                  (intention
                                    "Cannot execute the command."))
          "Checks whether the ladder VALUE is set, returning no value on
           confirmation, otherwise signaling an error whose message is
           preceded by the optional INTENTION information."
          (declare (type string intention))
          (unless value
            (error "~a No ladder value is yet defined." intention))
          (values)))
      
      (with-input-from-string (code-stream code)
        (declare (type string-stream code-stream))
        
        (loop
          for line
            of-type (or null string)
            =       (read-line code-stream NIL)
          do
            (let ((command (parse-line line)))
              (declare (type Command command))
              
              (case (command-type command)
                
                (:create-step
                  (case state
                    (:initialized
                      (setf indentation (command-indentation command))
                      (setf state       :started))
                    (otherwise
                      (error "A ladder has already been started."))))
                
                (:reset-value
                  (check-if-running)
                  (check-indentation command)
                  (setf value NIL))
                
                (:set-memory
                  (check-if-running)
                  (check-indentation command)
                  (setf memory value))
                
                (:add-to-memory
                  (check-if-running)
                  (check-indentation command)
                  (check-if-value-is-set
                    "Cannot add the ladder value to the memory.")
                  (incf memory value))
                
                (:print-character
                  (check-if-running)
                  (check-indentation command)
                  (check-if-value-is-reset "Cannot print the memory.")
                  (write-char (code-char memory)))
                
                (:reset-memory
                  (check-if-running)
                  (check-indentation command)
                  (setf memory 0))
                
                (:set-value
                  (check-if-running)
                  (check-indentation command)
                  (check-if-value-is-reset "Cannot set the ladder value.")
                  (setf value (command-value command)))
                
                (:empty
                  (error "Empty line encountered."))
                
                (:blank
                  (error "Blank line encountered."))
                
                (:eof
                  (loop-finish))
                
                (T
                  (error "Invalid command: ~s." command))))))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of laddermaker text generator.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-character-code-portions (character)
  "Returns a list containing a combination of summands in the valid
   laddermaker character code range [1, 99] which, when summed,
   constitute the CHARACTER's ASCII code.
   ---
   The constitution and order of the returned summands do not subscribe
   to any particular regulation, its design, however, by the method of
   its implementation presents monotonically decreasing numbers."
  (declare (type character character))
  (let ((remaining-code (char-code character)))
    (declare (type (unsigned-byte 8) remaining-code))
    (the (list-of (integer 1 99))
      (loop
        for code-portion
          of-type (unsigned-byte 8)
          =       (min remaining-code 99)
        do
          (setf remaining-code
            (max 0 (- remaining-code code-portion)))
        collect code-portion
        while   (plusp remaining-code)))))

;;; -------------------------------------------------------

(defun generate-text-program (text
                              &key (destination T)
                                   (indentation 0))
  "Generates a laddermaker program which prints the TEXT to the standard
   output, indenting the code by the INDENTATION size, and printing the
   it to the DESTINATION."
  (declare (type string                          text))
  (declare (type (or null (eql T) stream string) destination))
  (declare (type (integer 0 *)                   indentation))
  
  (if destination
    (labels
        ((output-instruction (control-string &rest format-arguments)
          "Prints to the DESTINATION the CONTROL-STRING formatted using
           the FORMAT-ARGUMENTS, and preceded by a conditional newline
           and an indentation of INDENTATION size, finally returning
           with no value."
          (declare (type string      control-string))
          (declare (type (list-of T) format-arguments))
          (format destination "~&~v@t~?" indentation
            control-string format-arguments)
          (values))
         
         (output-character (character)
          "Prints to the DESTINATION the laddermaker instructions
           necessary to print the CHARACTER, and returns no value."
          (declare (type character character))
          (loop
            for code-portion
              of-type (integer 1 99)
              in      (get-character-code-portions character)
            for first-portion-p
              of-type boolean
              =       T
              then    NIL
            do
              (output-instruction "+  +")
              (output-instruction "+~2d+" code-portion)
              (if first-portion-p
                (output-instruction "++++")
                (output-instruction "+[]+")))
          (output-instruction "+  +")
          (output-instruction "+__+")
          (values)))
      
      ;; Create a step.
      (output-instruction "+--+")
      
      ;; Create the instructions necessary to print the characters.
      (loop for character of-type character across text do
        (output-character character))
      
      ;; Clear the memory.
      (output-instruction "+<>+"))
  
  (the string
    (with-output-to-string (output)
      (declare (type string-stream output))
      (generate-text-program text :destination output :indentation indentation)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the letter "K".
(interpret-laddermaker
"+--+
+  +
+65+
++++
+  +
+10+
+[]+
+  +
+__+
+<>+"
)

;;; -------------------------------------------------------

;; Generate a laddermaker program to print "Hello, World!" and print the
;; thus produced code to the standard output.
(generate-text-program "Hello, World!")

;;; -------------------------------------------------------

;; Generate a laddermaker program to print "Hello, World!" and return
;; the thus produced code as a string.
(generate-text-program "Hello, World!" :destination NIL)

;;; -------------------------------------------------------

;; Generate a laddermaker program to print "Hello, World!" and execute
;; the thus produced code using the interpreter.
(interpret-laddermaker
  (generate-text-program "Hello, World!" :destination NIL))
