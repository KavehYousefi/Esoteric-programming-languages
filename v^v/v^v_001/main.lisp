;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter and text output code generator
;; for the esoteric programming language "v^v", invented by the Esolang
;; user "Username1234", and based upon the manipulation of a stack by
;; operations represented through the tally of consecutive "v"
;; characters, with the paravaunt purpose of output message productions.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/V%5Ev"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack ()
  "The ``stack'' type defines a list-based stack of zero or more integer
   values."
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
                  (integerp element))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   including, without the claim of exhaustion, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-v^v (code)
  "Interprets the piece of v^v CODE and returns no value."
  (declare (type string code))
  
  (let ((position  0)
        (character (char code 0))
        (command   0)
        (stack     NIL))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (declare (type (integer 0 *)       command))
    (declare (type stack               stack))
    
    (labels
        ((advance ()
          "Moves the POSITION cursor to the next character in the CODE,
           if possible, updates the current CHARACTER, and returns no
           value."
          (setf character
            (when (array-in-bounds-p code (1+ position))
              (char code (incf position))))
          (values))
         
         (left-parenthesis-follows-p ()
          "Checks whether the next character constitutes a left
           parenthesis (')'), returning on confirmation a ``boolean''
           value of ``T'', otherwise ``NIL''."
          (the boolean
            (not (null
              (and (array-in-bounds-p code (1+ position))
                   (char= (char code (1+ position)) #\)))))))
          
         (comment-follows-p ()
          "Checks whether a comment section follows, starting with the
           current character being a left parenthesis (')'), and returns
           on confirmation a ``boolean'' value of ``T'', otherwise
           ``NIL''."
          (the boolean
            (the boolean
              (not (null
                (and character
                     (char= character #\))
                     (left-parenthesis-follows-p)))))))
         
         (build-command ()
          "Tallies the number of consecutive 'v' characters, store this
           value in the COMMAND, and returns no value."
          (setf command
            (loop
              while (and character (char= character #\v))
              do    (advance)
              count 1))
          (values))
         
         (evaluate-command ()
          "Evaluates the COMMAND and returns no value."
          (case command
            (1
              ;; Push the value of the COMMAND to the stack, which
              ;; always equals one (1).
              (push command stack))
            
            ;; Pop the top stack element.
            (2
              (pop stack))
            
            ;; Print the ASCII character corresponding to the top stack
            ;; element.
            (3
              (write-char (code-char (first stack))))
            
            ;; Increment the top stack element by one.
            (4
              (incf (first stack)))
            
            ;; Decrement the top stack element by one.
            (5
              (decf (first stack)))
            
            ;; Set the top stack element x to
            ;;   x = (x^x) mod 128
            (6
              (setf (first stack)
                    (mod (expt (first stack) (first stack)) 128)))
            
            ;; Terminate the program.
            (7
              (setf position  (length code))
              (setf character NIL))
            
            ;; Other COMMAND values are not tolerated.
            (otherwise
              (error "Invalid command: ~d." command)))
          (values)))
      
      (loop
        while character
        do
          (cond
            ;; End of program?
            ;; => Terminate.
            ((null character)
              (loop-finish))
            
            ;; "v" found?
            ;; => Count "v" characters and store in COMMAND.
            ((char= character #\v)
              (build-command))
            
            ;; "^" found?
            ;; => Execute the operation connoted with the COMMAND.
            ((char= character #\^)
              (evaluate-command)
              (setf command 0)
              (advance))
            
            ;; "))" follows?
            ;; => Rest of the code is a comment.
            ;;    => Terminate the program immediately.
            ((comment-follows-p)
              (evaluate-command)
              (return NIL))
            
            ;; Other characters are prohibited.
            (T
              (error "Invalid character ~s at position ~d."
                character position)))
        finally
          (evaluate-command))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of v^v text generator.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-text-program (text &key (destination NIL))
  "Creates a v^v program which outputs the TEXT and writes the code to
   the DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responding with a fresh string which contains the
   program."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (cond
      (destination
        (loop for character of-type character across text do
          ;; Push the number one (1) unto the stack.
          (format destination "v^")
          ;; Modify the top stack value until it matches the CHARACTER's
          ;; ASCII code.
          (let ((character-code (char-code character)))
            (declare (type fixnum character-code))
            (if (plusp character-code)
              ;; The CHARACTER-CODE is greater than or equal to one?
              ;; => Increment the one-valued top stack element until it
              ;;    matches the CHARACTER's ASCII code.
              (loop repeat (1- character-code) do
                (format destination "vvvv^"))
              ;; The CHARACTER-CODE equals zero or is negative?
              ;; => Decrement the one-valued top stack element until it
              ;;    matches the CHARACTER's ASCII code.
              (loop repeat (abs (+ character-code -1)) do
                (format destination "vvvvv^"))))
          ;; Print the top of the stack as an ASCII character.
          (format destination "vvv^")
          ;; Pop the just printed top of the stack.
          (format destination "vv^"))
        ;; Terminate the v^v program.
        (format destination "vvvvvvv"))
      (T
        (with-output-to-string (output)
          (declare (type string-stream output))
          (generate-text-program text :destination output))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "CCF".
(interpret-v^v "v^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvv^vv^v^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvv^vv^v^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvvv^vvv^vv^vvvvvvv")

;;; -------------------------------------------------------

;; Generate the v^v program for printing the text "Hello, World" and
;; print it to the standard output.
(generate-text-program "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Generate the v^v program for printing the text "Hello, World" and
;; interpret it.
(interpret-v^v
  (generate-text-program "Hello, World!"))
