;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Spaces and tabs", invented by the Esolang user "Cinnamony"
;; and presented on June 22nd, 2023, the foundation of which answers to
;; a reformulation of Urban Mueller's "brainfuck", the octuple
;; instruction set of which, in lieu of single-character identifiers,
;; assumes trebles of space and horizontal tab symbols, augmented by an
;; aefauld program halting operation.
;; 
;; 
;; Concept
;; =======
;; Founded upon brainfuck's command and memory layout, the former of
;; which naits a very constraiend, yet Turing-complete instruction set,
;; whereas the latter adheres to a bilaterally infinite extent of
;; octet-valued cells, each the salvatory to a single datum in the
;; integral range [0, 255], Spaces and tabs in its majority substitutes
;; the octuple set of single-character tokens by trebles of spaces and
;; horizontal tabs, augmented by a single "end" program terminator.
;; 
;; 
;; Instructions
;; ============
;; In preponderance a modulated agnomination scheme for brainfuck, an
;; advenient "end" command for immediate termination is introduced.
;; 
;; == OVERVIEW ==
;; A foundational acquaintance with the language's instructions shall
;; now be imparted by an apercu.
;; 
;; Please heed that the tokens "space" and "tab" are expected to be
;; substituted by the actual space (" ") and horizontal tab character in
;; the program.
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   space space spac  | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   space tab   space | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   tab   space space | Increments the current cell by one.
;;   ..................................................................
;;   tab   space tab   | Decrements the current cell by one.
;;   ..................................................................
;;   tab   tab   tab   | Queries the user for a character and stores
;;                     | its ASCII code in the current cell.
;;   ..................................................................
;;   tab   tab  space  | Prints the character whose ASCII code
;;                     | corresponds with the current cell value to the
;;                     | standard output.
;;   ..................................................................
;;   space tab  space  | If the current cell value equals zero (0),
;;                     | moves the instruction pointer (IP) forward to
;;                     | the position immediately succeeding the
;;                     | matching "]" command. Otherwise proceeds as
;;                     | usual.
;;   ..................................................................
;;   space tab  tab    | If the current cell value does not equal zero
;;                     | (0), moves the instruction pointer (IP) back
;;                     | to the position immediately succeeding the
;;                     | matching "[" command. Otherwise proceeds as
;;                     | usual.
;;   ..................................................................
;;   end               | Immediately terminates the program, akin to an
;;                     | end-of-file (EOF) marker.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation in Common Lisp proceeds in two stages; imprimis,
;; the Spaces and tabs source code string is transformed into a sequence
;; of commands, the same are subsequently naited to assemble a
;; Common Lisp program in a macro that is ultimately executed.
;; 
;; The second element of this application, maugre its vallidom condeign
;; an askance apercu, shall hopefully be vindicated by its endeictic
;; pursuit.
;; 
;; == GOTO LABEL CONSTRUCTION ==
;; The Common Lisp code production concerning the forward and back jump
;; points involves the definition of unique label names as well as their
;; association in the case of a forward/back limit.
;; 
;; The "normalized", that is, stringently matching standard design of
;; brainfuck jump points is ostended below, with the goto elements'
;; matching numbers displayed as parhedral companions:
;; 
;;   [ [ ] [ ] ]
;;   1 2 2 3 3 1
;; 
;; The increased enumeration problem for the more liberal abstinence
;; from the pairing requirement shall be displayed in the following
;; illustration.
;; 
;;   ] ] [ [ ]
;;   1 2 3 4 4
;; 
;; Two constiuents' participation establish a requisite to the
;; processing infrastructure:
;; 
;;   labelCounter
;;     Used for the production of unique label names, which
;;     concomitantly respects the pairing of at most one forward jump to
;;     one back jump label.
;;     A forward jump command appropriates the current labelCounter
;;     value for its, and its contingent opposite's, identifier, while
;;     consequently increasing the counter state by one for a possible
;;     successor forward jump.
;;   
;;   jumpStack
;;     A stack whose castaldy of the encountered forward jump commands
;;     by adminiculum of their label numbers, that is, their assumed
;;     labelCounter states, permits a matching of goto twains.
;;     The most recently passed forward jump's unique number being held
;;     at the top, a potentially following back jump may acquire the
;;     same and build its corresponding reference back to the peer.
;; 
;; For the forward jump instruction "[", the following pseudocode
;; applies:
;; 
;;   let labelNumber <- labelCounter
;;   labelCounter    <- labelCounter + 1
;;   push labelNumber unto jumpStack
;;   
;;   let labelName <- buildForwardJumpLabel(labelNumber)
;;   append labelName to code
;; 
;; The back jump instruction "]" answers to the pseudocode fragment:
;; 
;;   let labelNumber <- nil
;;   
;;   if jumpStack is not empty then
;;     labelNumber <- pop top element from jumpStack  
;;   else
;;     labelNumber  <- labelCounter
;;     labelCounter <- labelCounter + 1
;;   end if
;;   
;;   let labelName <- buildBackJumpLabelName(labelNumber)
;;   append labelName to code
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-16
;; 
;; Sources:
;;   [esolang2023Spacesandtabs]
;;   The Esolang contributors, "Spaces and tabs", June 22nd, 2023
;;   URL: "https://esolangs.org/wiki/Spaces_and_tabs"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, defaulting to the comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which assumes the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both acquiring the comprehensive
   default ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized Spaces and tabs
   commands, augmented by the adscititious ``:nop'' member which serves
   to denote a no-operation instruction."
  '(member
    :move-right
    :move-left
    :increment
    :decrement
    :output
    :input
    :jump-forward
    :jump-back
    :eof
    :nop))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a Spaces and tabs program as a list of
   zero or more ``command'' objects."
  '(list-of command))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number greater than
   or equal to one (1), but not impounded anenst the upper extremum."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, in this being a commorant of the closed integer range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the program memory's infinite tape of
   unsigned-byte-valued cells as a sparse vector, based upon a hash
   table whose signed integer keys allied with ``octet'' values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, without exhaustion, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command (token)
  "Returns the command associated with the TOKEN, responding with the
   sentinel ``:nop'' (no-operation) for a failed correspondence."
  (declare (type string token))
  (flet ((token-matches-p (characters)
          "Determines whether the TOKEN matches the CHARACTERS sequence,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type (simple-string 3) characters))
          (the boolean
            (not (null
              (and
                (= (length token)
                   (length characters))
                (every #'char= token characters)))))))
    (the command
      (cond
        ((token-matches-p "   ")   :move-right)
        ((token-matches-p " 	 ")  :move-left)
        ((token-matches-p "	  ")   :increment)
        ((token-matches-p "	 	")   :decrement)
        ((token-matches-p "		 ")  :output)
        ((token-matches-p "			") :input)
        ((token-matches-p "  	")   :jump-forward)
        ((token-matches-p " 		") :jump-back)
        ((token-matches-p "end")   :eof)
        (T                         :nop)))))

;;; -------------------------------------------------------

(defun get-token (source start)
  "Proceeding from the START position in the SOURCE, reads the next
   three-character sequence and either returns a string representation
   thereof, or responds with the ``NIL'' value if less than three
   characters succeed this location."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((end (+ start 3)))
    (declare (type fixnum end))
    (the (or null string)
      (when (<= end (length source))
        (subseq source start end)))))

;;; -------------------------------------------------------

(defun probe-token (source start)
  "Proceeding from the START position in the SOURCE, reads the next
   three-character sequence and returns two values:
     (1) The command associated with the sequence, which might be the
         ``:nop'' sentinel in the case of no correspondence of less than
         three characters' participation.
     (2) If three characters coud be gathered and combined into a
         non-``:nop'' command, the position into the SOURCE immediately
         succeeding the consumed portion; otherwise simply the position
         immediately following the START."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((token (get-token source start)))
    (declare (type (or null string) token))
    (the (values command fixnum)
      (if token
        (values (get-command token) (+ start (length token)))
        (values :nop                (+ start 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (code)
  "Extracts and returns from the piece of Spaces and tabs source CODE
   a one-dimensional simple array of its commands."
  (declare (type string code))
  (the program
    (loop
      with position     of-type fixnum  = 0
      and  next-command of-type command = :nop
      
      while (< position (length code))
      
      do
        (setf (values next-command position)
          (probe-token code position))
      
      unless (eq next-command :nop)
        collect next-command)))

;;; -------------------------------------------------------

(defun parse-program (program)
  "Generates and returns for a Spaces and tabs program a list of Common
   Lisp forms capacitated to accompass its operations."
  (declare (type program program))
  
  (let ((lisp-forms    NIL)
        (label-counter 1)
        (jump-stack    NIL))
    (declare (type (list-of T)                lisp-forms))
    (declare (type positive-integer           label-counter))
    (declare (type (list-of positive-integer) jump-stack))
    
    (flet ((insert-form (new-form)
            "Prepends the NEW-FORM to the LISP-FORMS and returns no
             value."
            (declare (type T new-form))
            (push new-form lisp-forms)
            (values))
           
           (build-forward-jump-label (label-number)
            "Produces and returns a forward jump tag name which
             incorporates the LABEL-NUMBER."
            (declare (type positive-integer label-number))
            (the symbol
              (intern
                (format NIL "JUMP-FORWARD-~d" label-number))))
           
           (build-back-jump-label (label-number)
            "Produces and returns a back jump tag name which
             incorporates the LABEL-NUMBER."
            (declare (type positive-integer label-number))
            (the symbol
              (intern
                (format NIL "JUMP-BACK-~d" label-number)))))
      
      (loop for command of-type command in program do
        (case command
          (:move-right
            (insert-form
              `(incf cell-pointer)))
          
          (:move-left
            (insert-form
              `(decf cell-pointer)))
          
          (:increment
            (insert-form
              `(incf current-cell)))
          
          (:decrement
            (insert-form
              `(decf current-cell)))
          
          (:input
            (insert-form
              `(format T "~&>> "))
            (insert-form
              `(setf current-cell
                 (char-code
                   (read-char *standard-input* NIL #\Null))))
            (insert-form
              `(clear-input)))
          
          (:output
            (insert-form
              `(write-char
                 (code-char current-cell))))
          
          (:jump-forward
            (let ((label-number
                    (prog1 label-counter
                      (incf label-counter))))
              (declare (type positive-integer label-number))
              
              (push label-number jump-stack)
              
              (insert-form
                (build-forward-jump-label label-number))
              
              (insert-form
                `(when (zerop current-cell)
                   (go ,(build-back-jump-label label-number))))))
          
          (:jump-back
            (let ((label-number
                    (if jump-stack
                      (pop jump-stack)
                      (prog1 label-counter
                        (incf label-counter)))))
              (declare (type positive-integer label-number))
              
              (insert-form
                (build-back-jump-label label-number))
              
              (insert-form
                `(unless (zerop current-cell)
                   (go ,(build-forward-jump-label label-number))))))
          
          (:eof
            (insert-form
              `(go end)))
          
          (otherwise
            (error "Invalid command: ~s." command)))
        
        finally
          (insert-form 'end)))
    
    (the (list-of T)
      `(tagbody
         ,@(nreverse lisp-forms)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun memory-cell-at (memory index)
  "Returns the byte value stored in the MEMORY cell at the INDEX."
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE, contingently ensuing from a prior wrapping into
   the unsigned byte range of [0, 255], in the MEMORY cell located at
   the INDEX and returns the thus memorized datum."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (setf (gethash index memory 0)
          (mod new-value 256))))

;;; -------------------------------------------------------

(defmacro build-lisp-code (program)
  "Builds and returns the complete Common Lisp code capable of executing
   the Spaces and tabs PROGRAM."
  ``(let ((memory       (make-hash-table :test #'eql))
          (cell-pointer 0))
      (declare (type memory  memory))
      (declare (ignorable    memory))
      (declare (type integer cell-pointer))
      (declare (ignorable    cell-pointer))
      (symbol-macrolet
          ((current-cell
            (the (or octet (eql -1))
              (memory-cell-at memory cell-pointer))))
        (declare (type (or octet (eql -1)) current-cell))
        (declare (ignorable                current-cell))
        ,,program)))

;;; -------------------------------------------------------

(defun interpret-Spaces-and-tabs (code)
  "Interprets the piece of Spaces and tabs source CODE and returns no
   value."
  (declare (type string code))
  (eval
    (build-lisp-code
      (parse-program
        (extract-commands code))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-Spaces-and-tabs converter.    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-from-brainfuck (brainfuck-code
                               &key (destination NIL))
  "Generates a Spaces and tabs program equivalent to the BRAINFUCK-CODE
   and writes the resulting code to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a fresh string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (flet ((write-space ()
              "Prints a space to the DESTINATION and returns no value."
              (format destination " ")
              (values))
             (write-tab ()
              "Prints a horizontal tab to the DESTINATION and returns no
               value."
              (format destination "~c" #\Tab)
              (values))
             (write-end ()
              "Prints the text \"end\" to the DESTINATION and returns no
               value."
              (format destination "end")
              (values)))
        (loop
          for brainfuck-token of-type character across brainfuck-code
          do
            (case brainfuck-token
              (#\>
                (write-space)
                (write-space)
                (write-space))
              (#\<
                (write-space)
                (write-tab)
                (write-space))
              (#\+
                (write-tab)
                (write-space)
                (write-space))
              (#\-
                (write-tab)
                (write-space)
                (write-tab))
              (#\.
                (write-tab)
                (write-tab)
                (write-space))
              (#\,
                (write-tab)
                (write-tab)
                (write-tab))
              (#\[
                (write-space)
                (write-space)
                (write-tab))
              (#\]
                (write-space)
                (write-tab)
                (write-tab))
              (otherwise
                NIL))
          finally
            (write-end)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-from-brainfuck brainfuck-code :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Spaces-and-tabs
  "	 		 	 	 	 	 	  	 	    		    	 	 	     	 		 		 	   	 	   	 	   	 	 	  	  	  		    		 	  	 	 		 			  	 	  	  	  	  	  	  		  	  	 	 			 		  	  	 		  	 	  		       		       		  	  	  	 		 	  	  	  		       		       	 			  	  	  	 	  		 end")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates on a "null"
;; character input.
(interpret-Spaces-and-tabs "	    						  		end")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Spaces-and-tabs
  "         					   	  		 	   	   	  	 	      		   	 	 		 	  	  	   	 	  	  		     			  		end")

;;; -------------------------------------------------------

;; Convert a "Hello, World!" program from brainfuck to Spaces and tabs
;; and execute the same.
(interpret-Spaces-and-tabs
  (convert-from-brainfuck
    "--<-<<+[+[<+>--->->->-<<<]>]<<--.<++++++.<<-..<<.<+.>>.>>.<<<.+++.>>.>>-.<<<+."))
