;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter as well as bidirectional
;; converters for the esoteric programming language "M?!", invented by
;; the Esolang user "Mipinggfxgbtftybfhfyhfn" and tallied among the
;; derivatives of brainfuck by Urban Mueller.
;; 
;; Concept
;; =======
;; M?! is subsumed under the family of brainfuck derivatives, languages
;; whose syntax and/or programming model adhere within some mete of
;; variability to that of Urban Mueller's brainfuck. M?!'s mark of
;; distinguishment is realized in the object to reduce its ancestor's
;; necessary character set even further from eight to three basic
;; instructions, the costage of which is defrayed by an internally
;; managed \"mode\".
;; 
;; The M?! grammar expresses instructions by a set of three characters:
;; "m", "?", and "!"; any other content is treated with tolerance and
;; construed as a commentary insertion. The language accomplishes a
;; perfect coverage of brainfuck's instruction repertoire, advanced by
;; the interplay of the mode, manipulated by the command "m", and the
;; veridical actuators "?" and "!".
;; 
;; == THE MODE "M" ==
;; Pursuant in its mimicry of brainfuck's octuple commands by a tally in
;; quadruple reduction, the interpretation of either "?" and "!" depends
;; on the currently active mode, an unsigned integer number in the
;; closed interval [0, 3]. Initially set to 0, the mode is increased
;; with every encounter of the letter "m" in the M?! source code, and
;; wrapped to zero if exceeding the limit of 3. The reckoning of a mode
;; "m_new" when augmenting its current magnitude "m_current" thus
;; concurs with the formula
;;   m_new = (m_current + 1) modulo 4
;; 
;; == THE ACTUATORS "?" AND "!"
;; While the mode "m" exerts influence upon the internal state of a
;; program, actions are submitted into effect by the two instructions
;; "?" and "!", acting as "actuators". Any of this pair is capable of
;; producing at an instant one of four effects, each a manifestation of
;; the equinumerant tally of mode states.
;; 
;; == THE INTERPLAY OF MODE AND ACTUATORS ==
;; With the correlations specified, each conceptual instruction in M?!
;; can be regarded as a tuple (mode, actuator), where "mode" is the
;; already disquisited integer-valued mode in the range [0,3], and
;; an actuator resolves to one of the driving instruction tokens "?"
;; and "!". This set, modest in its cardinality, exhibits the capacity
;; of immediate exhaustion:
;;   (0, "?"), (1, "?"), (2, "?"), (3, "?"),
;;   (0, "!"), (1, "!"), (2, "!"), (3, "!").
;; 
;; Having been thus exposed, the following table shall elucidate the
;; multifarious combinations of M?! commands and their association with
;; the brainfuck archetypes:
;; 
;;       M?!        |           |
;;   mode | command | brainfuck | Description
;;   -----+---------+-----------+--------------------------------------
;;    0   | ?       | +         | Increments the cell at the pointer
;;        |         |           | by one.
;;   ..................................................................
;;    1   | ?       | >         | Moves the pointer one cell to the
;;        |         |           | right.
;;   ..................................................................
;;    2   | ?       | [         | Starts an iteration if the current
;;        |         |           | cell's value equals zero.
;;   ..................................................................
;;    3   | ?       | .         | Prints the ASCII character
;;        |         |           | corresponding to the value of the
;;        |         |           | memory cell at the pointer.
;;   -----+---------+-----------+--------------------------------------
;;    0   | !       | -         | Decrements the cell at the pointer.
;;        |         |           | by one.
;;   .....|.........|...........|......................................
;;    1   | !       | <         | Moves the pointer one cell to the
;;        |         |           | left.
;;   .....|.........|...........|......................................
;;    2   | !       | ]         | Stops an iteration if the current
;;        |         |           | cell's value equals zero.
;;   .....|.........|...........|......................................
;;    3   | !       | ,         | Queries the user for a character and
;;        |         |           | stores the ASCII character code in
;;        |         |           | the memory cell at the pointer.
;; 
;; == CONTEXT-SENSITIVITY, THE PRICE OF PARSIMONY ==
;; A corollary ensuing from the mode's latent workings is indicated by
;; the insufficient expressiveness of a character in its isolated
;; inspection: Any of the eight instructions in brainfuck, regardless of
;; their context and surroundings, immediately bewray their semantics;
;; such a token in M?!, on the other hand, is inflicted with ambiguity
;; without intelligence regarding the state of the mode at the point of
;; the instruction's operating.
;; 
;; An example be granted, in this excerpt from a brainfuck code
;;   {some code omitted} + {some code omitted}
;; we immediately known the meaning of the operation "+": the current
;; cell value's augmentation. Supplied this M?! source code fragment
;;   {some code omitted} ! {some code omitted}
;; we cannot with certitude postulate a clear effect of "!", as it may,
;; with respect to the four possible mode states, either decrement the
;; current cell value, move the pointer to the left, control a loop, or
;; query the user for a character.
;; 
;; 
;; Conversions
;; ===========
;; M?!'s equipollence with brainfuck enables the conversion to progress
;; without loss of generality on either end of laterality. The symmetry
;; in transcription does not imply egal simplicity.
;; 
;; == CONVERSION FROM M?! TO BRAINFUCK ==
;; Indeed, the generation of brainfuck code from an M?! source betokens
;; nearly no true hindrances at all. Merely the additional stewardship
;; of the current mode, an integer in the range [0, 3], accounts for
;; a peculiarity, yet being of little burden. Each character from the
;; M?! program must be processed and interpreted in regard to the active
;; mode value. "m" tokens, when consumed, increase or wrap around this
;; state.
;; 
;; == CONVERSION FROM BRAINFUCK TO M?! ==
;; Counterdistinguished from the athwart transcription, the conversion
;; from brainfuck to M?! involves some patent convolution. Returning to
;; the mapping principles elucidated above, we had concluded that each
;; brainfuck command is unambiguously associated with a (mode, actuator)
;; tuple in M?!, where mode, being an integer from {0, 1, 2, 3} and
;; expressed by the token "m", operates in conjunction with one of the
;; actuators "?" and "!". In order to transliterate one brainfuck
;; instruction i_b, we ought to perform two following steps: find the
;; tuple (m, a) from M?! which corresponds to brainfuck's i_b, and
;; write the appropriate commands to the sink, representing the M?!
;; output code, in order to change into that mode m and activate a.
;; 
;; The incipient step, namely the discovery of the correct mode and
;; actuator tuple, is designed in a very eath manner:
;;   (1) Find the mode m, with 0 <= m <= 3, for which one of the
;;       actuators can mimic the effect of i_b.
;;   (2) Find the actuator a, with a in {"?", "!"}, which responds to
;;       the effect of i_b.
;;       => We have obtained the M?! tuple (m, a) corresponding to
;;          brainfuck's i_b.
;; Given the unique bidirectional mapping of command-tuple pairs, the
;; above process can be simplified to a lookup in the respective table
;; exposed above.
;; 
;; The actual manifestation of the mode-actuator composite (m, a)
;; constitutes the chief predicament in the whole translation. Forced to
;; prefix each output M?! instruction with the correct tally of "m" to
;; accord the state with desiderated interpretion of immediately
;; succeeding "?" or "!", we again are required to maintain the mode
;; in the converter. For every single brainfuck command introduced to
;; its translation, we ought to recompute the necessary mode m and
;; actuator a, as described in the first step, then print out the "m"s
;; and the M?! actuator:
;;   (1) Calculate the tally of "m"s to print for the brainfuck
;;       instruction i_b.
;;       => This is the already discovered value m.
;;   (2) Print m times the M?! instruction "m" to the output code.
;;   (3) Print the actuator a to the output code.
;; 
;; The reckoning of the new mode state m_new based upon the current
;; state m_current defines the most demanding effort, and it is advised
;; to simply update m_current by reappropriation of the already known
;; formula
;;   m_new = (m_current + 1) modulo 4
;; to
;;   while (m_current != m) do
;;     m_current = (m_current + 1) modulo 4
;;     print "m" to the output code
;;   end while
;; 
;; 
;; Implementation
;; ==============
;; The implementation presented in the code below relies on Common Lisp
;; and its standard library alone.
;; 
;; == THE TAPE ==
;; This implementation subscribes to the more potent unbounded tape
;; version of brainfuck, hence transferring the wider range to the
;; M?! realization. In corollary, the central data structure, the
;; memory, is realized in the form of an initially empty hash table,
;; the keys of which constitute signed integer values of unbounded
;; magnitude, occupying the bilaterally unrestraint interval
;; [-infinity, +infinity], while administering the same trait to the
;; associated values. Each table entry thus maps
;;   [integer] => [integer]
;; 
;; == THE POINTER ==
;; An integer pointer, equipollent in type and comprehensibility to the
;; hash table keys, is defined, at the start of a program storing the
;; value zero (0) so as to point to the "first" cell.
;; 
;; == THE INTERPLATY OF TAPE AND POINTER ==
;; Extending the possibility to reply upon queries of absent keys with a
;; default value, an entry in the hash table is never created
;; implicitly. If, for instance, the program moves the pointer forward
;; to a non-existing cell, the standard value zero is returned, no entry
;; is actually created in the table. Conceptually, by this handling the
;; data structure can be regarded as an infinite series of initially
;; zero-valued entries or cells. An entry is generated or modified if an
;; instruction would impel a recomputation of the respective cell.
;; 
;; == THE MODE ==
;; The two actuator commands' dependence upon the mode is modeled by the
;; maintenance of a respective state slot. This field's value is
;; amenable to the encounter of "m" tokens in the perused M?! code by
;; augmentation and adjustment in the range [0, 3].
;; 
;; == LOOPS ==
;; The very simple foundation of the M?! language, inherited from
;; brainfuck, permits the eschewing of a parse tree for direction
;; interpretation of the source code. M?!'s infliction, however, with a
;; difficulty not present in the predecessor, imposes the necessity of
;; resourcefulness: brainfuck establishes an iterative construct in the
;; pairing of the left opening bracket '[' and its closing counterpart
;; ']', both interpreted as contrasting conditionals. The two symbols
;; remain well distiguished amidst all character in a brainfuck code.
;; M?! does not vouch for this facility, as the two characters "?" and
;; "!" may control a loop among a triple of other functions each. In
;; consectary, once a loop start has been retrieved, this incipiency,
;; together with its terminal post on the yonder end, must be searched
;; for, discovered, and its position permanently marked in order for
;; contingent iterations in the demarcated region.
;; 
;; The current implementation relays the storage problem to a plain
;; structure, "BFLoop", an abbreviation for "brainfuck loop", as an
;; ancestral allusion. This composite ensconces merely two members: the
;; start position of the loop, that is, the index of the "?" that
;; introduced it, and the end position, referring to the "!" at its
;; terminating point.
;; 
;; Upon the consumption of the "?" command in its agency as an iteration
;; inciter, the current position is memorized in a newly created
;; "BFLoop" object, subsequently searching for a matching "!"
;; instruction and storing its location as the end point of the common
;; "BFLoop", and persisting this loop representation in a collection.
;; If, during the processing of the program, a loop end point is
;; reached, and a repetition of the encompassed instructions is
;; designated necessary, the collection of "BFLoop"s is queried for that
;; instance containing the current character index, whence obtaining the
;; start position, and thus moving the interpreter to the loop head.
;; 
;; In the exact same manner the skipping of an iteration section at the
;; loop head can be accomplished by inquiring into the "BFLoop"
;; coinciding in its start with the current position, then transferring
;; the interpreter's location immediately past the "BFLoop" end point.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/M%3F!"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type describes a sink for print operations,
   including, among others, the functions ``format'' and ``write-char''."
  `(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type describes a list of zero or more elements, the
   elements of which all conform to the ELEMENT-TYPE."
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
  "The ``hash-table-of'' type describes a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and the
   associated value to the VALUE-TYPE."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for
                key
                of-type T
                being   the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of M?! interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (BFLoop
  (:constructor make-bfloop (start end)))
  "Represents the boundaries of a loop in the M?! programming language."
  (start NIL :type (or null fixnum))
  (end   NIL :type (or null fixnum)))

;;; -------------------------------------------------------

(defun get-loop-starting-at (loops position)
  "Returns the ``BFLoop'' among the LOOPS whose start position concurs
   with the POSITION."
  (declare (type (list-of BFLoop) loops))
  (declare (type fixnum           position))
  (the (or null BFLoop)
    (find-if
      #'(lambda (bfloop)
          (declare (type BFLoop bfloop))
          (and (bfloop-start bfloop)
               (= (bfloop-start bfloop) position)))
      loops)))

;;; -------------------------------------------------------

(defun get-loop-ending-at (loops position)
  "Returns the ``BFLoop'' among the LOOPS whose end position concurs
   with the POSITION."
  (declare (type (list-of BFLoop) loops))
  (declare (type fixnum           position))
  (the (or null BFLoop)
    (find-if
      #'(lambda (bfloop)
          (declare (type BFLoop bfloop))
          (and (bfloop-end bfloop)
               (= (bfloop-end bfloop) position)))
      loops)))

;;; -------------------------------------------------------

(defun interpret-M?! (code)
  "Interprets the M?! CODE and returns ``NIL''."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (flet
          ((advance ()
            "Moves to the POSITION to the next location in the CODE and
             updates the CHARACTER."
            (cond
              ((< position (1- (length code)))
                (incf position)
                (setf character (char code position)))
              (T
                (setf character NIL))))
           
           (recede ()
            "Moves the POSITION to the previous location in the CODE
             and updates the current CHARACTER."
            (cond
              ((plusp position)
                (decf position)
                (setf character (char code position)))
              (T
                (setf character NIL)))))
        
        (let ((memory  (make-hash-table :test #'eql))
              (pointer 0)
              (mode    0)
              (loops   NIL))
          (declare (type (hash-table-of integer integer) memory))
          (declare (type integer                         pointer))
          (declare (type (mod 4)                         mode))
          (declare (type (list-of BFLoop)                loops))
          
          (flet ((go-to-next-mode ()
                  "Switches to the next MODE."
                  (if (>= mode 3)
                    (setf mode 0)
                    (incf mode))))
            
            (loop do
              
              (case character
                
                ((NIL)
                  (loop-finish))
                
                ;; Change mode.
                (#\m
                  (go-to-next-mode)
                  (advance))
                
                (#\?
                  (case mode
                    ;; "+"
                    (0
                      (incf (gethash pointer memory 0))
                      (advance))
                    
                    ;; ">"
                    (1
                      (incf pointer)
                      (advance))
                    
                    ;; "["
                    (2
                      (let ((this-loop (get-loop-starting-at loops position)))
                        (declare (type (or null BFLoop) this-loop))
                        
                        (unless this-loop
                          (setf this-loop (make-bfloop position NIL))
                          (advance)
                          (loop with nesting of-type fixnum = 0 do
                            (cond
                              ((null character)
                                (error "Unterminated opening bracket '['."))
                              ;; Opening bracket '['?
                              ((and (char= character #\?)
                                    (= mode 2))
                                (incf nesting)
                                (advance))
                              ;; Closing bracket ']'?
                              ((and (char= character #\!)
                                    (= mode 2))
                                (cond
                                  ((zerop nesting)
                                    (setf (bfloop-end this-loop) position)
                                    (loop-finish))
                                  (T
                                    (decf nesting)
                                    (advance))))
                              ((char= character #\m)
                                (advance)
                                (go-to-next-mode))
                              (T
                                (advance))))
                          (setf position  (bfloop-start this-loop))
                          (setf character (char code position))
                          (push this-loop loops))
                        
                        (cond
                          ;; memory[pointer] = 0?
                          ;; => Jump past ']'.
                          ((zerop (gethash pointer memory 0))
                            ;; No closing bracket for this loop yet?
                            ;; => Find and store it.
                            (setf position  (bfloop-end this-loop))
                            (setf character (char code position))
                            (advance))
                          ;; memory[pointer] != 0?
                          ;; => Simply move on.
                          (T
                            (advance)))))
                    
                    ;; "."
                    (3
                      (write-char (code-char (gethash pointer memory 0)) T)
                      (advance))
                    
                    (otherwise
                      (error "Invalid mode: ~d applied to command '?' ~
                              at position ~d."
                        mode position))))
                
                (#\!
                  (case mode
                    ;; "-"
                    (0
                      (decf (gethash pointer memory 0))
                      (advance))
                    
                    ;; "<"
                    (1
                      (decf pointer)
                      (advance))
                    
                    ;; "]"
                    (2
                      (cond
                        ((not (zerop (gethash pointer memory 0)))
                          (let ((this-loop (get-loop-ending-at loops position)))
                            (declare (type (or null BFLoop) this-loop))
                            (unless this-loop
                              (error "End of loop without start found ~
                                      at position ~d."
                                position))
                            (setf position  (bfloop-start this-loop))
                            (setf character (char code position))))
                        (T
                          (advance))))
                    
                    ;; ","
                    (3
                      (format T "~&Please enter a character: ")
                      (let ((input (read-char)))
                        (declare (type character input))
                        (setf (gethash pointer memory)
                              (char-code input)))
                      (advance))
                    
                    (otherwise
                      (error "Invalid mode ~d applied to command '!' ~
                              at position ~d."
                        mode position))))
                
                (otherwise
                  (advance))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of M?!-to-brainfuck converter.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-M?!-to-brainfuck (M?!-code &optional (destination T))
  "Converts the M?!-CODE into a brainfuck program and writes the
   generated code to the DESTINATION, which defaults to the standard
   output."
  (declare (type string      M?!-code))
  (declare (type destination destination))
  (when (plusp (length M?!-code))
    (if destination
      (let ((position  0)
            (character (char M?!-code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (flet
            ((advance ()
              (cond
                ((< position (1- (length M?!-code)))
                  (incf position)
                  (setf character (char M?!-code position)))
                (T
                  (setf character NIL))))
             (recede ()
              (cond
                ((plusp position)
                  (decf position)
                  (setf character (char M?!-code position)))
                (T
                  (setf character NIL)))))
          
          (let ((mode 0))
            (declare (type (mod 4) mode))
            
            (loop do
              (case character
                (#\m
                  (if (>= mode 3)
                    (setf mode 0)
                    (incf mode)))
                
                (#\?
                  (case mode
                    (0 (write-char #\+ destination))
                    (1 (write-char #\> destination))
                    (2 (write-char #\[ destination))
                    (3 (write-char #\. destination))
                    (otherwise
                      (error "Invalid mode: ~d applied to command ~
                              '?' at position ~d."
                        mode position))))
                
                (#\!
                  (case mode
                    (0 (write-char #\- destination))
                    (1 (write-char #\< destination))
                    (2 (write-char #\] destination))
                    (3 (write-char #\, destination))
                    (otherwise
                      (error "Invalid mode ~d applied to command ~
                              '!' at position ~d."
                        mode position))))
                
                (otherwise
                  (write-char character destination)))
              (advance)))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-M?!-to-brainfuck M?!-code output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck-to-M?! converter.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-brainfuck-to-M?! (brainfuck-code
                                 &optional (destination T))
  "Converts the BRAINFUCK-CODE into an M?! program and writes the
   generated code to the DESTINATION, which defaults to the standard
   output."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (when (plusp (length brainfuck-code))
    (if destination
      (let ((position  0)
            (character (char brainfuck-code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (flet
            ((advance ()
              "Moves the POSITION to the next CHARACTER of the
               BRAINFUCK-CODE and returns no value."
              (cond
                ((< position (1- (length brainfuck-code)))
                  (incf position)
                  (setf character (char brainfuck-code position)))
                (T
                  (setf character NIL)))))
          
          (let ((mode 0))
            (declare (type (mod 4) mode))
            (loop do
              (labels
                  ((ensure-mode (desired-mode)
                    "Ensures that the converter occupies the
                     DESIRED-MODE by writing the number of 'm' commands
                     necessary to switch to this mode, while returning
                     no value."
                    (declare (type (mod 4) desired-mode))
                    (unless (= mode desired-mode)
                      (loop until (= mode desired-mode) do
                        (setf mode (mod (1+ mode) 4))
                        (write-char #\m destination)))
                    (values))
                   
                   (reset-mode ()
                    "Writes the number of 'm' commands necessary to
                     transfer the converter to the mode 0 and returns
                     no value."
                    (ensure-mode 0)
                    (values)))
                
                (case character
                  
                  ((NIL)
                    (loop-finish))
                  
                  (#\>
                    (ensure-mode 1)
                    (write-char #\? destination)
                    (advance))
                  
                  (#\<
                    (ensure-mode 1)
                    (write-char #\! destination)
                    (advance))
                  
                  (#\+
                    (ensure-mode 0)
                    (write-char #\? destination)
                    (advance))
                  
                  (#\-
                    (ensure-mode 0)
                    (write-char #\! destination)
                    (advance))
                  
                  (#\.
                    (ensure-mode 3)
                    (write-char #\? destination)
                    (advance))
                  
                  (#\,
                    (ensure-mode 3)
                    (write-char #\! destination)
                    (advance))
                  
                  (#\[
                    (ensure-mode 2)
                    (write-char #\? destination)
                    
                    (reset-mode)
                    
                    (advance))
                  
                  (#\]
                    (ensure-mode 2)
                    (write-char #\! destination)
                    
                    (reset-mode)
                    
                    (advance))
                  
                  (otherwise
                    (advance))))))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (convert-brainfuck-to-M?! brainfuck-code output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple cat program, querying the user for a character and printing
;; the same to the standard output.
(interpret-M?! "mmm!?")

;;; -------------------------------------------------------

;; A simple loop test which counts from three down to one, printing
;; the respective ASCII characters.
(interpret-M?! "???   | +++  M=0
                mm?   | [    M=2
                m?    | .    M=3
                m!    | -    M=0
                mm!   | ]    M=2")

;;; -------------------------------------------------------

;; Print "Hello World!" to the standard output.
(interpret-M?!
  "????????mm?mmm?mmm????mm?mmm?mmm??m?mmm???m?mmm???m?mmm?m!!!!mmm!mm!mmm?mmm?m?mmm?m?mmm!m??mmm?mm?mmm!m!mmm!mmm!mm!mmm??mm?mm?mmm!!!mmm?m???????mmm??m???mmm?mm??mm?mm!mmm!mmm?mm!mm?m???mmm?m!!!!!!mmm?m!!!!!!!!mmm?mm??mmm?mmm?mm?mmm??mmm?")

;;; -------------------------------------------------------

;; Convert a simple cat program from brainfuck into M?!.
(convert-brainfuck-to-M?! ",.")

;;; -------------------------------------------------------

;; Convert a simple cat program from brainfuck into M?! and executes it.
(interpret-M?! (convert-brainfuck-to-M?! ",." NIL))

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program to M?! and print it
;; to the standard output.
(convert-brainfuck-to-M?! "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

;;; -------------------------------------------------------

;; Convert the brainfuck "Hello World!" program to M?! and interpret
;; it.
(interpret-M?!
  (convert-brainfuck-to-M?!
    "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
    NIL))
