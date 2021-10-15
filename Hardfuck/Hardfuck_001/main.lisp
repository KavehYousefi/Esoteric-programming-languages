;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Hardfuck", invented by the Esolang user "Lebster" and
;; tallied among the derivatives of brainfuck by Urban Mueller.
;; 
;; Concept
;; =======
;; Hardfuck is subsumed under the family of brainfuck derivatives,
;; languages whose syntax and/or programming model adhere within some
;; mete of variability to that of Urban Mueller's brainfuck. Hardfuck
;; retains fidelity to its ancestral syntax, yet distinguishing itself
;; by the declared purpose of inflicting the language with increased
;; difficulty in programming, while at the same time extending its
;; foundation by two new commands.
;; 
;; One moeity of operations in Hardfuck mimics that of its inspiration
;; verbatim; however, the remaining parcel in some ways deserts from
;; pure semblance. Most conspicuously, three instructions are encumbered
;; with variation:
;;   (1) The output operator is associated with the comma "," instead
;;       of brainfuck's dot "."; it also does not print the value of the
;;       current cell as an ASCII character, but that of the cell
;;       immediately PRECEDING it. Consequently, in lieu of the
;;       brainfuck version's pseudocode
;;         print (getCharacterForCode (memory[pointer]))
;;       Hardfuck requires
;;         print (getCharacterForCode (memory[pointer - 1]))
;;   (2) The input is queried using the dot "." rather than the original
;;       comman ",". In addition, the user reply is also printed to the
;;       standard output ere its storage at the current cell.
;;   (3) The loop start instruction "[" moves to the matching right
;;       bracket "]" if the value BEFORE the pointer equals zero.
;;   (4) The loop end instruction "]" returns to the matching left
;;       bracket "[" if the value AFTER the pointer does not equal zero.
;; 
;; Apart from the affectation of existing behaviors, the command set has
;; been augmented with two members foreign to the standard brainfuck:
;;   
;;   Command | Description
;;   --------+---------------------------------------------------------
;;    @      | Multiplies the pointer by four and stores the product
;;           | in the cell preceding the pointer.
;;   ..................................................................
;;    /      | Relocates the pointer to the first cell.
;; 
;; The "@" command permits the reappropriation of the pointer, an
;; integer number commonly construed as either in the traditional range
;; of brainfuck, [0, 29999], or extended to the more liberal gamut
;; [0, +infinity], as an operand for computing the cell value preceding
;; the selection. The instruction can be described in pseudocode by
;;   memory[pointer - 1] <- pointer * 4
;; 
;; The instruction "/" may be regarded as a convenience structure,
;; instantly resetting the pointer to the initial position, that is, the
;; first cell. Expressed in pseudocode:
;;   pointer <- 0
;; 
;; Akin to the original brainfuck, Hardfuck treats any non-instruction
;; character with tolerance, assigning to such content the agency of
;; comments. Please remember that in the case of Hardfuck the two
;; characters ampersand ("@") and slash ("/") are integrated into the
;; reserved set.
;; 
;; The following table summarizes all Hardfuck commands, allocating an
;; apostille where necessary:
;; 
;;   Command | Description
;;   --------+---------------------------------------------------------
;;    >      | Moves the pointer to the right.
;;   ..................................................................
;;    <      | Moves the pointer to the left.
;;   ..................................................................
;;    +      | Increments the memory cell value at the pointer by one.
;;   ..................................................................
;;    -      | Decrements the memory cell value at the pointer by one.
;;   ..................................................................
;;    .      | Queries the user for a character, prints it to the
;;           | standard output, and stores the ASCII character code in
;;           | the memory cell at the pointer.
;;           | * Note: This instruction coalesces the characteristics of
;;           |   the two brainfuck commands "," (input) and
;;           |   "." (output).
;;   ..................................................................
;;    ,      | Prints the ASCII character corresponding to the value of
;;           | the memory cell immediately preceding the pointer.
;;           | * Note: This command can be regarded as a variation on
;;           |   brainfuck's "." (output), targeting, however, the
;;           |   preceding cell.
;;   ..................................................................
;;    [      | Jumps to the matching right bracket "]" if the value of
;;           | the memory cell preceding the pointer equals zero.
;;           | * Note: This command can be regarded as a variation on
;;           |   brainfuck's "[" (jump to "]"}, targeting, however, the
;;           |   preceding cell.
;;   ..................................................................
;;    ]      | Jumps back to the matching left bracket "[" if the value
;;           | of the memory cell succeeding the pointer does not equal
;;           | zero.
;;           | * Note: This command can be regarded as a variation on
;;           |   brainfuck's "]" (jump to "["), targeting, however, the
;;           |   succeeding cell.
;;   ..................................................................
;;    @      | Multiplies the pointer by four and stores the product
;;           | in the cell preceding the pointer.
;;           | * Note: This command does not exit in brainfuck.
;;   ..................................................................
;;    /      | Relocates the pointer to the first cell.
;;           | * Note: This command does not exit in brainfuck.
;; 
;; As an important fact please note that, given the diverging semantics,
;; in the general case a brainfuck program is not guaranteed to operate
;; correctly on a Hardfuck interpreter and vice versa.
;; 
;; 
;; Implementation
;; ==============
;; This implementation subscribes to the more potent unbounded tape
;; version of brainfuck, hence transferring the wider range to the
;; Hardfuck realization. In corollary, the central data structure, the
;; memory, is realized in the form of an initially empty hash table,
;; the keys of which constitute signed integer values of unbounded
;; magnitude, occupying the bilaterally unrestraint interval
;; [-infinity, +infinity], while administering the same trait to the
;; associated values. Each table entry thus maps
;;   [integer] => [integer]
;; 
;; An integer pointer, equipollent in type and comprehensibility to the
;; hash table keys, is defined, at the start of a program storing the
;; value zero (0) so as to point to the "first" cell.
;; 
;; Extending the possibility to reply upon queries of absent keys with a
;; default value, an entry in the hash table is never created
;; implicitly. If, for instance, the program moves the pointer forward
;; to a non-existing cell, the standard value zero is returned, no entry
;; is actually created in the table. Conceptually, by this handling the
;; data structure can be regarded as an infinite series of initially
;; zero-valued entries or cells. An entry is generated or modified if an
;; instruction would impel a recomputation of the respective cell,
;; usually by mediation of the commands "+", "-", or "@".
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-10-13
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Hardfuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-hardfuck (code)
  "Interprets and executes the Hardfuck CODE and returns ``NIL''."
  (declare (type string code))
  (let ((position  0)
        (character NIL))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    
    (flet
        ((advance ()
          "Moves the POSITION to the next CHARACTER Of the CODE and
           returns no value."
          (cond
            ((< position (1- (length code)))
              (incf position)
              (setf character (char code position)))
            (T
              (setf character NIL)))
          (values))
         
         (recede ()
          "Moves the POSITION to the previous CHARACTER of the CODE and
           returns no value."
          (cond
            ((plusp position)
              (decf position)
              (setf character (char code position)))
            (T
              (setf character NIL)))
          (values))
         
         (move-to (new-position)
          "Moves the POSITION to the NEW-POSITION, updates the
           CHARACTER, and returns no value."
          (declare (type fixnum new-position))
          (setf position  new-position)
          (setf character (char code position))
          (values)))
      
      (move-to position)
      
      (let ((memory  (make-hash-table :test #'eql))
            (pointer 0))
        (declare (type hash-table memory))
        (declare (type integer    pointer))
        
        (loop do
          (case character
            ((NIL)
              (loop-finish))
            
            (#\>
              (incf pointer)
              (advance))
            
            (#\<
              (decf pointer)
              (advance))
            
            (#\+
              (incf (gethash pointer memory 0))
              (advance))
            
            (#\-
              (decf (gethash pointer memory 0))
              (advance))
            
            (#\.
              (let ((input (read-char)))
                (declare (type character input))
                (write-char input)
                (setf (gethash pointer memory) (char-code input)))
              (advance))
            
            (#\,
              (write-char (code-char (gethash (1- pointer) memory 0)))
              (advance))
            
            (#\[
              (cond
                ((zerop (gethash (1- pointer) memory 0))
                  (advance)
                  (loop with level of-type fixnum = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated bracket at position ~d."
                          position))
                      (#\[
                        (incf level))
                      (#\]
                        (cond
                          ((zerop level)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (T
                        (advance)))))
                (T
                  (advance))))
            
            (#\]
              (cond
                ((zerop (gethash (1+ pointer) memory 0))
                  (advance))
                (T
                  (recede)
                  (loop with level of-type fixnum = 0 do
                    (case character
                      ((NIL)
                        (error "Unmatched closing bracket at position ~d."
                          position))
                      (#\]
                        (incf level))
                      (#\[
                        (cond
                          ((zerop level)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (T
                        (recede)))))))
            
            (#\@
              (setf (gethash (1- pointer) memory 0)
                    (* pointer 4))
              (advance))
            
            (#\/
              (setf pointer 0)
              (advance))
            
            (otherwise
              (advance))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "e".
(interpret-hardfuck
  "
   sets the cell to 18
   >>>>>>>>>>>>>>>>>>
   multiplies 18 by 4 which is equal to 72 and stores it in cell 17
   @
   goes to cell 50
   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   multiplies 50 by 4 which is equal to 200 and stores it in cell 49
   @
   we go to cell 49
   <
   and then do 99 negatives
   ---------------------------------------------------------------------------------------------------
   now cell 49 has the e and we go to cell 50 before printing cell 49
   >
   ,
  ")

;;; -------------------------------------------------------

;; Print "Hello World".
(interpret-hardfuck
  "
 sets the cell to 18
 >>>>>>>>>>>>>>>>>>
 multiplies 18 by 4 which is equal to 72 and stores it in cell 17
 @
 goes to cell 50
 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 multiplies 50 by 4 which is equal to 200 and stores it in cell 49
 @
 we go to cell 49
 <
 and then do 99 negatives
 ---------------------------------------------------------------------------------------------------
 now cell 49 has the e we can go to cell 51
 >>
 now 50 is 204
 @
 we go back
 <
 and then
 ------------------------------------------------------------------------------------------------
 we have our first l
 >
 I'm being lazy
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 second l
 >
 o
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 now for the space
 >
 ++++++++++++++++++++++++++++++++
 now to do the W
 >
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 and the o
 >
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 and the r
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 l
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 d
 >
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 now to print it out
 />>>>>>>>>>>>>>>>>>,>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>,>,>,>,>,>,>,>,>,>,
 ")

;;; -------------------------------------------------------

;; Minified "Hello World".
(interpret-hardfuck
  ">>>>>>>>>>>>>>>>>>@>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>@<--------------------------------------------------------------------------------------------------->>@<------------------------------------------------------------------------------------------------>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++/>>>>>>>>>>>>>>>>>>,>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>,>,>,>,>,>,>,>,>,>,")

;;; -------------------------------------------------------

;; A simple loop test with three cells, labeled as suc:
;;   a = memory[0]
;;   b = memory[1]
;;   c = memory[2]
(interpret-hardfuck
  "
  a = 1
  +
  
  b = 0
  >
  
  test: a = 0?
  [
    ,
    Go to a
    <
    a = 0
    -
    Go to b
    >
  test: c != 0?
  ]
  
  ,
  ")
