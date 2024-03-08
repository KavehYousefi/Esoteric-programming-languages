;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "brainappend", invented by the Esolang user "Joaozin003" and
;; presented on October 9th, 2023, being a derivative from Urban
;; Mueller's "brainfuck', yet whose cleronomy commingles with a bespoke
;; interpretation's application of the control structures "[" and "]"
;; that apportions to this twain in coefficiency the appendage of the
;; enclosed code at the program's desinence for a further execution.
;; 
;; 
;; Concept
;; =======
;; brainappend's substratum registers a derivation from brainfuck, the
;; verbatim appropriation of a sextuple subset's cardinality from the
;; entheus being a compernage to the modified twissel of the jump or
;; loop facilities "[" and "]", which are employed in their supersession
;; as warklumes for the program direct manipulation via the embraced
;; source code section's insertion at the program tail.
;; 
;; == EMBRACED SEGMENTS ARE COPIED TO THE PROGRAM END ==
;; Siccan program segments to whom an amplectation in a "[" and "]"
;; jumelle is imparted are, in conjuncture with the markers themselves,
;; copied in an ipsissima verba fashion to the program's desinence.
;; 
;; == PROGRAMS OPERATE ON AN INFINITE VECTOR OF BYTES ==
;; A pristine apprehension from its provenance, brainappend assumes the
;; bilaterally infinite tape of unsigned byte-valued cells of brainfuck,
;; wrapping around along outside bournes of [0, 255], with the incipial
;; state establishment in the lower extremum of zero (0).
;; 
;; A motile cell pointer, at the program's inchoation empight on the
;; first cell, designates at any instant during the execution the
;; currently active cell, the memory's only entity amenable to
;; perquisitions and modulations. Operations exists for the pointer's
;; gradual traversal across the thus spanned spatial extent.
;; 
;; 
;; Instructions
;; ============
;; Tallying an equinumerant octuple instruction set as its stock-father,
;; brainappend appropriates a preponderance of sextuples from
;; brainfuck, while the control structures molded into the "[" and "]"
;; tokens elude the ipsissima verba replication, nuncupated instead to
;; the duplication of code segments at the program's tail.
;; 
;; == OVERVIEW ==
;; An apercu's dation shall be realized in the adhibition of a cursory
;; grade of gnarity with the brainappend operational features:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one. If the new
;;           | cell state transcends the upper bourne of 255, its value
;;           | wraps around to the lower extremum of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   -       | Decrements the current cell value by one. If the new
;;           | cell state transcends the lower bourne of zero (0), its
;;           | value wraps around to the upper extremum of 255.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   >       | Translates the cell pointer one step to the right.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the left.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code corresponds to the
;;           | current memory cell value to the standard output.
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   ,       | Queries the standard input for a character and stores
;;           | ASCII code in the current memory cell.
;;           |---------------------------------------------------------
;;           | A failure to accommodate an input response is construed
;;           | as the "null character", conflating with the ASCII code
;;           | of zero (0).
;;           |---------------------------------------------------------
;;           | This instruction constitutes a verbatim appropriation
;;           | from brainfuck.
;;   ..................................................................
;;   [       | If the value of the current memory cell equals zero (0),
;;           | moves the instruction pointer (IP) forward to the
;;           | position immediately succeeding the matching "]";
;;           | otherwise proceeds as usual.
;;           |---------------------------------------------------------
;;           | This instruction deviates from the cognominal brainfuck
;;           | operation.
;;   ..................................................................
;;   ]       | If the value of the current memory cell does not equal
;;           | zero (0), appropriates the code section commencing from
;;           | inclusive the preceding matching "[" and inclusive this
;;           | "]", and appends it to the end of the currently extant
;;           | program.
;;           | Please do not forget to include in the copied code the
;;           | brackets, "[" and "]".
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been realized in the programming language Common
;; Lisp, allotting the onus of its programs' execution to an interpreter
;; nuncupated to the direct processing and contingent modification of
;; the brainappend source code in a dynamic string form.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-03-06
;; 
;; Sources:
;;   [esolang2023brainappend]
;;   The Esolang contributors, "brainappend", October 11th, 2023
;;   URL: "https://esolangs.org/wiki/Brainappend"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key among these complies with the KEY-TYPE and
   bonds with a value of the VALUE-TYPE, for both holding the default of
   ``T''."
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

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight (8)
   accolent bits, thus imposing an integral object in the range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines a sparse vector of unsigned bytes,
   extending along both lateralities into infinity, realized in a sparse
   fashion by a hash table whose signed integer keys correspond to the
   cell indices and map to the ``octet''-valued cell values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-string (initial-contents)
  "Creates and returns a dynamic string, comprehending in its inchoation
   the INITIAL-CONTENTS."
  (declare (type string initial-contents))
  (the string
    (make-array (length initial-contents)
      :element-type     'character
      :initial-contents initial-contents
      :adjustable       T
      :fill-pointer     T)))

;;; -------------------------------------------------------

(defun get-string-length (string)
  "Returns the number of effective characters in the STRING, which, for
   a string endowed with a fill pointer, constitutes this pointer's
   position, while, for the case of its lacuna, conflates with the
   STRING's capacity."
  (declare (type string string))
  (the fixnum
    (or (and (array-has-fill-pointer-p string)
             (fill-pointer             string))
        (length string))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-memory ()
  "Creates and returns a fresh ``memory'' instance."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun cell-value-at (memory index)
  "Returns the byte value stored in the MEMORY's INDEX-th cell."
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory index)
  "Stores the NEW-VALUE in the MEMORY's INDEX-th cell, contingently
   preceded by a wrapping of its state into the valid unsigned byte
   range, and returns no value."
  (declare (type memory  memory))
  (declare (type integer index))
  (setf (gethash index memory 0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-left-bracket (code start)
  "Proceeding from the START position into the CODE, and perambulating
   in a sinistral airt, seeks a left bracket (\"[\") occupying the same
   nesting level as the imputed right bracket (\"]\") on the START
   location, upon success returnig the discovery's position in the CODE,
   otherwise signals an error of an unspecified type."
  (declare (type string code))
  (declare (type fixnum start))
  (the fixnum
    (loop
      with nesting-level
        of-type fixnum
        =       0
      for position
        of-type fixnum
        from    start
        downto  0
      do
        (case (char code position)
          (#\[
            (if (zerop nesting-level)
              (return position)
              (decf   nesting-level)))
          (#\]
            (incf nesting-level))
          (otherwise
            NIL))
      finally
        (error "No matching left bracket (\"[\") found."))))

;;; -------------------------------------------------------

(defun locate-right-bracket (code start)
  "Proceeding from the START position into the CODE, and perambulating
   in a dextral airt, seeks a right bracket (\"]\") occupying the same
   nesting level as the imputed left bracket (\"[\") on the START
   location, upon success returnig the discovery's position in the CODE,
   otherwise signals an error of an unspecified type."
  (declare (type string code))
  (declare (type fixnum start))
  (the fixnum
    (loop
      with nesting-level
        of-type fixnum
        =       0
      for position
        of-type fixnum
        from    start
        below   (get-string-length code)
      do
        (case (char code position)
          (#\[
            (incf nesting-level))
          (#\]
            (if (zerop nesting-level)
              (return position)
              (decf   nesting-level)))
          (otherwise
            NIL))
      finally
        (error "No matching right bracket (\"]\") found."))))

;;; -------------------------------------------------------

(defun interpret-brainappend (initial-code)
  "Interprets the piece of brainappend INITIAL-CODE and returns no
   value."
  (declare (type string initial-code))
  
  (let ((code         (make-dynamic-string initial-code))
        (ip           0)
        (memory       (make-memory))
        (cell-pointer 0))
    (declare (type string  code))
    (declare (type fixnum  ip))
    (declare (type memory  memory))
    (declare (type integer cell-pointer))
    
    (loop while (< ip (fill-pointer code)) do
      (case (char code ip)
        (#\+
          (incf (cell-value-at memory cell-pointer)))
        
        (#\-
          (decf (cell-value-at memory cell-pointer)))
        
        (#\>
          (incf cell-pointer))
        
        (#\<
          (decf cell-pointer))
        
        (#\,
          (format T "~&>> ")
          (finish-output)
          (let ((input (read-char NIL NIL #\Null)))
            (declare (type character input))
            (setf (cell-value-at memory cell-pointer)
              (or (and (char= input #\Newline) 0)
                  (char-code input))))
          (clear-input))
        
        (#\.
          (write-char
            (code-char
              (cell-value-at memory cell-pointer))))
        
        (#\[
          (when (zerop (cell-value-at memory cell-pointer))
            (setf ip
              (locate-right-bracket code
                (1+ ip)))))
        
        (#\]
          (unless (zerop (cell-value-at memory cell-pointer))
            (let ((start-point (locate-left-bracket code (1- ip))))
              (declare (type fixnum start-point))
              (format code "~a"
                (subseq code start-point (1+ ip))))))
        
        (otherwise
          NIL))
      
      (incf ip)))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a user input of the
;; "null character", in this interpreter, for endeictic purposes, being
;; tantamount to a newline entity's entry.
(interpret-brainappend ",[.,]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-brainappend
  ",.
   >+++++++++++++++++++++++++++++++++++++++++++++++++
   <------------------------------------------------
   [>.<]")
