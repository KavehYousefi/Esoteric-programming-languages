;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "⫸fuck", presented by the Esolang user "Username1234" in
;; the year 2022, and intended as a syntactical reformulation of Urban
;; Mueller's "brainfuck" programming language, with the eight
;; instruction tokens substituted by Unicode alternatives of approximate
;; similitude in design.
;; 
;; 
;; Architecture
;; ============
;; ⫸fuck subscribes to the native tenets of its brainfuck ancestor,
;; maintaining a linear sequence of unsigned-byte-valued cells,
;; admitting the integer range of [0, 255], however, not necessitated to
;; accommodate the fixed 30,000 in tally, nor constrained to
;; non-negative indices.
;; 
;; 
;; Instructions
;; ============
;; ⫸fuck's cleronomy apportions to it the exact eight instructions
;; commorant in its inspiration, brainfuck; expressed simply in a more
;; elaborate guise.
;; 
;; == OVERVIEW ==
;; An apercu endowed with compendiousness shall educate about the octet
;; of instructions furnished to the language.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   ⫸       | Moves the cell pointer one step to the right.
;;   ..................................................................
;;   ⫷       | Moves the cell pointer one step to the left.
;;   ..................................................................
;;   ⁜       | Increments the current cell value by one.
;;            | If transcending the upper bound of 255, the value is
;;            | wrapped around to the minimum of zero (0).
;;   ..................................................................
;;   ⌁       | Decrements the current cell value by one.
;;           | If transcending the lower bound of zero (0), the value
;;           | is wrapped around to the maximum of 255.
;;   ..................................................................
;;   ⁕       | Prints to the standard output the character
;;           | corresponding to the current cell value when construed
;;           | an ASCII code.
;;   ..................................................................
;;   ⁑       | Queries the user for an ASCII character and stores its
;;           | ASCII code in the current cell.
;;   ..................................................................
;;   ⟬       | If the current cell value equals zero (0), moves the
;;           | instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "⟭". Otherwise
;;           | proceeds as usual.
;;   ..................................................................
;;   ⟭       | If the current cell value does not equal zero (0), moves
;;           | the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "⟬". Otherwise
;;           | proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == BRAINFUCK-EQUIVALENCY ==
;; The fact of its direct equivalency with brainfuck permits an
;; unambiguous juxtaposition regarding ⫸fuck's and its stock-father's
;; command tokens:
;; 
;;   -------------------------------------
;;   ⫸fuck command | brainfuck equivalent
;;   ---------------+---------------------
;;   ⫸             | >
;;   .....................................
;;   ⫷             | <
;;   .....................................
;;   ⁜             | +
;;   .....................................
;;   ⌁             | -
;;   .....................................
;;   ⁕             | .
;;   .....................................
;;   ⁑             | ,
;;   .....................................
;;   ⟬             | [
;;   .....................................
;;   ⟭             | ]
;;   -------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This simple interpreter has been implemented in Common Lisp, with its
;; operations applying directly to the source code string.
;; 
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-05-14
;; 
;; Sources:
;;   [esolang2022⫸fuck]
;;   The Esolang contributors, "⫸fuck", 2022
;;   URL: "https://esolangs.org/wiki/%E2%AB%B8fuck"
;;   
;;   [esolang2023trivialbfsub]
;;   The Esolang contributors, "Trivial brainfuck substitution", 2023
;;   URL: "https://esolangs.org/wiki/Trivial_brainfuck_substitution"
;;   Notes:
;;     - Describes the family of trivial brainfuck substitutions.
;;   
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   consecutive bits, and thus a commorant of the integer range
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype memory ()
  "The ``memory'' type defines the program memory as a theoretically
   infinite extent of unsigned-byte-valued cells, amenable to an
   arbitrary integer index, and manifesting in a hash table which maps
   integer keys to ``octet'' values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun memory-cell-at (memory index)
  "Returns the value of the cell in the MEMORY amenable to the INDEX,
   defaulting to zero (0) for pristine entries."
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (gethash index memory 0)))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory index)
  "Stores the NEW-VALUE in the memory cell at the INDEX, contingently
   wrapping its value in the unsigned byte range [0, 255] prior to the
   induction, and returns the, potentially adjusted, value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer index))
  (the octet
    (setf (gethash index memory 0)
          (mod new-value 256))))

;;; -------------------------------------------------------

(defun interpret-⫸fuck (code)
  "Interprets the piece of ⫸fuck source CODE and returns no value."
  (declare (type string code))
  
  (let ((memory  (make-hash-table :test #'eql))
        (pointer 0)
        (ip      0))
    (declare (type memory  memory))
    (declare (type integer pointer))
    (declare (type fixnum  ip))
    
    (symbol-macrolet
        ((current-instruction
          (the character
            (char code ip)))
         (eof-p
          (the boolean
            (not (array-in-bounds-p code ip))))
         (current-cell
          (the integer
            (memory-cell-at memory pointer))))
      
      (loop until eof-p do
        (case current-instruction
          (#\⫸
            (incf pointer))
          
          (#\⫷
            (decf pointer))
          
          (#\⁜
            (incf current-cell))
          
          (#\⌁
            (decf current-cell))
          
          (#\⁕
            (write-char
              (code-char current-cell)))
          
          (#\⁑
            (format T "~&>> ")
            (setf current-cell
              (prog1
                (char-code (read-char))
                (clear-input))))
          
          (#\⟬
            (when (zerop current-cell)
              (let ((start-position ip))
                (declare (type fixnum start-position))
                (loop with level of-type integer = 0 do
                  (incf ip)
                  (if eof-p
                    (error "Unmatched forward jump \"⟬\" at ~
                            position ~d."
                      start-position)
                    (case current-instruction
                      (#\⟬
                        (incf level))
                      (#\⟭
                        (if (zerop level)
                          (loop-finish)
                          (decf level)))
                      (otherwise
                        NIL)))))))
          
          (#\⟭
            (unless (zerop current-cell)
              (let ((start-position ip))
                (declare (type fixnum start-position))
                (loop with level of-type integer = 0 do
                  (decf ip)
                  (if eof-p
                    (error "Unmatched back jump \"⟭\" at position ~d."
                      start-position)
                    (case current-instruction
                      (#\⟬
                        (if (zerop level)
                          (loop-finish)
                          (decf level)))
                      (#\⟭
                        (incf level))
                      (otherwise
                        NIL)))))))
          
          (otherwise
            NIL))
        
        (incf ip))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-⫸fuck "⁜⟬⌁⌁⫸⌁⟬⫸⫸⁜⫸⌁⌁⌁⌁⌁⫷⫷⟭⫷⌁⌁⫷⌁⌁⌁⟭⫸⌁⁕⫸⫸⫸⁜⁕⫸⫸⁕⁕⁜⁜⁜⟬⁕⫸⟭⫷⫷⫷⫷⁕⁜⁜⁜⁕⌁⌁⌁⌁⌁⌁⁕⫷⫷⌁⁕⫸⫸⫸⫸⁜⁕")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates upon a null
;; character input.
(interpret-⫸fuck "⁑⟬⁕⁑⟭")

;;; -------------------------------------------------------

;; Truth-machine
(interpret-⫸fuck "⁑⁕⟬⌁⌁⫸⁜⟬⫸⫸⟭⫷⟬⁕⟭⫷⫷⟭")
