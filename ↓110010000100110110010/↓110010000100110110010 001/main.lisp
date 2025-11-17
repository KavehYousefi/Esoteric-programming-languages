;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "↓110010000100110110010", invented by the Esolang user
;; "Fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
;; and presented on December 8th, 2024, the accompt of its idiasm wones
;; in a program's representation as a stack of bits, embued with the
;; capacitation for its own state's modification by a twissel of
;; operative warklumes.
;; 
;; 
;; Concept
;; =======
;; The provenance of the ↓110010000100110110010 programming language's
;; entheus resides in its programs' representation as a stack of bits,
;; inwith whose bailiwick both the data acquisition and the operative
;; facilities entrepart in a conjoined exercise; begotten from this
;; twifaced syncretism, the language's subsumption incarnates as an
;; additament the self-modifying species.
;; 
;; == THE BIT STACK AS AN OMPHALOS: DATA CONVENES WITH CODE ==
;; The cynosure of a ↓110010000100110110010 program proclaims its
;; entelechia in the bit stack, inwith whose diorism a twyfold agency
;; is accommodated, governed by the owelty in rank of the instruction
;; signifiers' provision and the data items' castaldy.
;; 
;; == THE SOURCE CODE IS MOLDED INTO A BIT STACK ==
;; Empight on the inchoation of the interpretation process, a requisitum
;; is incorporated in the parasceve thilk, from the
;; ↓110010000100110110010 source code string, produces a stack of bits.
;; 
;; During this telos' pursuit, the source string's procession accompts
;; for an advancement in the acquainted sinistrodextral motation,
;; pushing zero (0) and one (1) bits to the conjoined program and data
;; stack, while any other contents' participation enjoys as assessment
;; not aboon an apostille's vallidom.
;; 
;; The natural sequela's obtention from this champarty of the
;; left-to-right string traversal and the bit stack's last-in-first-out
;; concept, the entailed bit values are gathered in their exact obverse
;; arrangement into the stack sink.
;; 
;; The concept serving as the program bits' distillation from the input
;; string shall be the following pseudocode treatise's simulacrum:
;; 
;;   function generateProgramStack (source)
;;     Input:
;;       source:   A string of characters whose bits shall be gathered,
;;                 in their reverse order, in the BIT_STACK.
;;     
;;     Output:
;;       bitStack: A stack composed of the bits retrieved from the
;;                 SOURCE, overthwart to their encountered order, and
;;                 which ultimately represents the program to execute.
;;     
;;     Process:
;;       let bitStack <- create an empty stack of bits
;;       
;;       for each character c in source do
;;         if c = "0" then
;;           push the bit value 0 onto the bitStack
;;         else if c = "1" then
;;           push the bit value 1 onto the bitStack
;;         else
;;           ignore c
;;         end if
;;       end for
;;       
;;       return bitStack
;;   end function
;; 
;; == THE PROGRAM EXECUTION: INSTRUCTIONS DESUMED FROM THE BIT STACK ==
;; A program's execution constitutes the continuous adhibition of
;; indagations and manipulations applied onto the bit stack, in this
;; effort a conjecture with the data obtention, alligated into a state
;; of isogeneity, from the same fount.
;; 
;; Any form of exhaustion, even as a concomitant to an already
;; instigated operation's course, segues into an orderly program
;; cessation, and the parhedral issuance of a certain character to the
;; standard output conduit.
;; 
;; The interpretation process' entirety, its adhibition an investment
;; on already prepared the bit stack, shall be the alow pseudocode
;; treatise's cynosure:
;; 
;;   procedure interpret (stack)
;;     Input:
;;       stack: The ↓110010000100110110010 code distilled into a stack
;;              of bits, intended for its execution.
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       repeat
;;         if stack.isEmpty() then
;;           halt the program
;;           print "H"
;;         
;;         else
;;           let command <- stack.pop()
;;           
;;           if command = 0 then
;;             if stack.isEmpty() then
;;               halt the program
;;               print "0"
;;             else
;;               let topElement     <- stack.pop()
;;               let negatedElement <- negate topElement
;;               
;;               stack.push(negatedElement)
;;             end if
;;           
;;           else if command = 1 then
;;             let numberOfZeros      <- 0
;;             let numberOfDuplicates <- 0
;;             
;;             repeat
;;               if stack.isEmpty() then
;;                 halt the program
;;                 print "1"
;;               else
;;                 let topElement <- stack.pop()
;;                 
;;                 if topElement = 0 then
;;                   numberOfZeros <- numberOfZeros + 1
;;                 else if topElement = 1 then
;;                   stop repetition
;;                 else
;;                   signal error: no bit
;;                 end if
;;               end if
;;             end repeat
;;             
;;             numberOfDuplicates <- (2 * numberOfZeros) + 1
;;             
;;             if numberOfDuplicates > stack.size() then
;;               halt the program
;;               print "1"
;;             else
;;               let duplicates <- list with numberOfDuplicates top
;;                                 elements from stack, commencing from
;;                                 the lower towards the higher stack
;;                                 positions
;;               
;;               for duplicate in duplicates do
;;                 stack.push(duplicate)
;;               end for
;;             end if
;;           
;;           else
;;             signal error: command is no bit
;;           end if
;;         end if
;;         
;;         
;;       end repeat
;;       
;;   end procedure
;; 
;; 
;; Instructions
;; ============
;; Its reliance naitly impelled into the realm of bits,
;; ↓110010000100110110010 wists of two instructions only, scilicet,
;; "0" and "1".
;; 
;; == OVERVIEW ==
;; The following apercu's cynosure shall be edified upon the hypostasis
;; concerning a requisite mete of nortelry's dation towards the
;; operative implements' construe:
;; 
;;   ------------------------------------------------------------------
;;   Command | Description
;;   --------+---------------------------------------------------------
;;   0       | Pops the top stack element, negates thilk, and pushes
;;           | this negated value twice onto the stack.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate an element to pop, the
;;           | program immediately halts, and the bit value zero (0) is
;;           | issued to the standard output conduit.
;;   ..................................................................
;;   1       | Repeatedly pops the top stack elements, until a one (1)
;;           | bit is retrieved, counts the zero (0) bits encountered
;;           | during this process, here nevened "n", and supputates
;;           | the value "m" as
;;           | 
;;           |   m = (2 * n) + 1.
;;           | 
;;           | Subsequently, the m top elements of the stack are
;;           | duplicated.
;;           |---------------------------------------------------------
;;           | This notion of duplication entails the imposition of the
;;           | top elements' copying in their extant arrangement, with
;;           | an ipsissima verba replication on the stack top.
;;           | 
;;           | Limned in a visual owelty of this elucidation, the
;;           | principle ostends this designment:
;;           | 
;;           |                       TOP
;;           |                     |  a  | <---+
;;           |                     |  b  | <---+----- new elements
;;           |     TOP             |  c  | <---+
;;           |   |  a  |           |  a  |
;;           |   |  b  |           |  b  |
;;           |   |  c  |  ======>  |  c  |
;;           |   | ... |           | ... |
;;           |   +-----+           +-----+
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate the requisite bits in
;;           | the course of any task, the program immediately halts,
;;           | and the bit value one (1) is issued to the standard
;;           | output conduit. In a concrete diction, such circumance's
;;           | encheson may concur with one of the following emprises:
;;           | 
;;           |   (a) If, during the removal of bits from stack until a
;;           |       one-bit's (1) retrieval, the stack is rendered
;;           |       exhausted ere this terminating sentinel's
;;           |       obtention.
;;           |   (b) If the top stack elements shall be duplicated,
;;           |       but the stack's conformation does not encompass
;;           |       the requisite tally.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The vehicle to this implementation's reification constitutes the
;; Common Lisp programming language, the mode of its actualization such
;; as to convey to the bit stack's evaluation a prevenience that
;; produces its representation as an adjustable bit vector from the
;; source string.
;; 
;; == THE PROGRAM STACK IS REALIZED AS A DYNAMIC BIT VECTOR ==
;; The binary stack's designment in a one-dimensional bit array's
;; guise, in lieu of the list data type autochthonous to Lisp and
;; entalented with excellence in the capacitation of a last in,
;; first-out salvatory's exploitation, ensues from the fact of the
;; "bit-vector" type's express specialization for purposes of this ilk,
;; invested by their very nature with superiority in performance.
;; 
;; == THE BIT VECTOR'S REAR REPRESENTS THE STACK'S TOP ==
;; A disrespondency occurs in the array type's disrated utilibility
;; anent insertions at the front, thilk would actually correlate to the
;; stack "top" position. In lieu of this bedeen concurrent affiliation,
;; the vector's rear, in Common Lisp governed by the "fill pointer" as
;; the next appendage place's designator, partakes of the stack's
;; boreal laterality's simulacrum.
;; 
;; == PUSH: INSERT AT THE BIT VECTOR REAR ==
;; Insertions at the fill pointer location limn an owelty to a "push"
;; action on the stack top; while the underlying vector automatically
;; advances the pointer to the subsequent, or "higher", spatial
;; signification.
;; 
;; == POP: REDUCE THE VECTOR'S FILL POINTER ==
;; The "pop" operation's reification is elicited by a reduction of the
;; fill pointer to the immediate sinistral vector index.
;; 
;; == PEEK: QUERY THE ELEMENT IMMEDIATELY BEFORE THE FILL POINTER ==
;; The "peek" capability, for a non-empty stack, or, its tantamount, a
;; non-vacant bit vector, does not impose a convolution ayond the
;; location's perquisition immediately before the fill pointer.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen2013lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-10-30
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2024↓110010000100110110010]
;;   The Esolang contributors, "↓110010000100110110010",
;;     December 9th, 2024
;;   URL: "https://esolangs.org/wiki/%E2%86%93110010000100110110010"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun obtain-the-boolean-value-of (object)
  "Translates the OBJECT from its \"generalized boolean\" facette into
   a veridicous Boolean discriminator, returning for a non-``NIL'' input
   a ``boolean'' value of ``T''; otherwise, for a ``NIL'' OBJECT,
   produces ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bit operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun negate-the-bit (bit)
  "Negates, or inverts, the BIT and returns the resulting value."
  (declare (type bit bit))
  (the bit
    (- 1 bit)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bit extraction operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designates-a-binary-digit-p (candidate)
  "Determines whether the CANDIDATE represents a binary digit in
   character form, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (obtain-the-boolean-value-of
      (digit-char-p candidate 2))))

;;; -------------------------------------------------------

(defun parse-as-a-binary-digit (source)
  "Parses the SOURCE character as a binary digit and returns the
   tantamount bit value.
   ---
   If no binary representation of the SOURCE character may be obtained,
   an error of an unspecified type is signaled."
  (declare (type character source))
  (the bit
    (or (digit-char-p source 2)
        (error "The character \"~c\" does not designate a binary digit."
          source))))

;;; -------------------------------------------------------

(defun extract-the-binary-digits (source)
  "Extracts from the SOURCE string the ensconced binary digits and
   returns thilk, in their encountered order, as a simple bit vector."
  (declare (type string source))
  (the simple-bit-vector
    (map 'simple-bit-vector #'parse-as-a-binary-digit
      (remove-if-not #'designates-a-binary-digit-p source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bit stack operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-a-bit-stack (bits)
  "Creates and returns a fresh bit stack comprehending the content of
   BITS vector.
   ---
   A bit stack's representation does not ostend a convolution's grade
   ayond a dynamic bit vector, whose dioristic proprium relates to the
   stack top notion's conflation with the vector's desinent position,
   always located immediately alow the fill pointer. As a consectary of
   this arranged nomothesia, insertions occurring at the stack's top
   translate to additaments' actuations on the vector's rear; deriving
   similiter, peeking and popping behests seek the selfsame locality
   for their accomplishments."
  (declare (type bit-vector bits))
  (the bit-vector
    (make-array
      (length bits)
      :element-type     'bit
      :initial-contents bits
      :adjustable       T
      :fill-pointer     T)))

;;; -------------------------------------------------------

(defun query-the-bit-stack-size (stack)
  "Returns the tally of elements partaking of the bit STACK."
  (declare (type bit-vector stack))
  (the fixnum
    (length stack)))

;;; -------------------------------------------------------

(defun bit-stack-is-empty-p (stack)
  "Determines whether the bit STACK is empty, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type bit-vector stack))
  (the boolean
    (not (null
      (zerop
        (length stack))))))

;;; -------------------------------------------------------

(defun peek-the-next-bit (stack)
  "Returns without its removal the top element on the bit STACK."
  (declare (type bit-vector stack))
  (the bit
    (bit stack
      (1- (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun pop-the-next-bit (stack)
  "Removes and returns the top element from the bit STACK."
  (declare (type bit-vector stack))
  (the bit
    (prog1
      (peek-the-next-bit stack)
      (decf (fill-pointer stack)))))

;;; -------------------------------------------------------

(defun push-the-bit (stack new-bit)
  "Inserts the NEW-BIT on the bit STACK's top position and returns no
   value."
  (declare (type bit-vector stack))
  (vector-push-extend new-bit stack)
  (values))

;;; -------------------------------------------------------

(defun duplicate-the-top-bits (stack tmema-size)
  "Duplicates the TMEMA-SIZE tally of elements from the bit STACK's top
   and returns no value.
   ---
   The duplication process' notion derives from a copying of the
   respective tmema, its lower verge the TMEMA-SIZE's stipulation, to
   the STACK's top; a visualized limning shall be the hyle established
   by this illustration:
   
                         TOP
                       |  a  | <---+
                       |  b  | <---+----- new elements
       TOP             |  c  | <---+
     |  a  |           |  a  |
     |  b  |           |  b  |
     |  c  |  ======>  |  c  |
     | ... |           | ... |
     +-----+           +-----+"
  (declare (type bit-vector    stack))
  (declare (type (integer 0 *) tmema-size))
  (loop
    for bit-to-duplicate
      of-type bit
      across  (subseq stack
                (- (fill-pointer stack) tmema-size)
                (fill-pointer stack))
    do
      (push-the-bit stack bit-to-duplicate))
  (values))

;;; -------------------------------------------------------

(defun next-bit-equals-zero-p (stack)
  "Determines whether the top element on the BIT stack equals zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type bit-vector stack))
  (the boolean
    (not (null
      (zerop
        (peek-the-next-bit stack))))))

;;; -------------------------------------------------------

(defun next-bit-equals-one-p (stack)
  "Determines whether the top element on the BIT stack equals one (1),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type bit-vector stack))
  (the boolean
    (not (null
      (= (peek-the-next-bit stack) 1)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-the-program (code)
  "Parses the piece of ↓110010000100110110010 source CODE and returns
   a bit vector representing the binary stack in its pristine
   constitution."
  (declare (type string code))
  (the bit-vector
    (make-a-bit-stack
      (extract-the-binary-digits code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the condition types.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Halt-Condition (condition)
  ((message
    :initarg       :message
    :initform      #\H
    :reader        halt-condition-message
    :type          standard-char
    :documentation "The character to issue to the standard output
                    conduit as an ultimity of the program's
                    cessation."))
  (:documentation
    "The ``Halt-Condition'' condition type serves in the apprizal about
     the intention to terminate the program, proceeding with the
     obbligato involving a character's issuance to the standard output
     as a valedictory epiphenomenon."))

;;; -------------------------------------------------------

(defun halt-the-program (message)
  "Signals a condition of the type ``Halt-Condition'', thilk
   communicates during its issance the MESSAGE."
  (declare (type standard-char message))
  (signal 'Halt-Condition :message message))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type boolean    +DEFAULT-PRINTS-THE-STACK-P+))
(declaim (type (real 0 *) +DEFAULT-PRINT-DELAY-IN-SECONDS+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-PRINTS-THE-STACK-P+ T
  "The default Boolean flag configuration which determines whether the
   stack shall be printed as an epiphenomenal action in conclusion of
   a command's execution.")

(defparameter +DEFAULT-PRINT-DELAY-IN-SECONDS+ 0.5
  "The default tally of seconds to prerogate the next stack printing
   issuance.")

;;; -------------------------------------------------------

(defun process-a-zero-bit (stack)
  "Accompasses the causata alligated into an affiliation with a zero (0)
   bit instruction, employing for its operations the bit STACK, and
   returns no value.
   ---
   Please heed the absence of the instigating zero (0) bit from the
   STACK's top ere this behest's invocation."
  (declare (type bit-vector stack))
  (if (bit-stack-is-empty-p stack)
    (halt-the-program #\0) 
    (let ((negated-bit (negate-the-bit (pop-the-next-bit stack))))
      (declare (type bit negated-bit))
      (push-the-bit stack negated-bit)
      (push-the-bit stack negated-bit)))
  (values))

;;; -------------------------------------------------------

(defun process-a-one-bit (stack)
  "Accompasses the causata alligated into an affiliation with a one (1)
   bit instruction, employing for its operations the bit STACK, and
   returns no value.
   ---
   Please heed the absence of the instigating one (1) bit from the
   STACK's top ere this behest's invocation."
  (declare (type bit-vector stack))
  (loop with number-of-zeros of-type (integer 0 *) = 0 do
    (cond
      ((bit-stack-is-empty-p stack)
        (halt-the-program #\1))
      ((next-bit-equals-zero-p stack)
        (pop-the-next-bit stack)
        (incf number-of-zeros))
      ((next-bit-equals-one-p stack)
        (pop-the-next-bit stack)
        (let ((number-of-duplications (1+ (* 2 number-of-zeros))))
          (declare (type (integer 1 *) number-of-duplications))
          (cond
            ((>= (query-the-bit-stack-size stack)
                 number-of-duplications)
              (duplicate-the-top-bits stack number-of-duplications)
              (loop-finish))
            (T
              (halt-the-program #\1)))))
      (T
        (error "Unexpected element on the bit stack: ~a."
          (peek-the-next-bit stack)))))
  (values))

;;; -------------------------------------------------------

(defun execute-the-program
    (stack
     &key (prints-the-stack-p     +DEFAULT-PRINTS-THE-STACK-P+)
          (print-delay-in-seconds +DEFAULT-PRINT-DELAY-IN-SECONDS+))
  "Executes the program founded upon the bit STACK's definitions and
   returns no value."
  (declare (type bit-vector stack))
  (declare (type boolean    prints-the-stack-p))
  (declare (type (real 0 *) print-delay-in-seconds))
  (handler-case
    (flet ((print-the-stack-if-desired ()
            "Upon optation, prints the contents of the STACK to the
             standard output conduit, rests for the
             PRINT-DELAY-IN-SECONDS period, and returns no value."
            (when prints-the-stack-p
              (format T "~&[bottom> ~a <top]" stack)
              (sleep  print-delay-in-seconds))
            (values)))
      (print-the-stack-if-desired)
      (loop do
        (cond
          ((bit-stack-is-empty-p stack)
            (halt-the-program #\H))
          ((next-bit-equals-zero-p stack)
            (pop-the-next-bit   stack)
            (process-a-zero-bit stack))
          ((next-bit-equals-one-p stack)
            (pop-the-next-bit  stack)
            (process-a-one-bit stack))
          (T
            (error "The value ~a does not represents a bit."
              (peek-the-next-bit stack))))
        (print-the-stack-if-desired)))
    (Halt-Condition (condition)
      (declare (type Halt-Condition condition))
      (format T "~&~c"
        (halt-condition-message condition))
      (values)))
  (values))

;;; -------------------------------------------------------

(defun interpret-↓110010000100110110010
    (code
     &key (prints-the-stack-p     +DEFAULT-PRINTS-THE-STACK-P+)
          (print-delay-in-seconds +DEFAULT-PRINT-DELAY-IN-SECONDS+))
  "Interprets the piece of ↓110010000100110110010 source CODE and
   returns no value."
  (declare (type string     code))
  (declare (type boolean    prints-the-stack-p))
  (declare (type (real 0 *) print-delay-in-seconds))
  (execute-the-program
    (parse-the-program code)
    :prints-the-stack-p     prints-the-stack-p
    :print-delay-in-seconds print-delay-in-seconds)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evolving and terminating program.
(interpret-↓110010000100110110010 "1010")

;;; -------------------------------------------------------

;; Infinite iterance.
(interpret-↓110010000100110110010 "101101")
