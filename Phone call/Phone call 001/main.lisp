;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Phone call", presented by the Esolang user "A" in the year
;; 2019, and based upon a syntax reminiscent of a telephone's keypad,
;; embracing the decimal digits 0 through 9 and the special symbols "*"
;; and "#" in order to manipulate a stack of integers and an
;; integer-valued scalar accumulator, as well as navigate or iterate
;; through the program.
;; 
;; 
;; Concept
;; =======
;; The Phone call esoteric programming language limits its character set
;; to the twelve symbols commorant on a telephone keypad, employed in
;; the manipulation of both an integer stack and an integer accumulator,
;; eschewing, however, any input and output conduits.
;; 
;; == PROGRAMS SIMULATE TELEPHONE INPUTS ==
;; The syntactical entheus woning in Phone call relays to the mimicry of
;; a common telephone keypad, the same's elements amplect a duodecimal
;; account of nine decimal digits and two symbols, "*" and "#",
;; ultimately amounting to the approximate design
;; 
;;   [1] [2] [3]
;;   [4] [5] [6]
;;   [7] [8] [9]
;;   [*] [0] [#]
;; 
;; == PROGRAMS OPERATE ON A STACK AND AN ACCUMULATOR ==
;; The data castaldy is consigned to the efforts of two storage units,
;; one being a stack of unbounded integers, the other a single
;; accumulator of the same integer type.
;; 
;; The stack's bailiwick governs the majority of operations, including
;; arithmetics and, of course, basic stack manipulations.
;; 
;; The accumulator exclusively relates to the control flow, permitting,
;; in dependence upon its value, either jumping (goto) or iteration.
;; 
;; A very slim interface permits the unidirectional copying of the
;; stack's top element into the accumulator in order to conduce their
;; coeffiency, thus realizing the conditional capacities.
;; 
;; 
;; Architecture
;; ============
;; Phone call programs may access two distinct storage units,
;; comprehending an unbounded stack of arbitrary integers and a scalar
;; integer accumulator, both of which may operate in coefficiency.
;; 
;; == STACK OF INTEGERS ==
;; The most potent storage, the last-in-first-out (LIFO) salvatory
;; maintains a theoretically infinite tally of unbounded integers. This
;; data compartment applies itself to a preponderance among the
;; language's capacitations, embracing the bittock of available
;; arithmetics and the interaction with the accumulator.
;; 
;; == INTEGER ACCUMULATOR ==
;; A scalar integer accumulator, also entrusted with an unbounded
;; integer's castaldy, yet invested with less competence, exercises its
;; influence in paravaunt mentioning with the governance of the goto or
;; iteration facility.
;; 
;; 
;; Data Types
;; ==========
;; The sole data type acquainted to Phone call constitutes integer of
;; any magnitude and sign, that is, commorants in the range
;; [-infinity, +infinity], maintained in the stack and accumulator.
;; 
;; 
;; Syntax
;; ======
;; Phone call's syntaxis relies on a very compendious ilk of characters,
;; exhausted already by the ten decimal digits 0, 1, 2, 3, 4, 5, 6, 7,
;; 8, and 9, accompanied by the special symbols asterisk ("*") and hash
;; sign ("#").
;; 
;; == INSTRUCTIONS ==
;; Each of the twelve instructions is represented by a single character,
;; its derivation from the telephone keypads' entheus, and capable of
;; its categorization into the following aspects:
;; 
;;   ------------------------------------------------------------------
;;   Subject         | Token
;;   ----------------+-------------------------------------------------
;;   Decimal digits  | 0
;;                   | 1
;;                   | 2
;;                   | 3
;;                   | 4
;;                   | 5
;;                   | 6
;;                   | 7
;;                   | 8
;;                   | 9
;;   ..................................................................
;;   Special symbols | *
;;                   | #
;;   ------------------------------------------------------------------
;; 
;; == WHITESPACES ==
;; Whitespaces, which amplect the space, horizontal tab and newline,
;; may be inserted liberally into the code.
;; 
;; == COMMENTS ==
;; No provisions for comments are incorporated into the current language
;; iteration.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) description applies to
;; Phone call's donat:
;; 
;;   program     := { whitespaces | command } ;
;;   command     := "0" | "1" | "2" | "3" | "4" | "5"
;;               |  "6" | "7" | "8" | "9" | "*" | "#"
;;               ;
;;   whitespaces := { whitespace } ;
;;   whitespace  := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; Phone call's instruction set tallies a duodecimal account of members,
;; a vast prepondance among the same invested in the stack's
;; manipulation, with a less numerous compernage appertaining to the
;; accumulator and navigational facilities.
;; 
;; Please note that a program's instruction sequence, when relating to
;; the instruction pointer's helming, enumerates its commands with a
;; one-based subscript, that is to say, the first instruction is
;; accessed with the index 1 (one).
;; 
;; == OVERVIEW ==
;; The twelve instructions shall now be adhibited a cursory exposition
;; in a tabular format:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   0       | Pushes the value 0 unto the stack.
;;   ..................................................................
;;   1       | Pushes the value -1 unto the stack.
;;   ..................................................................
;;   2       | Pushes the value 2 unto the stack.
;;   ..................................................................
;;   3       | Pushes the value 3 unto the stack.
;;   ..................................................................
;;   4       | Pushes the value 5 unto the stack.
;;   ..................................................................
;;   5       | Pushes the value 7 unto the stack.
;;   ..................................................................
;;   6       | Pushes the value 11 unto the stack.
;;   ..................................................................
;;   7       | Pushes the value 13 unto the stack.
;;   ..................................................................
;;   8       | Pops the top element from the stack.
;;   ..................................................................
;;   9       | Copies without removing the top stack element into the
;;           | accumulator.
;;   ..................................................................
;;   *       | Pops the two top stack elements and pushes their factor
;;           | unto the stack.
;;   ..................................................................
;;   #       | Perquires the accumulator value, issuing one of two
;;           | possible actions:
;;           |   (a) If the accumulator value is even, relocates the
;;           |       instruction pointer to the instruction at the
;;           |       one-based position p in the program which
;;           |       satisfies the equation
;;           |         p = (accumulatorValue / 2) + 1
;;           |   (b) If the value is odd, repeats the portion
;;           |       immediately following the "#" symbol until the end
;;           |       of the program until the top stack element equals
;;           |       zero (0).
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The language's compendious presentation offers the same as a victim
;; to some ambiguities, a subset of which shall manifest in the coming
;; treatise.
;; 
;; == HOW DO PROGRAMS LOOP? ==
;; The instruction token "#" affiliates in a twifaced mode with either
;; a conditional goto facility or an iterative response.
;; Counterdistinguished from the former's lucid specification, the
;; latter merely relates of a looping until the stack's top element
;; resolves to zero (0) --- destitute of an index into which program
;; portion shall serve as the iterative segment's marches.
;; 
;; It has been chosen to attend to the imputation that the iteration
;; extends from the position immediately succeeding the instigating "#"
;; symbol until the end of the program; upon necessity returning to the
;; loop start location.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation is deployed by adminiculum of the
;; programming language Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-04-16
;; 
;; Sources:
;;   [esolang2019PhoneCall]
;;   The Esolang contributors, "Phone call", 2019
;;   -> "https://esolangs.org/wiki/Phone_call"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype command ()
  "The ``command'' type enumerates the recognized Phone call instruction
   variants."
  '(member
    :push-zero
    :push-minus-one
    :push-two
    :push-three
    :push-five
    :push-seven
    :push-eleven
    :push-thirteen
    :pop-stack
    :copy-from-stack-to-accumulator
    :multiply-stack
    :jump-or-loop))

;;; -------------------------------------------------------

(deftype phone-call-program ()
  "The ``phone-call-program'' type defines an executable Phone call
   instruction sequence as either a vector of ``command'' objects or a
   one-dimensional simple-array of the same element type."
  '(or (vector       command  *)
       (simple-array command (*))))

;;; -------------------------------------------------------

(deftype stack ()
  "The ``stack'' type defines an unbounded stack of integer numbers."
  '(list-of integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Phone call source CODE a one-dimensional
   simple array of its instructions and returns the same."
  (declare (type string code))
  (let ((instructions NIL))
    (declare (type (list-of command) instructions))
    (flet ((collect-instruction (instruction)
            "Inserts the INSTRUCTION at the front of the INSTRUCTIONS
             list and returns no value."
            (declare (type command instruction))
            (push instruction instructions)
            (values)))
      (loop
        for token    of-type character across code
        and position of-type fixnum    from   0 by 1
        do
          (case token
            (#\0
              (collect-instruction :push-zero))
            (#\1
              (collect-instruction :push-minus-one))
            (#\2
              (collect-instruction :push-two))
            (#\3
              (collect-instruction :push-three))
            (#\4
              (collect-instruction :push-five))
            (#\5
              (collect-instruction :push-seven))
            (#\6
              (collect-instruction :push-eleven))
            (#\7
              (collect-instruction :push-thirteen))
            (#\8
              (collect-instruction :pop-stack))
            (#\9
              (collect-instruction :copy-from-stack-to-accumulator))
            (#\*
              (collect-instruction :multiply-stack))
            (#\#
              (collect-instruction :jump-or-loop))
            ((#\Newline #\Space #\Tab)
              NIL)
            (otherwise
              (error "Invalid token \"~c\" at position ~d."
                token position)))))
    (the phone-call-program
      (coerce (nreverse instructions)
        '(simple-array command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Processes the Phone call INSTRUCTIONS and returns no value.
   ---
   Upon each instruction's evaluation the memory, composed of the stack
   and accumulator, is printed to the standard output, rectifying the
   lacuna of output facilities."
  (declare (type phone-call-program instructions))
  
  (when (plusp (length instructions))
    (let ((stack       NIL)
          (accumulator 0)
          (ip          0)
          (looping-p   NIL)
          (loop-start  0))
      (declare (type stack   stack))
      (declare (type integer accumulator))
      (declare (type fixnum  ip))
      (declare (type boolean looping-p))
      (declare (type fixnum  loop-start))
      
      (flet
          ((get-current-instruction ()
            "Returns the instruction located at the instruction pointer
             position IP, or ``NIL'' if the pointer violates any of the
             program marches."
            (the (or null command)
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip))))
           
           (advance ()
            "Advances the instruction pointer IP to the next location in
             the INSTRUCTIONS sequence and returns no value."
            (incf ip)
            (values))
           
           (jump-to (new-position)
            "Relocates the instruction pointer IP to the one-based
             NEW-POSITION and returns no value."
            (declare (type fixnum new-position))
            (setf ip (1- new-position))
            (values))
           
           (jump-to-loop-head ()
            "Relocates the instruction pointer IP to the position
             immediately succeeding the current loop start and returns
             no value."
            (setf ip (1+ loop-start))
            (values))
           
           (set-loop-point (position)
            "Commences a new loop demarcated at its end by the POSITION,
             if not preceded by another loop point, and returns no
             value."
            (cond
              (looping-p
                (setf loop-start (min loop-start position)))
              (T
                (setf loop-start position)
                (setf looping-p  T)))
            (values))
           
           (can-terminate-loop-p ()
            "Determines whether the loop termination condition, realized
             in the necessity of the top stack element's value being
             zero (0), is satisfied, returning on confirmation a
             ``boolean'' value of ``T'', otherwise ``NIL''."
            (the boolean
              (not (null
                (or (null  (first stack))
                    (zerop (first stack)))))))
           
           (terminate-loop ()
            "Terminates the loop and returns no value."
            (setf looping-p  NIL)
            (setf loop-start 0)
            (values))
           
           (print-memory ()
            "Prints the stack and accumulator content to the standard
             output and returns no value."
            (format T "~2&")
            (format T "~&Stack       = (~{~d~^, ~})" (reverse stack))
            (format T "~&Accumulator = ~d"           accumulator)
            (values)))
        
        (loop do
          (case (get-current-instruction)
            ((NIL)
              (cond
                ;; Invalid IP position?
                ;; => Relocate to start of program.
                ((minusp ip)
                  (setf ip 0))
                
                ;; End of program, pending loop, required termination?
                ;; => Terminate the program.
                ((and looping-p
                      (can-terminate-loop-p))
                  (terminate-loop)
                  (loop-finish))
                
                ;; End of program, pending loop, required repetition?
                ;; => Return to most recent loop start position.
                ((and looping-p
                      (not (can-terminate-loop-p)))
                  (jump-to-loop-head))
                
                ;; End of program, and no pending loop?
                ;; => Terminate program.
                (T
                  (loop-finish))))
            
            (:push-zero
              (push 0 stack)
              (advance))
            
            (:push-minus-one
              (push -1 stack)
              (advance))
            
            (:push-two
              (push 2 stack)
              (advance))
            
            (:push-three
              (push 3 stack)
              (advance))
            
            (:push-five
              (push 5 stack)
              (advance))
            
            (:push-seven
              (push 7 stack)
              (advance))
            
            (:push-eleven
              (push 11 stack)
              (advance))
            
            (:push-thirteen
              (push 13 stack)
              (advance))
            
            (:pop-stack
              (pop stack)
              (advance))
            
            (:copy-from-stack-to-accumulator
              (setf accumulator (first stack))
              (advance))
            
            (:multiply-stack
              (push (* (pop stack)
                       (pop stack))
                    stack)
              (advance))
            
            (:jump-or-loop
              (cond
                ;; Even accumulator?
                ;; => Jump to instruction at position (accumulator/2+1).
                ((evenp accumulator)
                  (jump-to (1+ (round accumulator 2))))
                
                ;; Odd accumulator?
                ;; => Commence new loop ending at IP.
                (T
                  (set-loop-point ip)
                  (advance))))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                (get-current-instruction) ip)))
          
          (print-memory)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Phone-call (code)
  "Interprets the piece of Phone call source CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely generate the powers of 2, starting with 2^1 (= 2), at
;; the stack's top.
;; 
;;   ------------------------------------------------------------------
;;   Instr. # | Token | Action
;;   ---------+-------+------------------------------------------------
;;   1        | 2     | Push 2
;;   2        | 2     | Push 2
;;   3        | *     | Multiply 2*2 (calculates the power of 2)
;;   4        | 2     | Push 2       (base for coming jump operation)
;;   5        | 9     | Copy 2 to accumulator
;;   6        | 8     | Pop 2        (not required anymore for jumping)
;;   7        | #     | Jump to instruction (accumulator/2 + 1) = 2
;;   ------------------------------------------------------------------
(interpret-Phone-call
  "2
   2*
   298#")

;;; -------------------------------------------------------

;; Set accumulator to 3, pop stack.
(interpret-Phone-call
  "398
   #")

;;; -------------------------------------------------------

;; Push the elements 0, 3, 5, 7, 11 and 13 unto the stack, copy the
;; desinent item, 13, into the accumulator, and loop until the top stack
;; element equals zero, upon each iteration popping the current top
;; item from the stack.
(interpret-Phone-call
  "034567
   9
   #
   8")
