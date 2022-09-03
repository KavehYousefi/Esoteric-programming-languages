;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Hardfish", invented by the Esolang user "Mroman2", and
;; based on Jonathan Todd Skinner's "Deadfish".
;; 
;; Concepts
;; ========
;; Hardfish tallies among the esoteric tier of programming languages.
;; Its derivation from Deadfish endows it with a similar set of
;; attributes, comprehending in particular the interactive nature and
;; reliance upon an accumulator as its sole data salvatory.
;; 
;; == HARDFISH PROGRAMS OPERATE IN AN INTERACTIVE LOOP ==
;; A program in Hardfish performs inside of an infinite loop, on each
;; iteration's inchoation querying the user for a line of input, a
;; string which harbors a sequence of zero or more instructions,
;; however, tolerating in some sense any content. All tokens are
;; executed immediately.
;; 
;; == TWO CATEGORIES OF INPUT EXIST ==
;; Whereas valid commands exert their associated effects, unrecognized
;; tokens do not inflict the program with errors; instead, any such
;; instance conditions the printing of an empty line. Hardfish's
;; capabilities include the increasing of its accumulator, certain
;; arithmetics regarding this data item, its printing to the standard
;; output, and the repetition of preceding code portions.
;; 
;; == THE ACCUMULATOR AS THE LONE DATA REPOSITORY ==
;; All program data is realized in a single value, the accumulator, a
;; signed integer datum of unrestrained magnitude.
;; 
;; Begotten by Deadfish, a particular aspect of its haecceity has been
;; transmitted into the scion: the dioristic normalization of its
;; accumulator. After each operation engaged in its manipulation, the
;; value is perquired; upon equality to -1 or 256 the accumulator is
;; reset to zero, otherwise its current value remains preserved. In
;; pseudocode notation the following definition applies:
;; 
;;   if (accumulator = -1) or (accumulator = 256) then
;;     accumulator <- 0
;;   end if
;; 
;; 
;; Architecture
;; ============
;; Hardfish's architectural department is exhausted by a single scalar
;; storage, commonly known as the "accumulator", unbridled regarding
;; sign and magnitude.
;; 
;; 
;; Data Types
;; ==========
;; In its most basic rendition, Hardfish programS operate on signed
;; integer data only. However, implementations enjoy the homologation of
;; character representations during an output.
;; 
;; == INTEGERS ==
;; The primary type deployed in programs is comprised by signed integers
;; of any magnitude and sign, signified in particular by its role in
;; the accumulator's representation. Also, if adjudging fidelity with
;; Deadfish a paravaunt component, the output operation "o" commits its
;; data to the output conduit in the form of this type.
;; 
;; == CHARACTERS ==
;; A rather confined, and thoroughly optional engagement, ASCII
;; characters may be ordained by an implementation to realize the
;; accumulator output in lieu of a verbatim integer reproduction. In
;; such cases, the output item is yielded by construe of the accumulator
;; value as the ASCII code of the character to submit.
;; 
;; 
;; Syntax
;; ======
;; The language's Deadfish cleronomy serves to endow the syntaxis with a
;; lenience toward any input, the responsiveness barters the signaling
;; of errors for newline commissions.
;; 
;; == INSTRUCTIONS ==
;; Any instruction in Hardfish is compact of a single character from the
;; quintuple set { "c", "i", "o", "q", "r" }. A particular restriction
;; imposed by the language proscribes the repetition of two equal
;; commands in immediate succession --- irregardless of contingent
;; non-command characters' interposition.
;; 
;; == LINEBREAKS ==
;; Hardfish programs operate in a perpetually inquirying interactive
;; loop, prompting the user for a line of input, which may contain an
;; arbitrary tally of commands and superfluous constituents. Linebreaks
;; thus serve to segregate such sequences.
;; 
;; == SPACES ==
;; Spaces, conflating therein with any other non-command tokens, are
;; received with tolerance.
;; 
;; == GRAMMAR ==
;; Hardfish's donat may be reproduced in the given Extended Backus-Naur
;; Form (EBNF) description:
;; 
;;   program    := { token } ;
;;   token      := command | nonCommand ;
;;   nonCommand := character - command ;
;;   command    := "c" | "i" " | "o" | "q" | "r" ;
;; 
;; 
;; Instructions
;; ============
;; Being a derivative of the Deadfish programming language, Hardfish
;; appropriates a subset of operations and a select basic concepts;
;; however, extending the heritage by dioristic capabilities.
;; 
;; == OVERVIEW ==
;; The quintuple capacitation administered to Hardfish shall be a
;; cursory perusal's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator by one.
;;   ..................................................................
;;   o       | Outputs the accumulator in an implementation-dependent
;;           | fashion --- either as a number or the associated ASCII
;;           | character.
;;   ..................................................................
;;   c       | If the accumulator is even, sets it to its own half;
;;           | otherwise triples its value and increases it by one.
;;   ..................................................................
;;   q       | If the accumulator can be divided by 3 without rest,
;;           | sets the accumulator to itself divided by 3; otherwise
;;           | doubles the accumulator's value and increases it by one.
;;   ..................................................................
;;   r       | Removes this "r" instruction instance from the source
;;           | code and repeats all instructions preceding it once.
;;   ------------------------------------------------------------------
;; 
;; == INCREMENT OPERATION "i" ==
;; Increments the accumulator by one.
;; 
;; Signature:
;;   i
;; 
;; Pseudocode:
;;   accumulator <- accumulator + 1
;; 
;; Description:
;;   Increments the accumulator value by a quantity of one (1).
;; 
;; Side effects:
;;   - The accumulator is modified.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == OUTPUT OPERATION "o" ==
;; Outputs the accumulator in an implementation-dependent way.
;; 
;; Signature:
;;   o
;; 
;; Pseudocode --- numeric variant:
;;   print accumulator
;; Pseudocode --- ASCII character variant:
;;   print getASCIICharacterForCode(accumulator)
;; 
;; Description:
;;   Prints the accumulator value in an implementation-dependent way to
;;   the standard output, usually the console.
;;   
;;   While the output mode depends on the concrete interpreter
;;   implementation, the most common choices resolve to either a
;;   verbatim numeric display, destitute of a specific formatting, or
;;   the reproduction as an ASCII character by employing the accumulator
;;   as an ASCII code.
;; 
;; Side effects:
;;   - Prints an implementation-dependent content to the standard
;;     output.
;; 
;; Exceptional situations:
;;   - The effect is undefined if the accumulator value shall be printed
;;     as an ASCII character, yet its value does not correspond to any
;;     valid destination entity.
;;   - Anomalies and problems of the system's output conduit may
;;     reverberate in the interpreter behavior.
;; 
;; == HALFING OPERATION "c" ==
;; Divides the accumulator by two (2) if this is possible without
;; producing a remainder; otherwise triples its value and increases it
;; by one (1).
;; 
;; Signature:
;;   c
;; 
;; Pseudocode:
;;   if accumulator is even then
;;     accumulator <- accumulator / 2
;;   else
;;     accumulator <- (3 * accumulator) + 1
;;   end if
;; 
;; Description:
;;   If the accumulator represents a value that can be divided by two
;;   (2) without a remainder, it is updated via halfing:
;;     accumulator = accumulator / 2.
;;   Otherwise, the accumulator value is set to
;;     accumulator = (3 * accumulator) + 1.
;; 
;; Side effects:
;;   - The accumulator is modified.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == TRIPARTITION OPERATION "q" ==
;; Divides the accumulator by three (3) if this is possible without
;; producing a remainder; otherwise doubles its value and increases it
;; by one (1).
;; 
;; Signature:
;;   q
;; 
;; Pseudocode:
;;   if 3 is an aliquot of accumulator then
;;     accumulator <- accumulator / 3
;;   else
;;     accumulator <- (2 * accumulator) + 1
;;   end if
;; 
;; Description:
;;   If the accumulator represents a value that can be divided by three
;;   (3) without a remainder, it is updated via a division by three:
;;     accumulator = accumulator / 3.
;;   Otherwise, the accumulator value is set to
;;     accumulator = (2 * accumulator) + 1.
;; 
;; Side effects:
;;   - The accumulator is modified.
;; 
;; Exceptional situations:
;;   - None.
;; 
;; == REPETITION OPERATION "r" ==
;; Repeats all instructions preceding this "r" occurrence once, then
;; loses its effectivity.
;; 
;; Signature:
;;   r
;; 
;; Pseudocode:
;;   let rPosition <- position of this "r" command instance
;;   set instruction at rPosition to no-operation
;;   for p from 1 to (rPosition - 1)
;;     execute instruction at position p
;;     advance instruction pointer to p + 1
;;   end for
;;   skip instruction at position rPosition
;; 
;; Description:
;;   Repeats all instructions preceding this occurrence of "r" once,
;;   concomitantly removing the "r" command itself or rendering it
;;   permanently ineffectuous. In any case of formulation and
;;   realization, this instance of "r" will not take effect again.
;; 
;; Side effects:
;;   - All side effects of the instructions preceding this command are
;;     repeated.
;;   - The processed instance of the "r" command is removed or marked as
;;     ineffective.
;; 
;; Exceptional situations:
;;   - All exceptional situations appertaining to the instructions
;;     preceding this command permeate.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-03
;; 
;; Sources:
;;  -> "https://esolangs.org/wiki/Hardfish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized Hardfish commands, in
   conjunction with two adscititious members operating for internal
   purposes.
   ---
   A twain of participants does not account for an autochthonous
   commorant of Hardfish's instruction set:
     
     :no-op
       This command avails to \"disable\" a repetition command \"r\"
       prior to its execution in order to obviate its erroneous iterum
       invocation.
     
     :unrecognized
       A command which, by Deadfish's cleronomy, caters for the
       dioristic response to unrecognized tokens with a newline output."
  '(member
    :output
    :increment
    :repeat
    :modulo-2
    :modulo-3
    :no-op
    :unrecognized))

;;; -------------------------------------------------------

(deftype output-mode ()
  "The ``output-mode'' type enumerates the recognized policies for
   printing a Hardfish program's accumulator value."
  '(member :numeric :character))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for-character (character)
  "Returns the Hardfish command associated with the CHARACTER.
   ---
   A non-command CHARACTER will yield the ``:unrecognized'' element."
  (declare (type character character))
  (the command
    (case character
      (#\o       :output)
      (#\i       :increment)
      (#\r       :repeat)
      (#\c       :modulo-2)
      (#\q       :modulo-3)
      (otherwise :unrecognized))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts and returns from the piece of Hardfish CODE a
   one-dimensional simple array of commands.
   ---
   Please note that each character of the CODE is transformed into a
   command, with non-command constituents being substituted by
   ``:unrecognized'' members."
  (declare (type string code))
  (the (simple-array command (*))
    (map '(simple-array command (*))
      #'get-command-for-character code)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type integer     *accumulator*))
(declaim (type output-mode *output-mode*))

;;; -------------------------------------------------------

(defparameter *accumulator* 0
  "The program's storage, represented by a scalar integer of no
   restriction considering either sign or magnitude.")

(defparameter *output-mode* :numeric
  "The kind of output to commit.")

;;; -------------------------------------------------------

(defun check-for-instruction-duplications (instructions)
  "Scans the INSTRUCTIONS for duplications, that is, two or more
   instructions of the same kind in immediate succession, which violates
   the conformity to Hardfish, on affirmation of at least one instance
   signaling an error of an unspecified type; otherwise returning the
   unmodified INSTRUCTIONS."
  (declare (type (vector command *) instructions))
  (flet
      ((effective-instruction-p (instruction)
        "Checks whether the INSTRUCTION represents an \"effective\"
         operation, that is, one of the quintuple operations assigned an
         actual effect, returning on confirmation a ``boolean'' value of
         ``T'', otherwise ``NIL''."
        (declare (type command instruction))
        (the boolean
          (not
            (member instruction '(:no-op :unrecognized) :test #'eq)))))
      (loop
        ;; Previous effective instruction.
        with previous-instruction of-type (or null command) = NIL
        ;; Index of PREVIOUS-INSTRUCTION.
        with previous-index       of-type fixnum            = 0
        for  current-instruction  of-type command across instructions
        and  instruction-index    of-type fixnum  from    0
        do
          ;; Does a PREVIOUS-INSTRUCTION exist and concord with the
          ;; current one?
          ;; => Duplication.
          (when (and previous-instruction
                     (eq current-instruction previous-instruction))
            (error "Duplicate instruction ~s at position ~d and ~d."
              current-instruction previous-index instruction-index))
          
          (when (effective-instruction-p current-instruction)
            (setf previous-instruction current-instruction)
            (setf previous-index       instruction-index))))
  (the (vector command *) instructions))

;;; -------------------------------------------------------

(defun output-accumulator ()
  "Prints the value of the *ACCUMULATOR* to the standard output in a
   fashion appropriate for the currently set *OUTPUT-MODE* and returns
   no value."
  (case *output-mode*
    (:numeric
      (format T "~d" *accumulator*))
    (:character
      (format T "~c" (code-char *accumulator*)))
    (otherwise
      (error "Invalid output mode: ~s." *output-mode*)))
  (values))

;;; -------------------------------------------------------

(defun normalize-accumulator ()
  "Normalizes the accumulator by setting it to zero if its value equals
   -1 or 256, otherwise retaining its current state, in any case
   returning no value."
  (when (or (= *accumulator* -1)
            (= *accumulator* 256))
    (setf *accumulator* 0))
  (values))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the INSTRUCTIONS and returns no value.
   ---
   Dependencies:
     - The concrete output depends upon the *OUTPUT-MODE* configuration.
   ---
   Side effects:
     - The *ACCUMULATOR* might be modified."
  (declare (type (vector command *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip                  0)
          (current-instruction (aref instructions 0)))
      (declare (type fixnum            ip))
      (declare (type (or null command) current-instruction))
      
      (flet
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the CURRENT-INSTRUCTION, and returns
             no value."
            (setf current-instruction
              (when (array-in-bounds-p instructions (1+ ip))
                (aref instructions (incf ip))))
            (values))
           
           (repeat ()
            "Relocates the instruction pointer IP to the first
             instruction, updates the CURRENT-INSTRUCTION, and returns
             no value."
            (setf ip 0)
            (setf current-instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions 0)))
            (values))
           
           (disable-current-instruction ()
            "Converts the instruction at the instruction pointer
             location IP to a no-operation, thus effectively eradicating
             it from the INSTRUCTIONS vector, and returns no value."
            (setf (aref instructions ip) :no-op)
            (values)))
        
        (loop while current-instruction do
          (case current-instruction
            ;; End of INSTRUCTIONS.
            ((NIL)
              (loop-finish))
            
            ;; "o".
            (:output
              (output-accumulator)
              (advance))
            
            ;; "i".
            (:increment
              (incf *accumulator*)
              (advance))
            
            ;; "r".
            (:repeat
              (disable-current-instruction)
              (repeat))
            
            ;; "c".
            (:modulo-2
              (setf *accumulator*
                (if (evenp *accumulator*)
                  (round *accumulator* 2)
                  (+ (* 3 *accumulator*) 1)))
              (advance))
            
            ;; "q".
            (:modulo-3
              (setf *accumulator*
                (if (zerop (rem *accumulator* 3))
                  (round *accumulator* 3)
                  (+ (* 2 *accumulator*) 1)))
              (advance))
            
            ;; An "r" command deactivated prior to its execution.
            (:no-op
              (advance))
            
            ;; As with Deadfish, unrecognized tokens elicit a newline
            ;; output in lieu of an error signal.
            (:unrecognized
              (terpri)
              (advance))
            
            (otherwise
              (error "Invalid instruction: ~s."
                current-instruction)))
          
          (normalize-accumulator)))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-code (code)
  "Interprets the piece of Hardfish CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (check-for-instruction-duplications
      (extract-instructions code)))
  (values))

;;; -------------------------------------------------------

(defun execute-Hardfish (&key (output-mode :numeric))
  "Commences the Hardfish shell, optionally employing the specified
   OUTPUT-MODE in this context, and returns no value if ever aborted.
   ---
   Please note that this shell is intended for infinite continuance; a
   cessation thus constitutes an anomaly."
  (declare (type output-mode output-mode))
  (setf *accumulator* 0)
  (let ((*output-mode* output-mode))
    (declare (type output-mode *output-mode*))
    (loop do
      (format T "~&>> ")
      (let ((input (read-line)))
        (declare (type string input))
        (interpret-code input))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test the repetition command "r".
(interpret-code "icricro")

;;; -------------------------------------------------------

;; Generate and print the constant number 32.
(setf *accumulator* 0)
(interpret-code "qiririro")

;;; -------------------------------------------------------

(execute-Hardfish)
