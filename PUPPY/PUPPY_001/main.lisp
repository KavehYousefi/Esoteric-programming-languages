;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "PUPPY", invented by the Esolang user "Josh" in the year
;; 2017, and based upon the mimicry of vocal canine communication as
;; means for realizing the program design.
;; 
;; Concept
;; =======
;; The PUPPY language simulates the vocal expressions of dogs, with the
;; words "BARK" and "WOOF", in conjunction with the space character,
;; comprehending the exclusive elements of currency. The programs
;; operate on a single scalar integer, the accumulator, which might be
;; printed in its ASCII character form.
;; 
;; == PUPPY: THE LANGUAGE OF DOGS ==
;; PUPPY is subsumed into the joke language department of the esoteric
;; taxonomy, indulging in the most dedicated form of mateotechny.
;; 
;; Its emblematic contribution wones in the emulation of typical canine
;; vociferations, namely, "BARK" and "WOOF", as the paravaunt expressive
;; constituents, in the compernage of the space as a third element. Both
;; minuscular and majuscular forms, even mingled in a single term, are
;; vouchsafed adit.
;; 
;; == PROGRAMS MANIPULATE AN ACCUMULATOR ==
;; All operative entities ultimately manipulate the program's memory,
;; realized in a single scalar integer of any sign and magnitude.
;; 
;; Counterdistinguished from many programming language's wont, the
;; discrepancy betwixt the two chief keywords, "BARK" and "WOOF",
;; maintains no signifying incarnation --- instead, the choice of
;; lowercase and uppercase letters affects the result:
;; 
;;   - A minuscular letter decrements the accumulator by one.
;;   - A majuscular letter increments the accumulator by one.
;; 
;; Inside of these two tokens, spaces may be interspersed liberally and
;; are assigned no effect. When adduced around these word, however, each
;; space translates to an output instruction, printing to the standard
;; conduit the ASCII character associated with the accumulator value as
;; its code.
;; 
;; == ONLY EVEN NUMBERS CAN BE PRODUCED ==
;; PUPPY's capacitation is limited to the production of even values,
;; both along the positive and negative number scale, as attested in the
;; following schematic association of letters to increment/decrement
;; behests:
;; 
;;   B  A  R  K
;;   W  O  O  F
;;  +1 +1 +1 +1 = +4
;;  +1 +1 +1 -1 = +2
;;  +1 +1 -1 -1 =  0
;;  +1 -1 -1 -1 = -2
;;  -1 -1 -1 -1 = -4
;; 
;; Please note that all permutations abstain from influencing the
;; outcome of a combination, as merely the occurrences appropriate
;; significance. As an example, the second line
;; 
;;   +1 +1 +1 -1 = +2
;; 
;; is perfectly equivalent to its alternative arrangements:
;; 
;;   +1 +1 -1 +1 = +2
;;   +1 -1 +1 +1 = +2
;;   -1 +1 +1 +1 = +2
;; 
;; The program memory's inchoation at the value zero (0) establishes its
;; destination in assuming two's multiples.
;; 
;; 
;; Architecture
;; ============
;; The program memory comprehends a singular entity, a scalar integer
;; storage akin to an accumulator or a register, unbounded in the sign
;; and magnitude of its element. Operations exist to gradually increment
;; or decrement the state, or transfer its value along an output
;; conduit.
;; 
;; 
;; Data Types
;; ==========
;; A bifurcated type system manifests in PUPPY, with signed integers
;; appropriating the parauvant echolon, whereas characters, being the
;; secondary objects, are deigned merely an output role.
;; 
;; == PROGRAMS OPERATE ON INTEGERS ==
;; The fact that the program memory consists of a scalar integer value
;; in the liberal range [-infinity, +infinity] already intimates the
;; superior role of the numeric moeity in the language. All three
;; operations --- increment, decrement, and output --- rely on this
;; datum's manipulation.
;; 
;; == CHARACTERS PARTICIPATE ONLY IN THE OUTPUT ==
;; The sole admission of the character type transpires during the print
;; operation, an instruction that translates the numeric accumulator to
;; its ASCII character equivalent in order to write it to the standard
;; output conduit.
;; 
;; 
;; Syntax
;; ======
;; PUPPY programs entail merely the tokens "BARK" and "WOOF", in an
;; arbitrary mixture of minuscular and majuscular form, and spaces. Any
;; of these three tokens serve as instruction representatives.
;; 
;; == INSTRUCTIONS ==
;; PUPPY wists of three operative tokens only: "BARK", "WOOF", and
;; spaces, the former twain may be expressed in any design regarding the
;; case in order to assume the desiderated effect.
;; 
;; The "BARK" and "WOOF" exposition may incorporate spaces, which are
;; detected and neglected --- as opposed to such occurrencies in the
;; interstices or the code margins.
;; 
;; == SPACES ==
;; Beside the letters participating in the terms "BARK" and "WOOF", in
;; any combination of cases, the space character appropriates a
;; duplicate role.
;; 
;; Its insertion betwixt identifier tokens, that is, both "BARK" and
;; "WOOF", but also spaces in its own visne, evaluates to the output
;; instruction.
;; 
;; Its application inside of a valid "BARK" or "WOOF" token, on the
;; other hand, is administered as much patience and as it lacks effect.
;; 
;; == COMMENTS ==
;; No provisions for comments are embraced in the current language
;; rendition.
;; 
;; == FURTHER CONTENT ==
;; Any other character is inflicted with interdiction.
;; 
;; == GRAMMAR ==
;; PUPPY's donat, constituting a forbisen of simplicity, may be molded
;; into the following Extended Backus-Naur Form (EBNF):
;; 
;;   program        := { command } ;
;;   command        := bark | woof | output ;
;;   bark           := ( "b" | "B" ) , optionalSpaces ,
;;                     ( "a" | "A" ) , optionalSpaces ,
;;                     ( "r" | "R" ) , optionalSpaces ,
;;                     ( "k" | "K" ) , optionalSpaces
;;                  ;
;;   woof           := ( "w" | "W" ) , optionalSpaces ,
;;                     ( "o" | "O" ) , optionalSpaces ,
;;                     ( "o" | "O" ) , optionalSpaces ,
;;                     ( "f" | "F" ) , optionalSpaces
;;                  ;
;;   output         := space ;
;;   optionalSpaces := { space } ;
;;   space          := " " ;
;; 
;; 
;; Implementation
;; ==============
;; In commensuration with the PUPPY language's facile haecceity, this
;; implementation in Common Lisp adheres to a thoroughly simplistic
;; design. In particular, the repeated creation of lists and their
;; appendage to each other's tail constitutes an act of betise if
;; applied in a serious project, forcause penalties in performance will
;; indubitably arise.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-22
;; 
;; Sources:
;;   [esolang2017PUPPY]
;;   The Esolang contributors, "PUPPY", April 9th, 2017
;;   URL: "https://esolangs.org/wiki/PUPPY"
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

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized operations."
  '(member :decrement :increment :output))

;;; -------------------------------------------------------

(deftype instruction-set ()
  "The ``instruction-set'' type defines a list of zero or more
   instructions."
  '(list-of instruction))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (character)
  "Checks whether the CHARACTER represents a space, returnign on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean (not (null (char= character #\Space)))))

;;; -------------------------------------------------------

(declaim (type (or null string)    *source*))
(declaim (type fixnum              *position*))
(declaim (type (or null character) *character*))

;;; -------------------------------------------------------

(defparameter *source* NIL
  "The piece of PUPPY source code to interpret.")

(defparameter *position* 0
  "The current index into the *SOURCE*.")

(defparameter *character* NIL
  "The character at the current *POSITION* into the *SOURCE*.")

;;; -------------------------------------------------------

(defun advance ()
  "Moves the *POSITION* cursor to the next character in the *SOURCE*,
   if possible, updates the current *CHARACTER*, and returns no value."
  (setf *character*
    (when (array-in-bounds-p *source* (1+ *position*))
      (char *source* (incf *position*))))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces ()
  "Starting at the current *POSITION* in the *SOURCE*, skips a sequence
   of zero or more adjacent spaces and returns no value."
  (loop while (and *character* (space-character-p *character*)) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun signal-unrecognized-word-error
    (start-position
     &optional (end-position (length *source*)))
  "Signals an error of an unspecified type which informs about the
   portion of the *SOURCE* between the START-POSITION and the
   END-POSITION encompassing an unrecognized token."
  (declare (type fixnum start-position))
  (declare (type fixnum end-position))
  (error "I do not understand your code: The word ~s, extending ~
          between the positions ~d and ~d, seems foreign to me."
    (subseq *source* start-position end-position)
    start-position end-position))

;;; -------------------------------------------------------

(defun read-word (word)
  "Attempts to read the characters of the WORD, starting at the current
   *POSITION* in the *SOURCE*, on confirmation returning the associated
   instruction in an instruction list; otherwise signaling an error of
   an unspecified type."
  (declare (type string word))
  (let ((instructions  NIL)
        (start-position *position*))
    (declare (type instruction-set instructions))
    (declare (type fixnum          start-position))
    (loop for expected-character of-type character across word do
      (skip-spaces)
      (cond
        ;; The *SOURCE* is exhausted?
        ;; => The WORD is only partially found, if at all.
        ((null *character*)
          (signal-unrecognized-word-error start-position))
        ((and (char-equal *character* expected-character)
              (upper-case-p expected-character))
          (push :increment instructions)
          (advance))
        ((and (char-equal *character* expected-character)
              (upper-case-p expected-character))
          (push :decrement instructions)
          (advance))
        (T
          (signal-unrecognized-word-error start-position *position*))))
    (the instruction-set (nreverse instructions))))

;;; -------------------------------------------------------

(defun read-space ()
  "Attempts to read a space character, on success returning an output
   instruction, otherwise signaling an error of an unspecified type."
  (case *character*
    (#\Space
      (advance)
      :output)
    (otherwise
      (error "Expected a space, but encountered ~s at position ~a."
        *character* *position*))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts from the piece of PUPPY CODE a list of its instructions and
   returns the same."
  (setf *source*   code)
  (setf *position* 0)
  (setf *character*
    (when (array-in-bounds-p *source* *position*)
      (char *source* *position*)))
  
  (let ((instructions NIL))
    (declare (type instruction-set instructions))
    (loop while *character* do
      (case *character*
        ((NIL)
          (loop-finish))
        ((#\B #\b)
          (setf instructions
            (nconc instructions (read-word "BARK"))))
        ((#\W #\w)
          (setf instructions
            (nconc instructions (read-word "WOOF"))))
        (#\Space
          (setf instructions
            (nconc instructions (list (read-space)))))
        (otherwise
          (signal-unrecognized-word-error *position* (1+ *position*)))))
    (the instruction-set instructions)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Processes the PUPPY INSTRUCTIONS and returns no value."
  (declare (type instruction-set instructions))
  (let ((accumulator 0))
    (declare (type integer accumulator))
    (loop for instruction of-type instruction in instructions do
      (case instruction
        ((NIL)
          (loop-finish))
        (:decrement
          (decf accumulator))
        (:increment
          (incf accumulator))
        (:output
          (write-char (code-char accumulator)))
        (otherwise
          (error "I do not understand this command: ~s."
            instruction)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-PUPPY (code)
  "Interprets the piece of PUPPY CODE and returns no value."
  (declare (type string code))
  (process-instructions (extract-instructions code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "H".
(interpret-PUPPY "BARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOF ")

;;; -------------------------------------------------------

;; Print "H", while demonstrating the tolerance of spaces betwixt valid
;; token characters.
(interpret-PUPPY "BA RKWO OFB ARKWOO FBAR KW OOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOFBARKWOOF ")
