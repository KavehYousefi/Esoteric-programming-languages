;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements an interpreter for the esoteric programming
;; language "AEWNN", devised by the Esolang user "ResU".
;; 
;; Concept
;; =======
;; AEWNN, an acronym of "An esolang with no name", is founded upon an
;; interplay of a single numeric variable with a fixed set of variables
;; holding letters, manipulating the former and printing using the
;; latter.
;; 
;; == TWO CATEGORIES OF VARIABLES EXIST ==
;; A unique, anonymous variable called the "VariableWithNoName" exists
;; which stores a non-negative integer in the range [0, 52]. A fixed
;; set of 23 variables, known as "letter variables", are available,
;; each a defined by a single minuscular letter, excluding those
;; reserved for command identifiers --- "c", "p", and "r". Any of these
;; variables stores one letter.
;; 
;; == THE CHARACTER SET INCLUDES LETTERS ONLY ==
;; The valid character repertoire, which concomitantly defines the
;; possible values for the letter variables, consists of the Latin
;; upper-case and lower-case characters A through Z and a through z.
;; In lieu of the ASCII character encoding, a dedicated version realizes
;; the association of an integer in the range [0, 52] to a
;; letter, yielding
;;   Number | Character
;;   -------+----------
;;   1      | A
;;   2      | B
;;   3      | C
;;   ...    | ...
;;   26     | Z
;;   27     | a
;;   28     | b
;;   29     | c
;;   ...    | ...
;;   52     | z
;; This set patently excludes a large portion of the ASCII repertoire,
;; in particular digits and common symbols. It is also of significance
;; to mention that the value zero (0) does not allude to any character;
;; its association shall hence be regarded as a "null value", that is,
;; a non-letter or absence of letter variable content.
;; 
;; == THE INTERACTION OF THE VARIABLES DRIVES A PROGRAM ==
;; The VariableWithNoName and the letter variables establish two realms
;; of distinguished utility which only by cooperation on both sides can
;; embue a program with meaningful effectivity.
;; 
;; The VariableWithNoName effectively contains one of the 52
;; AEWNN-specific character codes, with the additional value of zero
;; resolving to the "null value". All manipulating operations in the
;; language are defined on this one variable, which include:
;;   - The incrementing of its value by one ("+" command).
;;   - The resetting of its value to an initial zero (0) ("r2" command).
;; These capabilities, however, lack a printing command.
;; 
;; The letter variables, on the other hand, participate in the primary
;; communicative instruction, the output of the stored content using
;; the "p" instruction, while missing any routines for manipulation.
;; 
;; A reasonable program must hence combine the means of both variable
;; categories if intending to achieve some effect. AEWNN to this end
;; provides commands for the bidirectional conversion:
;;   - The numeric value of the VariableWithNoName can be converted
;;     into a character and stored in a specified letter variable by
;;     aid of the "c" command.
;;   - The character value stored in a letter variable can be converted
;;     into a numeric object and transfered into the VariableWithNoName
;;     through mediation of the "r" command.
;; By operating upon the VariableWithNoName, transferring its value into
;; the letter variables, and printing their characters, one might create
;; a functioning piece of code.
;; 
;; 
;; Architecture
;; ============
;; The AEWNN programming language does not depend upon any particular
;; kind of architecture, its state being exclusively defined by its
;; variables.
;; 
;; 
;; Syntax
;; ======
;; AEWNN employs letters, digits, spaces, and brackets ('[', ']') as
;; code constituents.
;; 
;; == COMMANDS ==
;; Commands are of at most two characters' width, minuscules if letters,
;; and may in the usual case accept one optional argument. Exempt from
;; this pattern, the "space" command does not tolerate additional data,
;; and the iteration construct "[...]" accepts, besides its tally of
;; repetitions, an arbitrary number of commands.
;; 
;; == VARIABLES ==
;; The VariableWithNoName operates latently by mediation of its various
;; routines. Juxtaposed to this, letter variables constitute a fixed
;; set of 23 minuscular letters.
;; 
;; == Numbers ==
;; Numbers occur only in the form of non-negative, unsigned integer
;; values, solely installed as the tally of repetitions assumed by a
;; loop construct.
;; 
;; == GRAMMAR ==
;; The following EBNF describes the grammar of the AEWNN language:
;; 
;;   command        := "+"
;;                  |  "c" , { letterVariable }
;;                  |  "p" , { letterVariable }
;;                  |  "r" , { letterVariable }
;;                  |  "r2"
;;                  |  space
;;                  |  "[", integer , { command } , "]" ;
;;   space          := " " ;
;;   letterVariable := "a" | "b" | "d" | "e" | "f" | "g" | "h" | "i" | "j"
;;                  |  "k" | "l" | "m" | "n" | "o" | "q" | "s" | "t" | "u"
;;                  |  "v" | "w" | "x" | "y" | "z" ;
;;   integer := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Commands
;; ========
;; The instruction array encompasses seven commands:
;;   
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the value of the VariableWithNoName by one.
;;           | An error is signaled if the value transgresses the
;;           | maximum of 52.
;;   ..................................................................
;;   cX      | Converts the numeric value of the VariableWithNoName
;;           | into its associated character and stores it in the
;;           | letter variable designated by "X". If the destination is
;;           | omitted, the standard variable "a" is assumed.
;;   ..................................................................
;;   pX      | Prints the character stored in the letter Variable
;;           | designated by "X". If the argument is omitted, the
;;           | standard variable "a" is assumed. If the letter variable
;;           | does not hold a datum yet, nothing is printed.
;;   ..................................................................
;;   rX      | Converts the character stored in the letter variable
;;           | designated by "X" into its numeric value and stores it
;;           | in the VariableWithNoName. If "X" is omitted, the
;;           | standard variable "a" is assumed.
;;   ..................................................................
;;   r2      | Resets the VariableWithNoName to its default value 0.
;;   ..................................................................
;;   " "     | The space character is printed verbatim.
;;   ..................................................................
;;   [XY...] | Repeats the command sequence "Y...", composed of zero or
;;           | more commands, a tally of "X" times. If "X" is not a
;;           | non-negative integer number, an error is signaled. If
;;           | the tally equals zero, the command sequence is skipped.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; A few questions, not least issuing from the recency of the language
;; at the instant of the writing, arise; a significant parcel shall now
;; be enumerated:
;;   (1) Which character maps to a value of zero?
;;       o The original specification actually implies --- somewhat
;;         contradictionary --- the mapping of integer values in the
;;         range [1, 52] to characters.
;;       o A value of zero, which is a legitimate datum to the
;;         VariableWithNoName, is however never clarified in its effect
;;         on a conversion into a letter variable's character object.
;;       o It is assumed that a zero integer maps to a special "null
;;         object", different from the ASCII "null character", which is
;;         persisted in a letter variable, but does not print anything.
;;   (2) Which default value do letter variables assume?
;;       o While the VariableWithNoName assumes an inital value of zero,
;;         the program default of letter variables in not disquisited.
;;       o With respect to symmetry in all variables, the starting datum
;;         of a letter variable is chosen to reflect the zero number's
;;         imputed mapping of the "null object".
;; 
;; 
;; Implementation
;; ==============
;; The minimalistic design of the language projects into this
;; implementation.
;; 
;; The VariableWithNoName manifests as an unsigned integer variable,
;; whereas the letter variables enjoy their castaldy in the form of
;; a hash table, mapping to each variable name character a value which
;; itself is either a character or the ``NIL'' value, with the former
;; representing the entities associated with the character codes 1 to
;; 52, and the latter being a delegate of the "null" object, that is,
;; the analogue to integer datum zero (0).
;; 
;; The iteration construct ('[...]') introduces some convolution into
;; the design of the interpreter, as, apart from the conjoining and
;; topological retrieval of matching bracket pairs, the tally of
;; remenant iterations requires temporary persistence. In pursue of this
;; predicament's resolution a structure class ``AEWNN-Loop'' has been
;; incorporated, its two components being
;;   (1) The remaining number of repetitions. Incipiently, its value
;;       concurs with the integer value stated in the AEWNN source code,
;;       but is decremented upon each encounter with the matching
;;       terminating bracket ']'.
;;   (2) The position of the loop body following the opening bracket
;;       '['. This designates the location immediately following the
;;       mandatory integer tally of repetitions.
;; A stack of zero or more ``AEWNN-Loop'' instances is maintained,
;; subjected to the following principles:
;;   (1) If an opening bracket '[' is encountered, its tally of
;;       repetitions and subsequent location, being the start position
;;       of the first command ensconced by the loop body, are stored
;;       in a new ``AEWNN-Loop'' object. This entity is then pushed to
;;       the top of the loop stack, thus becoming the active loop.
;;   (2) The body commands are executed in the usual fashion, including
;;       the contingency of nested loops, which are handled according to
;;       the point (1) above.
;;   (3) Upon encountering a closing bracket ']', the topmost
;;       ``AEWNN-Loop'' on the stack is indagated and its repetition
;;       count decremented, as one iteration must have navigated it to
;;       this token. If a count of at least one remains, the loop cannot
;;       be exhausted yet, in which case the instruction pointer is
;;       relocated to the loop body start position as stored in this
;;       inquired object, instigating a further processing of its
;;       commands. When the tally descends to zero or below, the
;;       iteration must be terminated, realized by removing the active
;;       loop from the top of the stack and advancing the instruction
;;       pointer past this closing bracket.
;; A special attendance must be practiced in the case of a loop with
;; with an initial count of zero, which entails the skipping of all
;; characters until the matching closing bracket is found and the
;; instruction pointer advanced to the first token to its rear.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-11-09
;; 
;; Sources:
;;   "https://esolangs.org/wiki/AEWNN"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack (&optional (element-type T))
  "The ``stack'' type defines a linear last-in-first-out (LIFO) data
   structure based upon a list, the zero or more elements of which must
   conform to the ELEMENT-TYPE."
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

(deftype variable-set ()
  "The ``variable-set'' type defines a data structure suitable for the
   maintenance of letter variables in the form of a hash table, mapping
   character-typed keys to values of the same type or ``NIL''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   'character)
                     (typep value '(or null character)))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "AEWNN-Loop".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (AEWNN-Loop
  (:constructor make-aewnn-loop (repetitions body-start)))
  "The ``AEWNN-Loop'' bundles the data necessary for representing an
   iteration construct."
  (repetitions 0 :type (integer 0 *))
  (body-start  0 :type fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string +LETTERS+))
(declaim (type simple-string +VARIABLE-NAMES+))

;;; -------------------------------------------------------

(defparameter +LETTERS+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Defines the repertoire of recognized output letters, the positions
   of which corresponding to variable values if increased by one.
   ---
   The letter 'A', for instance, resides at the position 0; its number,
   however, if referred to by a variable, must be increased to 1.")

(defparameter +VARIABLE-NAMES+
  "abdefghijklmnoqstuvwxyz"
  "The set of valid variable names.
   ---
   Note that these identifiers avoid intersection with instructions
   names in order to retain unambiguity.")

;;; -------------------------------------------------------

(defun get-letter-for (number)
  "Returns the letter associated with the number."
  (declare (type (integer 1 52) number))
  (the character (schar +LETTERS+ (1- number))))

;;; -------------------------------------------------------

(defun is-letter-variable (character)
  "Checks whether the CHARACTER specifies a letter variable, returning
   on affirmation the CHARACTER itself, otherwise ``NIL''."
  (declare (type (or null character) character))
  (the (or null character)
    (when character
      (find character +VARIABLE-NAMES+ :test #'char=))))

;;; -------------------------------------------------------

(defun interpret-AEWNN (code)
  "Interprets the piece of AEWNN CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (let ((variable-with-no-name 0)
            (letter-variables      (make-hash-table :test #'eql))
            (loops                 NIL))
        (declare (type (integer 0 52)     variable-with-no-name))
        (declare (type variable-set       letter-variables))
        (declare (type (stack AEWNN-Loop) loops))
        
        (labels
            ((advance ()
              "Moves the POSITION one step forward, if possible, and
               updates the CHARACTER to the next CODE element, returning
               the new character or ``NIL'' on exhaustion."
              (the (or null character)
                (if (< position (1- (length code)))
                  (setf character (char code (incf position)))
                  (setf character NIL))))
             
             (read-number ()
              "Reads from the CODE an integer number and returns it,
               advancing the POSITION to the first non-digit character
               following the consumed number."
              (let ((start-position position)
                    (end-position   position))
                (declare (type fixnum start-position))
                (declare (type fixnum end-position))
                (loop while (and character (digit-char-p character)) do
                  (incf end-position)
                  (advance))
                (the (integer 0 *)
                  (parse-integer code
                    :start start-position :end end-position)))))
          
          (loop do
            (cond
              ((null character)
                (loop-finish))
              
              ((char= character #\Space)
                (write-char character)
                (advance))
              
              ((char= character #\+)
                (incf variable-with-no-name)
                (when (> variable-with-no-name 52)
                  (error "Value too big."))
                (advance))
              
              ((char= character #\c)
                (advance)
                (cond
                  ((is-letter-variable character)
                    (setf (gethash character letter-variables)
                      (get-letter-for variable-with-no-name))
                    (advance))
                  (T
                    (setf (gethash #\a letter-variables)
                      (get-letter-for variable-with-no-name)))))
              
              ((char= character #\p)
                (advance)
                (let ((variable-name #\a))
                  (declare (type character variable-name))
                  (when (is-letter-variable character)
                    (setf variable-name character)
                    (advance))
                  (let ((variable-value
                          (gethash variable-name letter-variables)))
                    (declare (type (or null character) variable-value))
                    (when variable-value
                      (write-char variable-value)))))
              
              ((char= character #\r)
                (advance)
                (cond
                  ;; 'r' command followed by letter variable?
                  ((is-letter-variable character)
                    (let ((variable-name character))
                      (declare (type (or null character) variable-name))
                      (when variable-name
                        (setf variable-with-no-name
                          (gethash variable-name letter-variables 0))))
                    (advance))
                  ;; 'r2' command?
                  ((char= character #\2)
                    (setf variable-with-no-name 0)
                    (advance))
                  ;; 'r' command without letter variable or "2"?
                  ;; => Assume implicit letter variable "a".
                  (T
                    (setf variable-with-no-name 1))))
              
              ((char= character #\[)
                (advance)
                (let ((repetitions (read-number)))
                  (declare (type (integer 0 *) repetitions))
                  (if (plusp repetitions)
                    (push (make-aewnn-loop repetitions position) loops)
                    (loop with nesting of-type fixnum = 0 do
                      (case character
                        ((NIL)
                          (error "Unterminated opening bracket."))
                        (#\[
                          (incf nesting)
                          (advance))
                        (#\]
                          (cond
                            ((zerop nesting)
                              (advance)
                              (loop-finish))
                            (T
                              (decf nesting)
                              (advance))))
                        (otherwise
                          (advance)))))))
              
              ((char= character #\])
                (advance)
                (let ((active-loop (first loops)))
                  (declare (type (or null AEWNN-Loop) active-loop))
                  (when active-loop
                    (when (plusp (aewnn-loop-repetitions active-loop))
                      (decf (aewnn-loop-repetitions active-loop)))
                    (cond
                      ((plusp (aewnn-loop-repetitions active-loop))
                        (setf position  (aewnn-loop-body-start active-loop))
                        (setf character (char code position)))
                      (T
                        (pop loops))))))
              
              (T
                (error "Invalid character ~s at position ~d."
                  character position))))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "DHL".
(interpret-AEWNN "[3++++cpa]")

;;; -------------------------------------------------------

;; Print "ddd".
(interpret-AEWNN "[3++++cpar2]")

;;; -------------------------------------------------------

;; Print "ddd".
(interpret-AEWNN "[30+][3cpa]")

;;; -------------------------------------------------------

;; Print "HELLO WORLD".
(interpret-AEWNN "[8+]cpar2[5+]cpa[7+]cpacpa+++cpa [8+]cpar2[15+]cpa+++cpar2[12+]cpar2++++cpa")

;;; -------------------------------------------------------

;; Demonstrates a zero-repetitions loop.
(interpret-AEWNN "+[0+++]cp")

;;; -------------------------------------------------------

;; Demonstrates an empty loop.
(interpret-AEWNN "+[5]cp")

;;; -------------------------------------------------------

;; Print all available letters in their lexicographical order.
(interpret-AEWNN "[52+cp]")
