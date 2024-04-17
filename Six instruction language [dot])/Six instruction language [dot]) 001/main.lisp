;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Six instruction language :)", invented by the Esolang user
;; "Xyzzy" and presented on July 1st, 2022, the kenspeckle haecceity of
;; the same wones in its six instructions, identified in an enumerative
;; manner by decimal digits, and employing among some of its specimens
;; a particular letter-encoding of integer arguments, with the telos of
;; three registers' manipulation.
;; 
;; 
;; Concept
;; =======
;; The Six instruction language :) is founded upon the manipulation of
;; a triad of registers, designated as "r1", "r2", and "r3", each siccan
;; component entalented with the capacity for an unsigned integer
;; literal desumed from the closed interval [0, 15].
;; 
;; == THREE REGISTERS GOVERN THE DATA COMPARTMENT ==
;; That bailiwick allotted to the data castaldy tallies a treble
;; componency of registers, nevened in official parlance by the
;; abbreviations "r1", "r2", and "r3".
;; 
;; Each such salvatory applies itself to a scalar integer's
;; administration, the gamut of which does not project beyond the
;; closed interval [0, 15].
;; 
;; == THE THIRD REGISTER (R3) CONTAINS THE PROGRAM OUTPUT ==
;; Concomitant to an input conduit's lacuna, the third register, r3,
;; at least is assigned a particular twifaced agency in its
;; incorporation of the program's output competence.
;; 
;; Ensuing from the program's termination, the state of this register r3
;; is printed in its numeric form, as an integral object in the range
;; [0, 15], to the standard output.
;; 
;; == NUMBERS ARE ENCODED IN LATIN MINUSCULES ==
;; A kenspeckle encoding is applied to the language's aefauld data type,
;; non-negative integral values in the range [0, 15], mapping to single
;; characters from the set of minuscular Latin letters.
;; 
;; A tabulation of this association from the encoding to the decoded
;; value shall be adduced:
;; 
;;   ---------------------
;;   Token | Numeric value
;;   ------+--------------
;;   a     | 0
;;   .....................
;;   b     | 1
;;   .....................
;;   c     | 2
;;   .....................
;;   q     | 3
;;   .....................
;;   w     | 4
;;   .....................
;;   e     | 5
;;   .....................
;;   d     | 6
;;   .....................
;;   f     | 7
;;   .....................
;;   g     | 8
;;   .....................
;;   r     | 9
;;   .....................
;;   t     | 10
;;   .....................
;;   y     | 11
;;   .....................
;;   h     | 12
;;   .....................
;;   i     | 13
;;   .....................
;;   j     | 14
;;   .....................
;;   u     | 15
;;   ---------------------
;; 
;; 
;; Syntax
;; ======
;; The Six instruction language :) programming language's syntaxis
;; imposes upon its programs a rather limited character repertoire,
;; reserving decimal digits from inclusive one (1) through six (6) for
;; the instruction identifiers, accompanied in two instances by a single
;; Latin minuscule as the operand, homologating in the interstices
;; merely whitespaces.
;; 
;; == GRAMMAR ==
;; The language's donet shall be administered a more formal diction by
;; adminiculum of an Exended Backus-Naur Form (EBNF) treatise:
;; 
;;   program     := { command | whitespaces } ;
;;   command     := ( "1" , whitespaces , operand )
;;               |  ( "2" , whitespaces , operand )
;;               |    "3"
;;               |    "4"
;;               |    "5"
;;               |    "6"
;;               ;
;;   operand     := "a" | "b" | "c" | "q" | "w" | "e" | "d" | "f"
;;               |  "g" | "r" | "t" | "y" | "h" | "i" | "j" | "u"
;;               ;
;;   whitespaces := { whitespace } ;
;;   whitespace  := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A sextuple cardinality's adhibition constitutes the language's
;; operative potentials, each among these expressed by a positive
;; integer digit, succeeded by zero or one argument encoded in the
;; minuscular form which please consult aboon in the section "Concept"
;; section's subordinate "NUMBERS ARE ENCODED IN LATIN MINUSCULES"
;; part.
;; 
;; == OVERVIEW ==
;; The following apercu's dever shall be the imposition of a cursory
;; mete of gnarity's conveyance anent the available instructions.
;; 
;; Please heed that succedaneous segments are designated by jumelles of
;; braces, "{...}", and intended for their supersession by actual
;; Six instruction language :) code in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   1{value} | Sets the value of the first register r1 to {value}.
;;            |--------------------------------------------------------
;;            | {value} must be a non-negative integer expressed in
;;            | the dioristic encoding.
;;   ..................................................................
;;   2{value} | Sets the value of the second register r2 to {value}.
;;            |--------------------------------------------------------
;;            | {value} must be a non-negative integer expressed in
;;            | the dioristic encoding.
;;   ..................................................................
;;   3        | Sets the value of the third register r3 to the sum of
;;            | the register value of r1 incremented by r2, that is:
;;            |   r3 <- r1 + r2.
;;   ..................................................................
;;   4        | Resets the value of the third register r3 to the
;;            | inchoate zero (0) state, that is:
;;            |   r3 <- 0.
;;   ..................................................................
;;   5        | Resets the values of the first and second register r1
;;            | and r2 to the inchoate zero (0) state, that is:
;;            |   r1 <- 0
;;            |   r2 <- 0
;;   ..................................................................
;;   6        | Inverts the value of the first register r1, that is:
;;            |   r1 <- 15 - r1.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-17
;; 
;; Sources:
;;   [esolang2023sixinstructionlanguage:)]
;;   The Esolang contributors, "Six instruction language :)",
;;     December 20th, 2023
;;   URL: "https://esolangs.org/wiki/Six_instruction_language_:)"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype uint16 ()
  "The ``uint16'' type defines an unsigned integer number composed of
   sixteen accolent bits, and by this diorism's virtue being a occupant
   of the closed integral interval [0, 15]."
  '(integer 0 15))

;;; -------------------------------------------------------

(deftype operator ()
  "The ``operator'' type enumerates the recognized instruction type
   variants."
  '(member
    :set-register-1
    :set-register-2
    :set-register-3
    :wipe-register-3
    :wipe-registers-1-and-2
    :invert-register-1))

;;; -------------------------------------------------------

(deftype operand ()
  "The ``operand'' type defines an instruction operand as an optional
   construct, either desumed from the ``uint16'' numeric range, or
   resolving to the ``NIL'' sentinel for its express omission."
  '(or null uint16))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type encapsulates a
   \"Six instruction language :)\" instruction as a composition of an
   operator and an optional operand, molded into a cons cell, the
   sinistral parcel of which desumes its values from the set of
   recognized ``operator''s, the dextral from from the ``operand''
   realm."
  '(cons operator operand))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, everychon among the membership complies with the
   ELEMENT-TYPE, its default the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable
   \"Six instruction language :)\" program as an ordered list of zero or
   more ``instruction''s."
  '(list-of instruction))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of number decoding table.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 16) +NUMBER-TABLE+))

;;; -------------------------------------------------------

(defparameter +NUMBER-TABLE+
  "abcqwedfgrtyhiju"
  "Associates the recognized instruction operands, or \"inputs\", in
   their encoded alphabetic form to the respective integer values by
   adminiculum of their zero-based positions inside of this decoding
   table.")

;;; -------------------------------------------------------

(defun decode-operand (identifier)
  "Returns the numeric value affiliated with the IDENTIFIER, or signals
   an error of an unspecified type upon its disresponency."
  (declare (type character identifier))
  (the uint16
    (or (position identifier +NUMBER-TABLE+ :test #'char=)
        (error "The token \"~c\" does not answer to a numeric value."
          identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean truth value as an equivalency to the OBJECT's
   notion as a \"generalized boolean\", returning ``T'' for a
   non-``NIL'' input, otherwise, if the OBJECT is tantamount to ``NIL'',
   responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   a diorism whose circumference inscribes spaces, horizontal tabs, as
   well as newline entities, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-next-token (source start)
  "Proceeding from the START position into the SOURCE, returns the
   location of the next token in the SOURCE, or, upon such a content's
   lacuna, return the length the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position into
   the SOURCE immediately succeeding the omitted segment."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-instruction (operator &optional (operand NIL))
  "Creates and returns a fresh ``instruction'', encapsulating in its
   conformation the OPERATOR and the optional OPERAND."
  (declare (type operator operator))
  (declare (type operand  operand))
  (the instruction
    (cons operator operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-operand (source start)
  "Proceeding from the START position into the SOURCE, parses an
   instruction operand and returns two values:
     (1) The numeric operand value an integer in the range [0, 15].
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the consumed operand."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values uint16 fixnum)
    (if (array-in-bounds-p source start)
      (values
        (decode-operand
          (char source start))
        (1+ start))
      (error "The index ~d described an invalid location in the ~
              source ~s."
        start source))))

;;; -------------------------------------------------------

(defun parse-instruction (source start)
  "Proceeding from the START position into the SOURCE, parses the next
   instruction and returns two values:
     (1) An ``instruction'' representation of the consumed instruction.
     (2) The position into the SOURCE immediately succeeding the
         detected instruction."
  (declare (type string source))
  (declare (type fixnum start))
  (the instruction
    (if (array-in-bounds-p source start)
      (let ((probed-character (char source start))
            (position         start))
        (declare (type character probed-character))
        (declare (type fixnum    position))
        (flet ((consume-instruction-identifier ()
                "Proceeding from the current POSITION into the SOURCE,
                 skips the instruction identifier and any subsequent
                 whitespaces, updates the POSITION to the respective new
                 location, and returns no value."
                (setf position
                  (skip-whitespaces source
                    (1+ position)))
                (values))
               
               (read-operand ()
                "Proceeding from the current POSITION into the SOURCE,
                 parses a numeric operand, updates the POSITION to the
                 location immediately succeeding the consumed operand,
                 and returns the retrieved operand."
                (let ((operand 0))
                  (declare (type uint16 operand))
                  (setf (values operand position)
                    (parse-operand source
                      (skip-whitespaces source position)))
                  (the uint16 operand))))
          (the (values instruction fixnum)
            (values
              (case probed-character
                (#\1
                  (consume-instruction-identifier)
                  (make-instruction :set-register-1
                    (read-operand)))
                
                (#\2
                  (consume-instruction-identifier)
                  (make-instruction :set-register-2
                    (read-operand)))
                
                (#\3
                  (consume-instruction-identifier)
                  (make-instruction :set-register-3))
                
                (#\4
                  (consume-instruction-identifier)
                  (make-instruction :wipe-register-3))
                
                (#\5
                  (consume-instruction-identifier)
                  (make-instruction :wipe-registers-1-and-2))
                
                (#\6
                  (consume-instruction-identifier)
                  (make-instruction :invert-register-1))
                
                (otherwise
                  (error "Invalid instruction identifier \"~c\" at ~
                          position ~d."
                    probed-character position)))
              position))))
      (error "The index ~d describes an invalid location in the ~
              source ~s."
        start source))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of \"Six instruction language :)\" SOURCE code and
   returns a list of its comprehended instruction."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((collect-instruction (instruction new-position)
            "Updates the POSITION cursor to the NEW-POSITION and returns
             the INSTRUCTION."
            (declare (type instruction instruction))
            (declare (type fixnum      new-position))
            (the instruction
              (prog1 instruction
                (setf position new-position)))))
      (the program
        (loop
          initially
            (setf position
              (skip-whitespaces source 0))
          while
            (< position (length source))
          collect
            (prog1
              (multiple-value-call #'collect-instruction
                (parse-instruction source position))
              (setf position
                (skip-whitespaces source position))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (program)))
  "The ``Interpreter'' class applies itself to the evaluation of a
   \"Six instruction language :)\" program in that pursuit to accompass
   actual effect to its instructions."
  (program   (error "Missing program.") :type program :read-only T)
  (register-1 0                         :type uint16  :read-only NIL)
  (register-2 0                         :type uint16  :read-only NIL)
  (register-3 0                         :type uint16  :read-only NIL))

;;; -------------------------------------------------------

(defun process-instruction (interpreter instruction)
  "Evaluates the INSTRUCTION in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type instruction instruction))
  (destructuring-bind (operator . operand) instruction
    (declare (type operator operator))
    (declare (type operand  operand))
    (declare (ignorable     operand))
    (case operator
      (:set-register-1
        (setf (interpreter-register-1 interpreter) operand))
      
      (:set-register-2
        (setf (interpreter-register-2 interpreter) operand))
      
      (:set-register-3
        (setf (interpreter-register-3 interpreter)
          (mod
            (+ (interpreter-register-1 interpreter)
               (interpreter-register-2 interpreter))
            16)))
      
      (:wipe-register-3
        (setf (interpreter-register-3 interpreter) 0))
      
      (:wipe-registers-1-and-2
        (psetf (interpreter-register-1 interpreter) 0
               (interpreter-register-2 interpreter) 0))
      
      (:invert-register-1
        (setf (interpreter-register-1 interpreter)
          (- 15 (interpreter-register-1 interpreter))))
      
      (otherwise
        (error "Unrecognized instruction: ~s." instruction))))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the program governed by the INTERPRETER's administration
   and returns no value."
  (declare (type Interpreter interpreter))
  (dolist (current-instruction (interpreter-program interpreter))
    (declare (type instruction current-instruction))
    (process-instruction interpreter current-instruction))
  (format T "~&~d"
    (interpreter-register-3 interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Six-instruction-language (code)
  "Interprets the piece of \"Six instruction language :)\" and returns
   no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (parse-program code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Calculate 1+1 and print the result: 2.
(interpret-Six-instruction-language "1b2b3")

;;; -------------------------------------------------------

;; Set register 1 to the value five (5), invert its state to ten (10),
;; and print the same.
(interpret-Six-instruction-language "1e63")
