;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Calculator fuck", presented by the Esolang user
;; "Esowiki201529A" in the year 2015, founded upon the manipulation and
;; printing of two integer-valued variables.
;; 
;; 
;; Concept
;; =======
;; Calculator fuck's operational foundation subscribes to the
;; manipulation of two variables, commonly agnominated "x" and "y", by
;; adminiculum of several instructions, each manifesting in exactly two
;; characters' compound.
;; 
;; == PROGRAMS OPERATE ON TWO VARIABLES ==
;; The program memory responds to a twain of variables, usually assigned
;; the designations "x" and "y", whose distinguishment does not
;; perpetuate further than into its onomastic diorism. Each of these
;; storage units maintains a scalar integer of unbounded magnitude and
;; any sign.
;; 
;; A large quantity of instructions exist to manipulate these two
;; participants in a segregated manner, as well such that require their
;; coefficiency.
;; 
;; == INSTRUCTIONS: COMPOSITIONS OF TWO CHARACTERS ==
;; Twenty-seven (27) members exhaust the operational facilities,
;; capacitating programs to apply basic arithmetics as well as output
;; operations on either one or both variables at an instant.
;; 
;; The circumference, however, is curtailed of iteration constructs,
;; conditionals, and input reception.
;; 
;; == FAULT-TOLERANCE DEFINES PROGRAMS ==
;; The Calculator fuck programming language ostends a veridically
;; lenient digestion of its source by processing character pairs
;; representative of instructions, while ignoring unresolvable content.
;; 
;; Starting at the first code position, any two subsequent characters
;; are ligated and inquired into their eligibility as a command token.
;; A correspondence to such an operative matter eventuates its
;; execution. Disrespondency resorts to ineffectuality, in this
;; deportment constituting an indicator of descants. An incomplete
;; compound, conceivable only in the case of source code whose length
;; participates in an odd character count, conforms with the tantamount
;; of an non-command token.
;; 
;; In a more explicit diction, the following pseudocode holds:
;; 
;;   procedure interpretCalculatorFuck(code)
;;     Input:
;;       code : string, with indices >= 1
;;     
;;     Body:
;;       let tokenIndex <- 1
;;       while tokenIndex <= length(code)
;;         { The token will entail only one character if the code is }
;;         { odd in its length.                                      }
;;         let token <- code[i, i + 1]
;;         { Non-command tokens will simply be ignored. }
;;         if token is a command then
;;           execute command associated with the token
;;         end if
;;         { Skip the two characters just processed. }
;;         tokenIndex <- tokenIndex + 2
;;       end while
;;   end procedure
;; 
;; == INITIALIZATION REPLACES INPUTS ==
;; The absence of an input conduit imposes upon the interpreter the onus
;; of the two variables' initialization prior to a program's execution.
;; 
;; This requisitum's concrete manifestation eludes a further
;; specification. It constitutes, however, a sensible choice to
;; homologate adit to these adventives by mediation of the user's
;; inquisition, or the utility of a siclike obtention aliunde.
;; 
;; In any case, failure to supply the custom piece of commencement
;; always concludes in the assumption of the default value of zero (0)
;; for both placeholders.
;; 
;; 
;; Architecture
;; ============
;; Calculator fuck abstains from imposing a complex architecture,
;; instead furnishing two variables, "x" and "y", each a salvatory to a
;; single signed integer, and, if no other provision has been met,
;; defaulting to the zero (0) value.
;; 
;; 
;; Data Types
;; ==========
;; Its deployment in the variables' matter apportions the signed integer
;; type a paravaunt rank among the two available object sets; with the
;; characters being a puisne companion.
;; 
;; == INTEGERS ==
;; The excellence vouchsafed to the signed integer's echolon, its woning
;; registering the range [-infinity, +infinity], manifests in its
;; tenancy inside of the two variables, in a mete that equipensates
;; the magnitude of arithmetic operations accommodated to them.
;; 
;; == CHARACTERS ==
;; The character type's contribution does not extend beyond the purpose
;; of display --- the provenance of its subordination. This set's
;; exclusive entelechy transpires during the output operation twain
;; "*p" and "p*".
;; 
;; 
;; Syntax
;; ======
;; A Calculator fuck program consists of a sequence of zero or more
;; characters, each two consecutive items of which are ligated and
;; indagated for their conjoined eligibility as a command identifier. A
;; confirmation transpires the corresponding effect's execution; while
;; in the case of a mismatch no effect percolates.
;; 
;; == INSTRUCTIONS ==
;; Instructions are defined by particular compounds of two characters'
;; tally, commanding no mandatory sepiments for their delineation, as
;; the interpretation procedure automatically proceeds via twain
;; buildings.
;; 
;; == WHITESPACES ==
;; No particular treatment accounts its adhibition to whitespaces, which
;; encompasses spaces, horizontal tabs, and newlines. Such elements'
;; occasion resolves to any non-command token's appreciation as a
;; commentary intermission.
;; 
;; == COMMENTS ==
;; Any content not engaged in the affiliation with a command is
;; construed as a descant.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) formulation shall be
;; a steadable source for an additional summary of the language:
;; 
;;   program         := { comment , instruction } ;
;;   comment         := { character } ;
;;   instruction     := incrementX
;;                   |  decrementX
;;                   |  incrementY
;;                   |  decrementY
;;                   |  swap
;;                   |  addYToX
;;                   |  addXToY
;;                   |  subtractXYIntoX
;;                   |  subtractXYIntoY
;;                   |  subtractYXIntoX
;;                   |  subtractYXIntoY
;;                   |  negateX
;;                   |  negateY
;;                   |  doubleX
;;                   |  doubleY
;;                   |  multiplyXByY
;;                   |  multiplyYbyX
;;                   |  halveX
;;                   |  halveY
;;                   |  divideXYIntoX
;;                   |  divideXYIntoY
;;                   |  divideYXIntoX
;;                   |  divideYXIntoY
;;                   |  printX
;;                   |  printY
;;                   |  setXToZero
;;                   |  setYToZero
;;                   ;
;;   incrementX      := "*+" ;
;;   decrementX      := "*-" ;
;;   incrementY      := "+*" ;
;;   decrementY      := "-*" ;
;;   swap            := "**" ;
;;   addYtoX         := "$+" ;
;;   addXtoY         := "+$" ;
;;   subtractXYIntoX := "$-" ;
;;   subtractXYIntoY := "-$" ;
;;   subtractYXIntoX := "@-" ;
;;   subtractYXIntoY := "-@" ;
;;   negateX         := "!*" ;
;;   negateY         := "*!" ;
;;   doubleX         := "*2" ;
;;   doubleY         := "2*" ;
;;   multiplyXByY    := "*m" ;
;;   multiplyYbyX    := "m*" ;
;;   halveX          := "*g" ;
;;   halveY          := "g*" ;
;;   divideXYIntoX   := "*d" ;
;;   divideXYIntoY   := "d*" ;
;;   divideYXIntoX   := "*f" ;
;;   divideYXIntoY   := "f*" ;
;;   printX          := "*p" ;
;;   printY          := "p*" ;
;;   setXToZero      := "*0" ;
;;   setYToZero      := "0*" ;
;; 
;; 
;; Instructions
;; ============
;; Instructions in the language appear in the form of two-character
;; compounds, exhausted by arithmetics, logistics, and output
;; capabilities.
;; 
;; == PARTICIPANTS: ARITHMETICS, LOGISTICS, AND OUTPUT CONDUITS ==
;; Calculator fuck's instruction set is composed of 27 members, each
;; represented by a twain of characters, operating either exclusively on
;; the "x" or "y" variable, or requiring both's participation.
;; 
;; The operational roster can be applied to a treble distinguishment
;; into:
;; 
;;   (a) Arithmetics
;;       This bailiwick embraces incrementing, addition, subtraction,
;;       multiplication, and division; supplemented by a setting of
;;       the variables to the value zero (0).
;;   (b) Logistics
;;       The only member in this tier accounts for the variable value
;;       exchange operation.
;;   (c) Output
;;       The variables may be printed in the form of characters when
;;       construed in their roles as character codes.
;; 
;; == ABSENTEES: CONDITIONALS AND INPUT CONDUITS ==
;; Besides the input conduit's lacuna --- a conspicable lack of
;; antilibration --- most conspicuously, Calculator fuck's destitution
;; regarding conditional or iterative facilities excludes its admission
;; to more sophisticated tasks.
;; 
;; == OVERVIEW ==
;; The following apercu shall administer a cursory mete of gnarity
;; anenst the language's faculties:
;; 
;;   ------------------------------------------------------------------
;;   Command | Schema    | Effect
;;   --------+-----------+---------------------------------------------
;;   *+      | x = x + 1 | Increments the value of the variable x by
;;           |           | one.
;;   ..................................................................
;;   +*      | y = y + 1 | Increments the value of the variable y by
;;           |           | one.
;;   ------------------------------------------------------------------
;;   *-      | x = x - 1 | Decrements the value of the variable x by
;;           |           | one.
;;   ..................................................................
;;   -*      | y = y - 1 | Decrements the value of the variable y by
;;           |           | one.
;;   ------------------------------------------------------------------
;;   **      | x <--> y  | Exchanges the values of the variables x and
;;           |           | y.
;;   ------------------------------------------------------------------
;;   $+      | x = x + y | Sets the value of the variable x to the sum
;;           |           | of x and the value of the variable y.
;;   ..................................................................
;;   +$      | y = x + y | Sets the value of the variable y to the sum
;;           |           | of x and the value of the variable y.
;;   ------------------------------------------------------------------
;;   $-      | x = x - y | Subtracts the value of the variable y from
;;           |           | the variable x.
;;   ..................................................................
;;   -$      | y = x - y | Sets the value of the variable y to the
;;           |           | difference of the variable x decremented by
;;           |           | y.
;;   ------------------------------------------------------------------
;;   @-      | x = y - x | Sets the value of the variable x to the
;;           |           | difference of the variable y decremented by
;;           |           | x.
;;   ..................................................................
;;   -@      | y = y - x | Subtracts the value of the variable x from
;;           |           | the variable y.
;;   ------------------------------------------------------------------
;;   !*      | x = -x    | Negates the value of the variable x, that
;;           |           | is, multiplies it by -1.
;;   ..................................................................
;;   *!      | y = -y    | Negates the value of the variable y, that
;;           |           | is, multiplies it by -1.
;;   ------------------------------------------------------------------
;;   *2      | x = x * 2 | Doubles the value of the variable x.
;;   ..................................................................
;;   2*      | y = y * 2 | Doubles the value of the variable y.
;;   ------------------------------------------------------------------
;;   *m      | x = x * y | Multiplies the value of the variable x by
;;           |           | that of the variable y.
;;   ..................................................................
;;   m*      | y = y * x | Multiplies the value of the variable y by
;;           |           | that of the variable x.
;;   ------------------------------------------------------------------
;;   *g      | x = x / 2 | Halves the value of the variable x, rounding
;;           |           | the result down if necessary, that is:
;;           |           |   x = floor(x / 2)
;;   ..................................................................
;;   g*      | y = y / 2 | Halves the value of the variable y, rounding
;;           |           | the result down if necessary, that is:
;;           |           |   y = floor(y / 2)
;;   ------------------------------------------------------------------
;;   *d      | x = x / y | Sets the value of the variable x to the
;;           |           | quotient of x divided by the variable y,
;;           |           | rounded down if necessary, that is:
;;           |           |   x = floor(x / y)
;;   ..................................................................
;;   d*      | y = x / y | Sets the value of the variable y to the
;;           |           | quotient of the variable x divided by y,
;;           |           | rounded down if necessary, that is:
;;           |           |   y = floor(x / y)
;;   ------------------------------------------------------------------
;;   *f      | x = y / x | Sets the value of the variable x to the
;;           |           | quotient of the variable y divided by x,
;;           |           | rounded down if necessary, that is:
;;           |           |   x = floor(y / x)
;;   ..................................................................
;;   f*      | y = y / x | Sets the value of the variable y to the
;;           |           | quotient of y divided by the variable x,
;;           |           | rounded down if necessary, that is:
;;           |           |   y = floor(y / x)
;;   ------------------------------------------------------------------
;;   *p      | print(x)  | Outputs the character associated with the
;;           |           | value of the variable x when interpreted as
;;           |           | the character code.
;;   ..................................................................
;;   p*      | print(y)  | Outputs the character associated with the
;;           |           | value of the variable y when interpreted as
;;           |           | the character code.
;;   ------------------------------------------------------------------
;;   *0      | x = 0     | Sets the value of the variable x to zero.
;;   ..................................................................
;;   0*      | y = 0     | Sets the value of the variable y to zero.
;;   ------------------------------------------------------------------
;; 
;; A more variable-centric perspective shall be occupied in the table
;; below, whose effects are counterposed to the respective variants for
;; the two variables:
;; 
;;   ------------------------------------------------------------------
;;                                                           | Variable
;;   Effect                                                  |---------
;;                                                           | X  |  Y
;;   --------------------------------------------------------+----+----
;;   Increment by one the value of                           | *+ | +*
;;   ..................................................................
;;   Decrement by one the value of                           | *- | -*
;;   ..................................................................
;;   Swap value with the other variable                      | ** | **
;;   ..................................................................
;;   Set to sum of x + y the value of                        | $+ | +$
;;   ..................................................................
;;   Set to difference of x - y the value of                 | $- | -$
;;   ..................................................................
;;   Set to difference of y - x the value of                 | @- | -@
;;   ..................................................................
;;   Negate the value of                                     | !* | *!
;;   ..................................................................
;;   Double the value of                                     | *2 | 2*
;;   ..................................................................
;;   Multiply by value of the other variable                 | *m | m*
;;   ..................................................................
;;   Halve and round down the value of                       | *g | g*
;;   ..................................................................
;;   Set to rounded down quotient of x / y the value of      | *d | d*
;;   ..................................................................
;;   Set to rounded down quotient of y / x the value of      | *f | f*
;;   ..................................................................
;;   Output the character associated with the value of       | *p | p*
;;   ..................................................................
;;   Set to zero the value of                                | *0 | 0*
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-28
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Calculator_fuck"
;;   -> "https://esolangs.org/wiki/Calculator_fuck/HTML_Code"
;;       o The language creator's original implementation in HTML and
;;         JavaScript.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE, associated
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
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

(deftype command ()
  "The ``command'' type enumerates the recognized Calculator fuck
   instruction types."
  '(member
    :increment-x-by-one         ;; *+
    :increment-y-by-one         ;; +*
    :decrement-x-by-one         ;; *-
    :decrement-y-by-one         ;; -*
    :swap-values                ;; **
    :set-x-to-x-plus-y           ;; $+
    :set-y-to-x-plus-y           ;; +$
    :set-x-to-x-minus-y         ;; $-
    :set-y-to-x-minus-y         ;; -$
    :set-x-to-y-minus-x         ;; @-
    :set-y-to-y-minus-x         ;; -@
    :negate-x                   ;; !*
    :negate-y                   ;; *!
    :double-x                   ;; *2
    :double-y                   ;; 2*
    :set-x-to-x-times-y         ;; *m
    :set-y-to-x-times-y         ;; m*
    :halve-x                    ;; *g
    :halve-y                    ;; g*
    :set-x-to-x-divided-by-y    ;; *d
    :set-y-to-x-divided-by-y    ;; d*
    :set-x-to-y-divided-by-x    ;; *f
    :set-y-to-y-divided-by-x    ;; f*
    :print-x                    ;; *p
    :print-y                    ;; p*
    :set-x-to-zero              ;; *0
    :set-y-to-zero))            ;; 0*

;;; -------------------------------------------------------

(deftype command-identifier ()
  "The ``command-identifier'' type defines a textual identifier token as
   a potential key to a ``command'' detection, manifesting as a simple
   string of two characters' length."
  '(simple-string 2))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' defines a mapping of command identifier
   tokens to acutal commands, represented by a hash table which
   accommodates the former as ``command-identifier'' keys, affiliated
   with ``command'' objects as the values."
  '(hash-table-of command-identifier command))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of identifier operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized command identifiers with the respective
   ``command'' objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (identifier command)
        "Associates the IDENTIFIER string with the COMMAND in the
         +IDENTIFIERS+ table, potentially superseding an extant entry
         with the same IDENTIFIER, and returns no value."
        (declare (type command-identifier identifier))
        (declare (type command            command))
        (setf (gethash identifier +IDENTIFIERS+) command)
        (values)))
  (register-identifier "*+" :increment-x-by-one)
  (register-identifier "+*" :increment-y-by-one)
  (register-identifier "*-" :decrement-x-by-one)
  (register-identifier "-*" :decrement-y-by-one)
  (register-identifier "**" :swap-values)
  (register-identifier "$+" :set-x-to-x-plus-y)
  (register-identifier "+$" :set-y-to-x-plus-y)
  (register-identifier "$-" :set-x-to-x-minus-y)
  (register-identifier "-$" :set-y-to-x-minus-y)
  (register-identifier "@-" :set-x-to-y-minus-x)
  (register-identifier "-@" :set-y-to-y-minus-x)
  (register-identifier "!*" :negate-x)
  (register-identifier "*!" :negate-y)
  (register-identifier "*2" :double-x)
  (register-identifier "2*" :double-y)
  (register-identifier "*m" :set-x-to-x-times-y)
  (register-identifier "m*" :set-y-to-x-times-y)
  (register-identifier "*g" :halve-x)
  (register-identifier "g*" :halve-y)
  (register-identifier "*d" :set-x-to-x-divided-by-y)
  (register-identifier "d*" :set-y-to-x-divided-by-y)
  (register-identifier "*f" :set-x-to-y-divided-by-x)
  (register-identifier "f*" :set-y-to-y-divided-by-x)
  (register-identifier "*p" :print-x)
  (register-identifier "p*" :print-y)
  (register-identifier "*0" :set-x-to-zero)
  (register-identifier "0*" :set-y-to-zero)
  (values))

;;; -------------------------------------------------------

(defun command-identifier-p (token)
  "Checks whether the TOKEN represents a valid command name, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string token))
  (the boolean
    (not (null
      (nth-value 1
        (gethash token +IDENTIFIERS+))))))

;;; -------------------------------------------------------

(defun get-command (token)
  "Returns the command associated with the TOKEN, or signals an error
   upon the absence of a correspondence."
  (declare (type string token))
  (the command
    (or (gethash token +IDENTIFIERS+)
        (error "Invalid identifier for a command: ~s." token))))

;;; -------------------------------------------------------

(defun get-optional-command (token)
  "Returns the command associated with the TOKEN, or the ``NIL'' value
   upon the absence of a correspondence."
  (declare (type string token))
  (the (or null command)
    (when (command-identifier-p token)
      (get-command token))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Calculator-fuck (code
                                  &key (initial-x 0)
                                       (initial-y 0))
  "Interprets the piece of Calculator fuck source CODE, endowing the
   variables x and y with the INITIAL-X and INITIAL-Y defaults, and
   returns no value."
  (declare (type string  code))
  (declare (type integer initial-x))
  (declare (type integer initial-y))
  (when (plusp (length code))
    (let ((x initial-x)         ;; The variable "x".
          (y initial-y))        ;; The variable "y".
      (declare (type integer x))
      (declare (type integer y))
      (labels
          ((slice (start)
            "Returns the substring of the CODE comprehending the two
             characters beginning from the START position.
             ---
             If the CODE length does not permit two characters'
             obtention, the yielded string will entails a single
             constituent."
            (declare (type fixnum start))
            (the string
              (subseq code start
                (min (+ start 2)
                     (length code)))))
           
           (extract-command (start)
            "Extracts the maximum of two characters from the CODE
             commences at the START position and returns either command
             associated with this token or, if no correspondence could
             be detected, the ``NIL'' value."
            (declare (type fixnum start))
            (the (or null command)
              (get-optional-command
                (slice start)))))
        
        (loop
          for start of-type fixnum from 0 below (length code) by 2
          do
            (case (extract-command start)
              ((NIL)
                NIL)
              
              (:increment-x-by-one
                (incf x 1))
              
              (:increment-y-by-one
                (incf y 1))
              
              (:decrement-x-by-one
                (decf x 1))
              
              (:decrement-y-by-one
                (decf y 1))
              
              (:swap-values
                (rotatef x y))
              
              (:set-x-to-x-plus-y
                (setf x (+ x y)))
              
              (:set-y-to-x-plus-y
                (setf y (+ x y)))
              
              (:set-x-to-x-minus-y
                (setf x (- x y)))
              
              (:set-y-to-x-minus-y
                (setf y (- x y)))
              
              (:set-x-to-y-minus-x
                (setf x (- y x)))
              
              (:set-y-to-y-minus-x
                (setf y (- y x)))
              
              (:negate-x
                (setf x (- x)))
              
              (:negate-y
                (setf y (- y)))
              
              (:double-x
                (setf x (* x 2)))
              
              (:double-y
                (setf y (* y 2)))
              
              (:set-x-to-x-times-y
                (setf x (* x y)))
              
              (:set-y-to-x-times-y
                (setf y (* x y)))
              
              (:halve-x
                (setf x (floor x 2)))
              
              (:halve-y
                (setf y (floor y 2)))
              
              (:set-x-to-x-divided-by-y
                (setf x (floor x y)))
              
              (:set-y-to-x-divided-by-y
                (setf y (floor x y)))
              
              (:set-x-to-y-divided-by-x
                (setf x (floor y x)))
              
              (:set-y-to-y-divided-by-x
                (setf y (floor y x)))
              
              (:print-x
                (write-char (code-char x)))
              
              (:print-y
                (write-char (code-char y)))
              
              (:set-x-to-zero
                (setf x 0))
              
              (:set-y-to-zero
                (setf y 0))
              
              (otherwise
                NIL))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, world!".
(interpret-Calculator-fuck
  "*++*$++$$++$$++$$++$$+g*g*$-g*$-*+*+*p$+2*2*$+*-*pg*g*$+*+*p*pg*$+*p*g2*2*$-*+*p$-*p*2*2$-g*g*$+*p+*2*$-*pg*$+*-*p2*$-*+*+*p$-*pg*-**d*p*d*-*p")

;;; -------------------------------------------------------

;; Print the character associated with character code 66 (= "B"),
;; halve and round down its value to 33 (= "!"), and subsequently output
;; it.
(interpret-Calculator-fuck
  "*p  *g  *p"
  :initial-x 66)
