;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "XSVL", invented by the Esolang user "Threesodas" and
;; presented on August 26th, 2021, its hyle's concoction that of a
;; language inspired by Urban Mueller's brainfuck, while operating on
;; ten unsigned integer-valued cells only by sixteen operations'
;; adminicula, whose parcery of capabilities enumerates basic
;; arithmetics and printing devices.
;; 
;; 
;; Concept
;; =======
;; The XSVL programming language's edification is based upon the
;; manipulation of a tape of decimal componency whose membership
;; enhalses signed integer numbers, among whom the active unit's
;; selection is a dever allotted to a dedicated cell pointer, this
;; operative competence's expression emerging in the guise of
;; one-symbol instruction identifiers, akin to the brainfuck
;; programming language.
;; 
;; == XSVL: E[X]ECUTABLE [S]YMBOLIC [V]ALUE [L]ANG ==
;; Its agnomination a concomitant intimation of the potential and
;; syntactical construction, the abbreviation "XSVL" dispands to the
;; actual stevening of an "eXecutable Symbolic Value Lang".
;; 
;; 
;; Data Types
;; ==========
;; XSVL's data classification bifurcates into the paravaunt realm of
;; signed integer numbers, the wike apportioned to thilk all arithmetics
;; as well as the output facility's one moiety, and the paravail
;; character species, to whom no bailiwick aboon the currency of the
;; second output mode accompts for an partage.
;; 
;; 
;; Architecture
;; ============
;; A rather constrained ilk of allowance's recipient, XSVL's memory
;; department wists of a tape with decimal componency only, its
;; constituents' reification entertaining the diorism of integer numbers
;; disencumbered from any stipulations concerning their sign and
;; mickleness.
;; 
;; Operating upon this starkly delineated dispansion, a cell pointer's
;; latreutical involvement conditions its selection of the currently
;; active cell, this imposing that at the respective instant exclusively
;; amenable unit to perquisitions and modulations. Its motile nature
;; homologates the cursor's stillatim progression along both of the
;; tape's axes, its bournes, however, defined on their terminal points,
;; beyond thilk no advancement may be actuated.
;; 
;; 
;; Instructions
;; ============
;; XSVL's instruction set enumerates a cardinality of 16 members, the
;; resulting competences' perimeter such to entalent the language with
;; the warklumes for basic arithmetics and numeric as well as
;; character-based output behests.
;; 
;; == OVERVIEW ==
;; The following apercu's wike shall be realized in a basic mete of
;; gnarity's conveyance with respect to the language's operative dation:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one (1).
;;   ..................................................................
;;   -       | Decrements the current cell value by one (1).
;;   ..................................................................
;;   >       | If the cell pointer does not wone in the rightmost cell,
;;           | translates this pointer one step to the right; otherwise
;;           | accompasses no causatum.
;;   ..................................................................
;;   <       | If the cell pointer does not wone in the leftmost cell,
;;           | translates this pointer one step to the left; otherwise
;;           | accompasses no causatum.
;;   ..................................................................
;;   .       | Prints the character whose ASCII code concurs with the
;;           | current cell value to the standard output conduit.
;;           |---------------------------------------------------------
;;           | If the cell value does not correspond to a valid
;;           | character code, the consequent deportment remains
;;           | undefined.
;;   ..................................................................
;;   :       | Prints the current cell value in its verbatim numeric
;;           | form to the standard output conduit.
;;   ..................................................................
;;   0       | Multiplies the current cell value by the number zero (0)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   1       | Multiplies the current cell value by the number one (1)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   2       | Multiplies the current cell value by the number two (2)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   3       | Multiplies the current cell value by the number three
;;           | (3) and stores the supputated product in the current
;;           | cell.
;;   ..................................................................
;;   4       | Multiplies the current cell value by the number four (4)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   5       | Multiplies the current cell value by the number five (5)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   6       | Multiplies the current cell value by the number six (6)
;;           | and stores the supputated product in the current cell.
;;   ..................................................................
;;   7       | Multiplies the current cell value by the number seven
;;           | (7) and stores the supputated product in the current
;;           | cell.
;;   ..................................................................
;;   8       | Multiplies the current cell value by the number eight
;;           | (8) and stores the supputated product in the current
;;           | cell.
;;   ..................................................................
;;   9       | Multiplies the current cell value by the number nine (9)
;;           | and stores the supputated product in the current cell.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the XSVL programming language's simplistic nature, a few
;; interferences from ambivalent construction may be registered; the
;; most peisant members shall be desumed in the subsequent sections.
;; 
;; == ARE CELL VALUES RESTRICTED TO THE BYTE RANGE? ==
;; Its status of reference to the brainfuck programming language as an
;; entheus, in some not clearly delineated grade, and most conspicuously
;; reverberating in the behest identifier tokens, segues into the
;; question's gendrure whether a tape cell's capacity shall be subject
;; to a restriction into a byte range, either signed or unsigned, or the
;; entire integer gamut.
;; 
;; It has been adjudged to adhere to the imputation of a cell's state
;; as a salvatory to integers of both signs and any mickleness.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter at hand's realization constitutes a nitency
;; engendered in the programming language Common Lisp, the efficacy's
;; adhibition the succedent of the induced XSVL code's immediate
;; processing.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-08-31
;; 
;; Sources:
;;   [esolang2024XSVL]
;;   The Esolang contributors, "XSVL", October 25th, 2024
;;   URL: "https://esolangs.org/wiki/XSVL"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tape ()
  "The ``tape'' type defines the XSVL memory tape as a one-dimensional
   simple array enumerating ten signed integer numbers the same wist of
   no bournes anent their mickleness."
  '(simple-array integer (10)))

;;; -------------------------------------------------------

(deftype cell-index ()
  "The ``cell-index'' type defines a valid index into the XSVL tape as
   an unsigned integer number occupying the closed interval of [0, 9],
   the cardinality of the thus specified set equinumerant to the tape's
   componency."
  '(integer 0 9))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-simple-base-string (source)
  "Creates a fresh simple base string representation of the general
   SOURCE string.
   ---
   The CODE itself will not be subjected to modulations."
  (declare (type string source))
  (the simple-base-string
    (coerce source 'simple-base-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 7) +XSVL-IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +XSVL-IDENTIFIERS+
  (convert-to-simple-base-string "+-><.:0")
  "Defines the recognized XSVL instruction symbols in a simple base
   string's ensconcement.")

;;; -------------------------------------------------------

(defun xsvl-identifier-p (candidate)
  "Determines whether the CANDIDATE represents one of the septuple XSVL
   instruction identifiers, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (find candidate +XSVL-IDENTIFIERS+ :test #'char=)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of XSVL code optimizer operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun optimize-XSVL-code (code)
  "Adhibits parasceuastic measures to the piece of XSVL source CODE by
   all non-operative symbols' ejection and a consequent conversion into
   a simple base string, thilk is finally returned.
   ---
   The CODE itself will not be subjected to modulations."
  (declare (type string code))
  (the simple-base-string
    (convert-to-simple-base-string
      (remove-if-not #'xsvl-identifier-p code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory tape operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tape ()
  "Creates and returns a fresh ``tape'' as a integer vector enumerating
   a decimal accompt in its componency."
  (the tape
    (make-array 10
      :element-type    'integer
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))

;;; -------------------------------------------------------

(defun cell-value-at (tape index)
  "Returns the value of the cell amenable to the zero-based INDEX into
   the TAPE."
  (declare (type tape       tape))
  (declare (type cell-index index))
  (the integer
    (aref tape index)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value tape index)
  "Stores the NEW-VALUE in the TAPE cell amenable to the zero-based
   INDEX and returns no value."
  (declare (type integer    new-value))
  (declare (type tape       tape))
  (declare (type cell-index index))
  (setf (aref tape index) new-value)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-modify-macro multiplyf (&rest factors)
  *
  "Destructively modifies the first member of the FACTORS, acting as a
   place, by assignment of all the FACTOR's product and returns the
   result.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-optimized-xsvl (code)
  "Interprets the piece of XSVL source CODE, expected in its
   conformation to align with a simple base string species, and returns
   no value."
  (declare (type simple-base-string code))
  
  (let ((ip           0)
        (tape         (make-tape))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type tape       tape))
    (declare (type cell-index cell-pointer))
    
    (loop while (< ip (length code)) do
      (case (schar code ip)
        (#\+
          (incf
            (cell-value-at tape cell-pointer)))
        
        (#\-
          (decf
            (cell-value-at tape cell-pointer)))
        
        (#\>
          (when (< cell-pointer 9)
            (incf cell-pointer)))
        
        (#\<
          (when (plusp cell-pointer)
            (decf cell-pointer)))
        
        (#\.
          (format *standard-output* "~c"
            (code-char
              (cell-value-at tape cell-pointer))))
        
        (#\:
          (format *standard-output* "~d"
            (cell-value-at tape cell-pointer)))
        
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (multiplyf (cell-value-at tape cell-pointer)
            (digit-char-p
              (schar code ip))))
        
        (otherwise
          NIL))
      
      (incf ip)))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-xsvl (code)
  "Interprets the piece of XSVL source CODE and returns no value."
  (declare (type string code))
  (interpret-optimized-xsvl
    (convert-to-simple-base-string code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello, World!" to the standard output conduit.
(interpret-xsvl
  "+89.0+554+.0+934..+++.>+67++.0+84.0+52+8-.<.+++.0+934.0+554.0+84+.")

;;; -------------------------------------------------------

;; XKCD random number: Prints the value four (4).
(interpret-xsvl "+4:")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language "0", whose programs
;; in a Procrustean mode of respondency output the message "0".
;; 
;; Please heed the advenient installment of commentary content, present
;; in the circumambiency of the aefauld operative symbol ":".
(interpret-xsvl "Hello :D")
