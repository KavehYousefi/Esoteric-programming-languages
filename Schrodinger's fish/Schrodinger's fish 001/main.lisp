;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Schrodinger's fish", invented by the Esolang user
;; "BestCoder" and presented on July 19th, 2024, to whom the dioristic
;; proprium's vouchsafement is affixed of a twifold augmentation:
;; imprimis, that of Jonathan Todd Skinner's "Deadfish", and, itself a
;; scion's status, "Alivefish" as a conception of the Esolang user
;; "ResU"; whence renders the confluency's gendrure of the accumulator
;; to ostend a dispansion in its singular capacity from the traditional
;; bourneless signed integer type to complex numbers whose constituting
;; twissel's tolerance amplects the selfsame contingency.
;; 
;; 
;; Concept
;; =======
;; Schrodinger's fish bears the conformation of a derivative and
;; expansion ensuing from Deadfish's and Alivefish's champarty, the
;; perimeter of its yet singular memory accumulator administered the
;; auctive potential of either comprehending the traditional signed
;; integer number, to whom no natural imposition anenst the mickleness
;; exists, or a biscalar, or complex number, of a constituting that
;; allocates a twissel of these integral objects in a compound.
;; 
;; == THE MEMORY: AN INTEGER OR INTEGRAL COMPLEX NUMBER ==
;; An enhaused variation on its stock-father's memory model,
;; Schrodinger's fish invents a supererogation in the aefauld register,
;; or "accumulator", by its admission of both integers disencumbered
;; from any restrictive nomothesy concerning the polarity or the
;; magnitude, as well as the novel polymechany commorant in the
;; homologation of complex number, adhering to a componency that
;; assigns an integral real part to an imaginary datum desumed from the
;; same vale.
;; 
;; == PROGRAMS OPERATE IN A PERPETUAL ITERANCE ==
;; A verbatim appropriation from its Deadfish entheus, programs in this
;; language partake of the same perpetuation concept in their execution
;; model, querying in a first step for a line of input, prevenient to
;; this communication ostending the prompt message
;; 
;;   >> 
;; 
;; and evaluating each of the zero or more committed symbols in a
;; seriatim procession as potential instruction identifiers serelepes.
;; 
;; == UNRECOGNIZED TOKENS ISSUE A NEWLINE OUTPUT ==
;; A kenspeckle proprium of Deadfish, and many of its derivatives,
;; inwith whose kinship "Schrodinger's fish" tends engage a subsumption,
;; non-operative symbols exhibit an abstention from both the abortive
;; implications of an erroneous inroad and the athwart extremum molded
;; into a docimasy begetting adiaphoracy; instead, each such token's
;; involvement produces the epiphenomenal gendrure of a newline
;; character's issuance to the standard output conduit.
;; 
;; 
;; Instructions
;; ============
;; The Schrodinger's fish's programming language ostends in its
;; operative warklumes' circumference an octuple membership, into whose
;; capabilities are subsumed basic arithmetics, including the handling
;; of biscalars, as well as numeric input and output communication
;; conduits.
;; 
;; Any character eloigned from an epiphenomenal potential, in lieu of
;; an abortive error's infliction, simply issues an aefauld newline
;; character to the standard output conduit.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be fulfilled in a requisite mete
;; of nortelry's adhibition concerning the language's operative
;; contingency:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator value by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes an ipsissima verba
;;           | appropriation from Deadfish, albeit modulated in
;;           | response to the biscalar data type's participation.
;;   ..................................................................
;;   I       | If the accumulator contains a complex number number,
;;           | converts thilk to an integral object by setting its
;;           | state to the real part only; otherwise, for an already
;;           | integral accumulator, accompasses no causatum.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious introduction
;;           | forinsecal to the Deadfish cleronomy.
;;   ..................................................................
;;   d       | Decrements the accumulator value by one (1).
;;           |---------------------------------------------------------
;;           | This operation constitutes an ipsissima verba
;;           | appropriation from Deadfish, albeit modulated in
;;           | response to the biscalar data type's participation.
;;   ..................................................................
;;   D       | Doubles the accumulator value by multiplying its state
;;           | by two (2).
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious introduction
;;           | forinsecal to the Deadfish cleronomy.
;;   ..................................................................
;;   s       | Squares the accumulator value by multiplying its state
;;           | by itself.
;;           |---------------------------------------------------------
;;           | This operation constitutes an ipsissima verba
;;           | appropriation from Deadfish, albeit modulated in
;;           | response to the biscalar data type's participation.
;;   ..................................................................
;;   S       | Sets the accumulator to the square root of its own
;;           | state, contingently rounded down to the nearest integer
;;           | number.
;;           |---------------------------------------------------------
;;           | Inwith this operation's wones the potential for the
;;           | conversion of an integral datum to a complex number,
;;           | this constituting an epiphenomenon begotten from the
;;           | radical's supputation on a negative integral accumulator
;;           | state.
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious introduction
;;           | forinsecal to the Deadfish cleronomy.
;;   ..................................................................
;;   o       | Prints the accumulator value in a format covenable to
;;           | its concrete datum type to the standard output conduit,
;;           | succeeded by a single newline's issuance.
;;           |---------------------------------------------------------
;;           | In a concrete diction, it holds:
;;           | 
;;           |   if the accumulator contains a complex number then
;;           |     let realPart <- real      part of the accumulator
;;           |     let imagPart <- imaginary part of the accumulator
;;           |     
;;           |     if imagePart >= 0 then
;;           |       print: realPart, "+", imagPart, newline
;;           |     else
;;           |       print: realPart, "-", imagPart, newline
;;           |     end if
;;           |   else
;;           |     print: accumulator, newline
;;           |   end if
;;           |---------------------------------------------------------
;;           | This operation constitutes an ipsissima verba
;;           | appropriation from Deadfish, albeit modulated in
;;           | response to the biscalar data type's participation.
;;   ..................................................................
;;   O       | Queries the standard input conduit for a signed or
;;           | unsigned integer number, or a complex number, whose any
;;           | of moeties may be a signed or unsigned integral number.
;;           |---------------------------------------------------------
;;           | If the input is intended to communicate a complex
;;           | number, its format must comply to the following donat,
;;           | as molded into the Extended Backus-Naur Form (EBNF)
;;           | conventions:
;;           | 
;;           |   complexNumber   := integer , signedInteger , "i" ;
;;           |   integer         := unsignedInteger | signedInteger ;
;;           |   signedInteger   := [ "+" | "-" ] , unsignedInteger ;
;;           |   unsignedInteger := digit , { digit } ;
;;           |   digit           := "0" | "1" | ... | "9" ;
;;           |---------------------------------------------------------
;;           | This operation constitutes an adscititious introduction
;;           | forinsecal to the Deadfish cleronomy.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's realization constitutes a sequela from the
;; Common Lisp programming language's employment, its operative entelchy
;; the source code strings' immediate procession.
;; 
;; An warklume of peisant vallidom in the tendance to Schrodinger's
;; fish's capacitation for complex numbers in the accumulator limns the
;; dation of autochthonous support for biscalars' in Common Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-16
;; 
;; Sources:
;;   [esolang2025:Schrodinger's fish]
;;   The Esolang contributors, "Schrodinger's fish", July 6th, 2025
;;   URL: "https://esolangs.org/wiki/Schrodinger%27s_fish"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype integral-number ()
  "The ``integral-number'' type defines a numeric object amounting
   either to a scalar integer or a biscalar, or complex number, whose
   both moeities comply to sclar integers."
  '(or integer (complex integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Transcripts the OBJECT from its role as a generalized boolean into a
   veridicous Boolean signifier, returning for a non-``NIL'' OBJECT a
   ``boolean'' value of ``T''; otherwise, for a ``NIL'' input, responds
   with the ``NIL'' sentinel itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space character, the
   diorism assigned to thilk amplects the horizontal tab, the vertical
   tabulation, as well as the traditional space character, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate '(9 11 32) :key #'code-char :test #'char=))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE represents an arithmetic sign, that
   is, either the plus (\"+\") or minus (\"-\") symbol, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (or (char= candidate #\+)
          (char= candidate #\-)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the number parsing operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a catena
   enumerating zero or more attiguous spaces, and returns the position
   into the SOURCE immediately succeeding the skipped tmema."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun sign-character-is-located-at-p (source position)
  "Determines whether the character at the POSITION into the SOURCE
   represents an arithmetic sign, that is, either a plus (\"+\") or
   minus (\"-\") symbol, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (convert-into-a-boolean-value
      (sign-character-p
        (char source position)))))

;;; -------------------------------------------------------

(defun locate-end-of-the-integer (source start)
  "Proceeding from the START position into the SOURCE, and expecting the
   same to either designate an arithmetic sign or a decimal digit,
   locates and returns the index immediately succeeding the ensconced
   integer number."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or
      (position-if-not #'digit-char-p source :start
        (if (sign-character-is-located-at-p source start)
          (1+ start)
          start))
      (length source))))

;;; -------------------------------------------------------

(defun extract-a-signed-or-unsigned-integer (source start)
  "Proceeding from the START position into the SOURCE, extracts a signed
   or unsigned integer number and returns two values:
     (1) The extracted number as an integral object.
     (2) The position into the SOURCE immediately succeeding the tmema
         occupied by the extracted integer number."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values integer fixnum)
    (parse-integer source
      :start start
      :end   (locate-end-of-the-integer source start))))

;;; -------------------------------------------------------

(defun extract-a-signed-integer (source start)
  "Proceeding from the START position into the SOURCE, extracts a
   mandatorily signed integer number and returns two values:
     (1) The extracted number as an integral object.
     (2) The position into the SOURCE immediately succeeding the tmema
         occupied by the extracted integer number."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values integer fixnum)
    (if (sign-character-is-located-at-p source start)
      (parse-integer source
        :start start
        :end   (locate-end-of-the-integer source start))
      (error "Not a signed integer number."))))

;;; -------------------------------------------------------

(defun parse-an-integer-or-complex-number (source)
  "Parses the SOURCE as either a signed or unsigned integer number or a
   biscalar, returning the most connable representation thereof."
  (declare (type string source))
  (let ((real-part        0)
        (imaginary-part   0)
        (current-position 0))
    (declare (type integer real-part))
    (declare (type integer imaginary-part))
    (declare (type fixnum  current-position))
    (symbol-macrolet
        ((current-token
          (the character
            (char source current-position)))
         (source-is-exhausted-p
          (the boolean
            (not (array-in-bounds-p source current-position)))))
      (declare (type character current-token))
      (declare (type boolean   source-is-exhausted-p))
      
      (multiple-value-setq (real-part current-position)
        (extract-a-signed-or-unsigned-integer source
          (skip-spaces source current-position)))
      
      (setf current-position
        (skip-spaces source current-position))
      
      (when (and (not source-is-exhausted-p)
                 (sign-character-p current-token))
        (multiple-value-setq (imaginary-part current-position)
          (extract-a-signed-integer source
            (skip-spaces source current-position)))
        
        (setf current-position
          (skip-spaces source current-position))
        
        ;; Expect the suffix "i".
        (if (and (not source-is-exhausted-p)
                 (char= current-token #\i))
          (setf current-position
            (skip-spaces source
              (1+ current-position)))
          (error "Expected the complex number to terminate in the ~
                  suffix \"i\".")))
      
      (unless source-is-exhausted-p
        (error "Unexpected content at position ~d of ~s."
          current-position source)))
    
    (the integral-number
      (complex real-part imaginary-part))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro iterate-the-string-with ((current-character-variable source)
                                   &body body)
  "Iterates the entire SOURCE string in a sinistrodextral procession,
   binding each character during its traversal to the agnomination
   desumed from the CURRENT-CHARACTER-VARIABLE, while executing the
   BODY forms, and finally returns no value."
  `(loop
     for ,current-character-variable
       of-type character
       across  ,source
     do
       ,@body
     finally
       (return (values))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the arithmetic operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (number)
  "Multiplies the NUMBER by itself and returns the yielded product."
  (declare (type integral-number number))
  (the integral-number
    (* number number)))

;;; -------------------------------------------------------

(define-modify-macro square-in-place ()
  square
  "Multiplies the first argument, this constituting a place, by itself,
   stores the result in this place, and returns its new state.")

;;; -------------------------------------------------------

(defun convert-into-an-integral-number (number)
  "Converts the NUMBER into an integral object, either, for an integer
   or floating-point scalar, producing an approximation by rounding down
   to the next lower integer less than or equal to the NUMBER; or, for
   a biscalar, assembling a new complex number whose both twissels, the
   real and imaginary parts, are rounded in such a fashion.
   ---
   The resulting object is vouched to either represent an integer scalar
   or a complex number composed two integer scalars."
  (declare (type number number))
  (the integral-number
    (if (complexp number)
      (complex
        (floor (realpart number))
        (floor (imagpart number)))
      (nth-value 0
        (floor number)))))

;;; -------------------------------------------------------

(defun square-root-of (number)
  "Supputates and returns the square root of the NUMBER."
  (declare (type number number))
  (the integral-number
    (convert-into-an-integral-number
      (sqrt number))))

;;; -------------------------------------------------------

(define-modify-macro supputate-the-square-root-in-place ()
  square-root-of
  "Supputates the square root of its first argument, this constituting
   a place, stores the result in this place, and returns its new
   state.")

;;; -------------------------------------------------------

(defun double-the-number (number)
  "Multiplies the NUMBER by a factor of two (2) and returns the
   resulting product."
  (declare (type integral-number number))
  (the integral-number
    (* number 2)))

;;; -------------------------------------------------------

(define-modify-macro double-in-place ()
  double-the-number
  "Multiplies its first argument, this constituting a place, by a factor
   of two (2), stores the resulting product in the place itself, and
   returns the new state.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the output operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-the-accumulator (accumulator)
  (:documentation
    "Prints the ACCUMULATOR state to the ``*query-io*'' destination in
     a format covenable to its species, and returns no value.")
  
  (:method ((accumulator integer))
    (declare (type integer accumulator))
    (format *query-io* "~d" accumulator)
    (values))
  
  (:method ((accumulator complex))
    (declare (type complex accumulator))
    (format *query-io* "~d~@di"
      (realpart accumulator)
      (imagpart accumulator))
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun input-a-number ()
  "Queries the ``*query-io*'' conduit for a signed or unsigned integer
   or a complex number, contingently repeatedly until such is committed,
   and returns the thus conceived datum."
  (the integral-number
    (prog ((original-input "")
           (parsed-input   NIL))
      (declare (type string           original-input))
      (declare (type (or null number) parsed-input))
      
      request-and-receive-an-input
        (format *query-io* "~&Please input an integer or ~
                              complex number: ")
        (finish-output *query-io*)
        (setf original-input
          (read-line *query-io* NIL NIL))
        (clear-input *query-io*)
        
        (setf parsed-input
          (ignore-errors
            (parse-an-integer-or-complex-number original-input)))
        
        (if parsed-input
          (go return-the-parsed-input)
          (go request-another-input))
      
      request-another-input
        (format *query-io* "The input of ~s cannot be parsed as an ~
                            integer or complex number. Please try ~
                            again."
          original-input)
        (finish-output *query-io*)
        (go request-and-receive-an-input)
      
      return-the-parsed-input
        (return-from input-a-number parsed-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun request-the-next-program-line ()
  "Queries the ``*query-io*'' conduit for a line of Schrodinger's fish
   code and returns the same."
  (format        *query-io* "~&>> ")
  (finish-output *query-io*)
  (the string
    (prog1
      (read-line   *query-io* NIL "")
      (clear-input *query-io*))))

;;; -------------------------------------------------------

(defun interpret-|Schrodinger's fish| (&optional (initial-program ""))
  "Starts the \"Schrodinger's fish\" interpreter, potentially utlizing
   the INITIAL-PROGRAM as an incipient program, and subsequently
   repeatedly querying for input and evaluating the same, and finally,
   if aborted in any fashion, returns no value."
  (declare (type string initial-program))
  (let ((accumulator 0))
    (declare (type integral-number accumulator))
    (loop
      for current-line
        of-type string
        =       initial-program
        then    (request-the-next-program-line)
      do
        (iterate-the-string-with (current-token current-line)
          (case current-token
            (#\s
              (square-in-place accumulator))
            (#\S
              (supputate-the-square-root-in-place accumulator))
            (#\i
              (incf accumulator))
            (#\I
              (setf accumulator (realpart accumulator)))
            (#\d
              (decf accumulator))
            (#\D
              (double-in-place accumulator))
            (#\o
              (print-the-accumulator accumulator))
            (#\O
              (setf accumulator
                (input-a-number)))
            (otherwise
              (format *query-io* "~%"))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A one-time numeric cat program.
(interpret-|Schrodinger's fish| "Oo")

;;; -------------------------------------------------------

;; A one-time numeric cat program which doubles its input.
(interpret-|Schrodinger's fish| "ODo")

;;; -------------------------------------------------------

;; Output the biscalar
;;   0+1i
(interpret-|Schrodinger's fish| "dSo")
