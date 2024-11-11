;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "DeadIFsh", invented by the Esolang user "ChuckEsoteric08"
;; and presented on September 1st, 2022, the haecceity of which
;; manifests in a derivation of Jonathan Todd Skinner's "Deadfish"
;; language by the addition of iterative and conditional control
;; structures.
;; 
;; 
;; Concept
;; =======
;; The DeadIFsh programming language establishes an extension of
;; Deadfish by a quadruple supererogation's contribution, the diorism
;; tallies three iterance constructs and an aefauld conditional
;; execution mechanism, the entire set of adscititious components
;; founded upon the numeric accumulator's state.
;; 
;; == THE MEMORY: AN AEFAULD ACCUMULATOR ==
;; The cleronomy vouchsafed by Deadfish to its derivation includes,
;; among other aspects, the memory's incarnation as a scalar integer
;; register of theoretically bourneless conformation along both axes,
;; however, subjected to a particular stipulation's governance, the same
;; imposes that, for a value of exactly -1 or 256, the state returns to
;; the incipial form of zero (0).
;; 
;; 
;; Instructions
;; ============
;; DeadIFsh extends Deadfish's quadruple instruction set to an octuple
;; account, the basic arithmetics and output capacities supplemented
;; by control flow helming warklumes.
;; 
;; == OVERVIEW ==
;; The following tabulation's dation shall amplect a cursory nortelry's
;; administration concerning the language's operative competences.
;; 
;; Please heed that any token not subsuming into the recognized variety
;; allies with the default response of printing a single newline
;; character to the standard output.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the accumulator value by one (1). If the new
;;           | state equals -1 or 256, the accumulator returns to its
;;           | default state of zero (0).
;;   ..................................................................
;;   d       | Decrements the accumulator value by one (1). If the new
;;           | state equals -1 or 256, the accumulator returns to its
;;           | default state of zero (0).
;;   ..................................................................
;;   s       | Squares the accumulator value, that is, multiplies it
;;           | by itself.
;;   ..................................................................
;;   o       | Prints the accumulator value in its verbatim numeric
;;           | form to the standard output, succeeded by a single
;;           | newline character's issuance.
;;   ..................................................................
;;   [       | Commences a loop which perpetuates while the accumulator
;;           | value does not equal zero (0). The loop body extends to
;;           | the matching "]" token.
;;   ..................................................................
;;   ]       | Demarcates the end of the matching "[" instruction's
;;           | body.
;;   ..................................................................
;;   (       | Commences a loop which perpetuates while the accumulator
;;           | value equals zero (0). The loop body extends to the
;;           | matching ")" token.
;;   ..................................................................
;;   )       | Demarcates the end of the matching "(" instruction's
;;           | body.
;;   ..................................................................
;;   {       | Commences a loop which perpetuates while the accumulator
;;           | value is strictly greater than zero (0). The loop body
;;           | extends to the matching "}" token.
;;   ..................................................................
;;   }       | Demarcates the end of the matching "{" instruction's
;;           | body.
;;   ..................................................................
;;   <       | Commences a conditional segment which shall be executed
;;           | once if the accumulator value equals zero (0). The
;;           | condition body extends to the matching ">" token.
;;   ..................................................................
;;   >       | Demarcates the end of the matching "<" instruction's
;;           | body.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's incarnation proceeds in the programming language
;; Common Lisp, the executive constituent operating immediately on the
;; source code string.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-11-09
;; 
;; Sources:
;;   [esolang2022DeadIFsh]
;;   The Esolang contributors, "DeadIFsh", September 1st, 2022
;;   URL: "https://esolangs.org/wiki/DeadIFsh"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type (type-name
                                  (candidate-name &rest lambda-list)
                                  &body body)
  "Defines a derived type whose agnomination is extracted from the
   TYPE-NAME, its formal parameters registering the LAMBDA-LIST's
   dation, and whose probed object partakes under the stevening imposed
   by the CANDIDATE-NAME, the docimasy proceeding by the BODY forms'
   evaluation, with the desinent form's primary return value
   constituting the assessment's ultimity, a \"generalized boolean\"
   response being tantamount to the candidate's compliance with the thus
   specified stipulations, while a \"false\" answer signifies its
   rejection.
   ---
   The first BODY form, if resolving to a string object, will be
   construed as the derived type's documentation string, and will be
   reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-predicated-type hash-table-of (candidate
                                       &optional (key-type   '*)
                                                 (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which complies with the KEY-TYPE and
   answers to a value of the VALUE-TYPE, for both holds the generic
   sentinel of ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (or (eq    key-type   '*)
                 (typep key        key-type))
             (or (eq    value-type '*)
                 (typep value      value-type))))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which complies with the ELEMENT-TYPE, for
   which holds the default of the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (eq element-type '*)
      (loop
        for    list-element of-type T in (the list candidate)
        always (typep list-element element-type)))))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral alliance betwixt matching
   jump points in a DeadIFsh program."
  '(hash-table-of fixnum fixnum))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-empty-jump-table ()
  "Creates and returns a fresh ``jump-table'' commorant in its pristine
   vacant state."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun populate-jump-table (jump-table code opening-token closing-token)
  "Connects the twains of jump points such commence at instances of the
   OPENING-TOKEN and terminate in the CLOSING-TOKEN, registers these
   vincula in the JUMP-TABLE, and returns the modified JUMP-TABLE."
  (declare (type jump-table jump-table))
  (declare (type string     code))
  (declare (type character  opening-token))
  (declare (type character  closing-token))
  (let ((start-points NIL))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-token    of-type character across code
      for current-position of-type fixnum    from   0 by 1
      
      if (char= current-token opening-token) do
        (push current-position start-points)
      else if (char= current-token closing-token) do
        (if start-points
          (let ((current-start-point (pop start-points)))
            (declare (type fixnum current-start-point))
            (psetf
              (gethash current-start-point jump-table)
                current-position
              (gethash current-position    jump-table)
                current-start-point))
          (error "Closing \"~c\" token without matching \"~c\" ~
                  instances found at position ~d."
            closing-token opening-token current-position))
      end
      
      finally
        (when start-points
          (let ((number-of-mismatches (length start-points)))
            (declare (type fixnum number-of-mismatches))
            (error "Opening \"~c\" token~p without matching \"~c\" ~
                    instance~p found at position~:p ~{~d, ~^~}."
              opening-token number-of-mismatches closing-token
              number-of-mismatches start-points)))))
  
  (the jump-table jump-table))

;;; -------------------------------------------------------

(defun build-jump-table (code)
  "Generates and returns for the piece of DeadIFsh source code a jump
   table which affiliates the various specimens of control flow
   constructs, distinguished by their species, in a bidirection mode."
  (declare (type string code))
  (the jump-table
    (populate-jump-table
      (populate-jump-table
        (populate-jump-table
          (populate-jump-table
            (prepare-empty-jump-table)
            code #\[ #\])
          code #\( #\))
        code #\{ #\})
      code #\< #\>)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table point-of-departure)
  "Expecting the POINT-OF-DEPARTURE to specifies the index of a jump
   point inside of the JUMP-TABLE, returns the obverse location, or,
   upon its disrespondency, signals an error of an unspecified type."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure jump-table)
        (error "No jump destination associated with the position ~d."
          point-of-departure))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (number)
  "Returns the product of the NUMBER multiplied by itself."
  (declare (type integer number))
  (the (integer 0 *)
    (* number number)))

;;; -------------------------------------------------------

(define-modify-macro squaref ()
  square
  "Modifies the first argument, the place, by setting its value to the
   product of the original value multiplied by itself.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of accumulator operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-accumulator (current-state)
  "Upon necessity normalizes the accumulator in its CURRENT-STATE to
   obey the pecularity of Deadfish's handling of the special values
   -1 and 256, and either returns the normalized state or the original
   CURRENT-STATE."
  (declare (type integer current-state))
  (the integer
    (or (and (or (= current-state -1)
                 (= current-state 256))
             0)
        current-state)))

;;; -------------------------------------------------------

(define-modify-macro normalizef ()
  normalize-accumulator
  "Upon necessity, normalizes its first argument, the place, this being
   construed as the current DeadIFsh accumulator state, by destructively
   modifying the same, or retains its original value, returning the
   either modified or pristine accumulator state.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-DeadIFsh (code)
  "Interprets the piece of DeadIFsh source CODE and returns no value."
  (declare (type string code))
  
  (let ((ip          0)
        (jump-table  (build-jump-table code))
        (accumulator 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type integer    accumulator))
    
    (symbol-macrolet
        ((program-completed-p
          (the boolean
            (not (null
              (>= ip (length code))))))
         (current-token
          (the character
            (char code ip))))
      (declare (type boolean   program-completed-p))
      (declare (type character current-token))
      
      (flet ((jump-to-destination ()
              "Expecting the instruction pointer (IP) to currently
               reside on a jump point, relocates the same to the obverse
               location and returns no value; otherwise signals an error
               of an unspecified type."
              (setf ip (get-jump-destination jump-table ip))
              (values)))
        
        (loop until program-completed-p do
          (normalizef accumulator)
          
          (case current-token
            ;; Standard Deadfish commands.
            (#\i (incf accumulator))
            (#\d (decf accumulator))
            (#\s (squaref accumulator))
            (#\o (format T "~d~%" accumulator))
            
            ;; Loop while not zero.
            (#\[
              (when (zerop accumulator)
                (jump-to-destination)))
            (#\]
              (unless (zerop accumulator)
                (jump-to-destination)))
            
            ;; Loop while zero.
            (#\(
              (unless (zerop accumulator)
                (jump-to-destination)))
            (#\)
              (when (zerop accumulator)
                (jump-to-destination)))
            
            ;; Loop while greater than zero.
            (#\{
              (unless (plusp accumulator)
                (jump-to-destination)))
            (#\}
              (when (plusp accumulator)
                (jump-to-destination)))
            
            ;; If zero.
            (#\<
              (unless (zerop accumulator)
                (jump-to-destination)))
            (#\>
              NIL)
            
            ;; Handle any non-command token as a newline output.
            (otherwise
              (format T "~%")))
          
          (incf ip)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Printing countdown from inclusive ten (10) to inclusive one (1):
(interpret-DeadIFsh "iiiiiiiiii[od]")

;;; -------------------------------------------------------

;; Perpetually print the integral numbers from inclusive zero (0)
;; through inclusive 255 and repeat the process.
(interpret-DeadIFsh "(oi[oi])")

;;; -------------------------------------------------------

;; Print the numbers from inclusive zero (0) through inclusive 255 once.
(interpret-DeadIFsh "oi[oi]")

;;; -------------------------------------------------------

;; Perpetually print the numbers from inclusive zero (0) through
;; inclusive 255.
(interpret-DeadIFsh "oi[oi<oi>]")

;;; -------------------------------------------------------

;; Square and print, commencing from inclusive two (2), until the
;; accumulator state is normalized from the value 256 to zero (0).
(interpret-DeadIFsh "ii[os]")
