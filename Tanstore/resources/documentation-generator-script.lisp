;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements operations for the generation of tabulations
;; ostending the Tanstore encoding principles in documentatary
;; contexts.
;; 
;; In particular, the associations betwixt the Tanstore codes and their
;; allied symbols may be replicated in molds governed by a conformance
;; with Common Lisp comment sections and the "Esolang" website's
;; "wiki table" format.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-04
;; 
;; Sources:
;;   [esolang2024Tanstore]
;;   The Esolang contributors, "Tanstore", August 10th, 2024
;;   URL: "https://esolangs.org/wiki/Tanstore"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of Tanstore symbol table.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-string 38) +TANSTORE-SYMBOLS+))

;;; -------------------------------------------------------

(defparameter +TANSTORE-SYMBOLS+
  " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
"
  "Associates the Tanstore symbols with their character codes by
   adminiculum of their locations inside of this string.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of code generator operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-official-category-for-tanstore-symbol (symbol)
  "Returns the original category designation, desumed in an ipsissima
   verba mode from the Tanstore protolog, into which the Tanstore SYMBOL
   is subsumed."
  (declare (type character symbol))
  (the simple-string
    (cond
      ((char=        symbol #\Space)   "(space)")
      ((char=        symbol #\Newline) "(newline)")
      ((alpha-char-p symbol)           "alphabet")
      ((digit-char-p symbol)           "numberbet")
      (T (error "No matching category found for ~s." symbol)))))

;;; -------------------------------------------------------

(defun get-formal-category-for-tanstore-symbol (symbol)
  "Returns a formal category designation into which the Tanstore SYMBOL
   is subsumed."
  (declare (type character symbol))
  (the simple-string
    (cond
      ((char=        symbol #\Space)   "Spaces")
      ((char=        symbol #\Newline) "Linebreaks")
      ((alpha-char-p symbol)           "Alphabetic")
      ((digit-char-p symbol)           "Numeric")
      (T (error "No matching category found for ~s." symbol)))))

;;; -------------------------------------------------------

(defun get-display-name-for-tanstore-symbol (symbol)
  "Returns for the Tanstore SYMBOL a covenable display name
   representation."
  (declare (type character symbol))
  (the (or character simple-string)
    (case symbol
      (#\Space   "(space)")
      (#\Newline "(newline)")
      (otherwise symbol))))

;;; -------------------------------------------------------

(defun print-sepiment (character number-of-repetitions)
  "Prints a sepiment line composed of a NUMBER-OF-REPETITIONS tally of
   duplicates of the CHARACTER to the standard output and returns no
   value."
  (declare (type character     character))
  (declare (type (integer 0 *) number-of-repetitions))
  (format T "~&~v@{~c~:*~}" number-of-repetitions character)
  (values))

;;; -------------------------------------------------------

(defun generate-lisp-comment-table ()
  "Generates a tabulation comprehending the Tanstore codes and allied
   characters in a format conable for the deployment in a Common Lisp
   documentation, prints thilk to the standard output, and returns no
   value."
  (loop
    initially
      (print-sepiment #\- 30)
      (format T "~&Code | Character | Category")
      (format T "~&-----+-----------+------------")
    
    for symbol
      of-type character
      across  +TANSTORE-SYMBOLS+
    for category
      of-type string
      =       (get-formal-category-for-tanstore-symbol symbol)
    and character-code
      of-type (integer 0 38)
      from    0
      to      37
    and first-row-p
      of-type boolean
      =       T
      then    NIL
    do
      (unless first-row-p
        (print-sepiment #\. 30))
      
      (format T "~&~4<~d~> | ~9:@<~a~> | ~10@<~a~>"
        character-code
        (get-display-name-for-tanstore-symbol symbol)
        category)
    
    finally
      (print-sepiment #\- 30))
  (values))

;;; -------------------------------------------------------

(defun generate-wiki-table ()
  "Generates a tabulation comprehending the Tanstore codes and allied
   characters in a format conable for the deployment in a Esolang
   documentation, prints thilk to the standard output, and returns no
   value."
  (loop
    initially
      (format T "~&{| class=\"wikitable sortable\"")
      (format T "~&|-")
      (format T "~&! Code !! Character !! Category")
    
    for symbol
      of-type character
      across  +TANSTORE-SYMBOLS+
    for category
      of-type string
      =       (get-official-category-for-tanstore-symbol symbol)
    and character-code
      of-type (integer 0 38)
      from    0
      to      37
    do
      (format T "~&|-")
      (format T "~&| ~d || ~a || ~a"
        character-code
        (get-display-name-for-tanstore-symbol symbol)
        category)
    
    finally
      (format T "~&|}"))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Invocation of documentation generators.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(generate-lisp-comment-table)

;;; -------------------------------------------------------

(generate-wiki-table)
