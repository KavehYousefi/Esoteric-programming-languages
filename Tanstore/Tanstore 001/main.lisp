;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements encoders and decoders for the character
;; encoding system "Tanstore", invented by the Esolang user
;; "Tommyaweosme" and presented on June 6th, 2024, its existency's
;; vindication woning in an alternative's provision to the ASCII
;; coding system, restricting the circumference of its representable
;; characters to a cardinality enumerating 38 members, whence ensues a
;; componency incarnated in the space, newline, as well as Latin
;; letters, and decimal digits.
;; 
;; 
;; Concept
;; =======
;; Tanstore's furnishment encompasses an encoding for the 38 most
;; behoovable characters in the context of programming languages,
;; especially the subspecies embodied by the esoteric realm, affiliating
;; these entities with integral codes in the range [0, 37].
;; 
;; == TANSTORE: A SUBSET OF ASCII ==
;; The Tanstore encoding system capacitates the representation of
;; characters in a repertoire constrained in its admission to the
;; most peisant entities, as such furnishing an alternative to the more
;; potent "American Standard Code for Information Interchange" (ASCII)
;; encoding standard, to whom it establishes a subset.
;; 
;; == 26 LETTERS + 10 DIGITS + 2 WHITESPACES = 38 MEMBERS ==
;; Appertaining to a compass admissible to the most salient characters
;; employed in environment of programming languages, the Tanstore
;; standard's amplectation is laid around 38 members, intrining in this
;; account the 26 Latin letters, the ten decimal digits, and the twissel
;; of space and newline symbols.
;; 
;; The standard, however, abstains from a nominatim discrepancy's
;; imposition anent the question of majuscules and minuscular letters,
;; apportioning such deliberations to the respective implementation.
;; 
;; == EACH CHARACTER IS ENCODED AS A NON-NEGATIVE INTEGER ==
;; Adhering to the well probed stipulations of the ASCII notion, each
;; of the 38 available Tanstore character's representation proceeds by
;; the encoding in a non-negative integer number, desumed from the
;; closed interval [0, 37], engaged unambiguously in an affiliation to
;; the decoded entity.
;; 
;; In a concrete diction, the following associations hold, which, please
;; heed, prefers the majuscular Latin letters as a succedaneum in the
;; face of the ambiguity among the cases:
;; 
;;   ------------------------------
;;   Code | Character | Category
;;   -----+-----------+------------
;;      0 |  (space)  | Spaces    
;;   ..............................
;;      1 |     A     | Alphabetic
;;   ..............................
;;      2 |     B     | Alphabetic
;;   ..............................
;;      3 |     C     | Alphabetic
;;   ..............................
;;      4 |     D     | Alphabetic
;;   ..............................
;;      5 |     E     | Alphabetic
;;   ..............................
;;      6 |     F     | Alphabetic
;;   ..............................
;;      7 |     G     | Alphabetic
;;   ..............................
;;      8 |     H     | Alphabetic
;;   ..............................
;;      9 |     I     | Alphabetic
;;   ..............................
;;     10 |     J     | Alphabetic
;;   ..............................
;;     11 |     K     | Alphabetic
;;   ..............................
;;     12 |     L     | Alphabetic
;;   ..............................
;;     13 |     M     | Alphabetic
;;   ..............................
;;     14 |     N     | Alphabetic
;;   ..............................
;;     15 |     O     | Alphabetic
;;   ..............................
;;     16 |     P     | Alphabetic
;;   ..............................
;;     17 |     Q     | Alphabetic
;;   ..............................
;;     18 |     R     | Alphabetic
;;   ..............................
;;     19 |     S     | Alphabetic
;;   ..............................
;;     20 |     T     | Alphabetic
;;   ..............................
;;     21 |     U     | Alphabetic
;;   ..............................
;;     22 |     V     | Alphabetic
;;   ..............................
;;     23 |     W     | Alphabetic
;;   ..............................
;;     24 |     X     | Alphabetic
;;   ..............................
;;     25 |     Y     | Alphabetic
;;   ..............................
;;     26 |     Z     | Alphabetic
;;   ..............................
;;     27 |     0     | Numeric   
;;   ..............................
;;     28 |     1     | Numeric   
;;   ..............................
;;     29 |     2     | Numeric   
;;   ..............................
;;     30 |     3     | Numeric   
;;   ..............................
;;     31 |     4     | Numeric   
;;   ..............................
;;     32 |     5     | Numeric   
;;   ..............................
;;     33 |     6     | Numeric   
;;   ..............................
;;     34 |     7     | Numeric   
;;   ..............................
;;     35 |     8     | Numeric   
;;   ..............................
;;     36 |     9     | Numeric   
;;   ..............................
;;     37 | (newline) | Linebreaks
;;   ------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The Tanstore protolog's curtailed nature accommodates the contingency
;; of inroads for ambivalencies in its eisegesis, whence the most
;; peisant subset's extrication shall be exercised.
;; 
;; == WHICH CASE APPERTAINS TO LETTERS? ==
;; The gamut of 26 places reserved among the 38 participants for Latin
;; letters educes the inquisition into the stipulated case, which in
;; the original specification remains destitute of resolution.
;; 
;; It has been adjudged to tolerate the imputation of an
;; implementation-depedent aspect in this concern. The implementation
;; at hand, for instance, abides to majuscules.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-02-06
;; 
;; Sources:
;;   [esolang2024Tanstore]
;;   The Esolang contributors, "Tanstore", August 10th, 2024
;;   URL: "https://esolangs.org/wiki/Tanstore"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tanstore-code ()
  "The ``tanstore-code'' type defines a numeric character code conable
   for the representation of a Tanstore symbol, specified as an integer
   number commorant in the closed interval [0, 37]."
  '(integer 0 37))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which encompasses, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Tanstore-Error (error)
  ()
  (:documentation
    "The ``Tanstore-Error'' condition type serves as the foundry for
     all conditions dedicated to the interaction with the Tanstore
     encoding system."))

;;; -------------------------------------------------------

(define-condition No-Tanstore-Character-Error (Tanstore-Error)
  ((offending-character
    :initarg       :offending-character
    :initform      (error "Missing offending character.")
    :reader        no-tanstore-character-error-offending-character
    :type          character
    :documentation "The character absent from Tanstore's repertoire,
                    whose code has been requested."))
  (:report
    (lambda (condition stream)
      (declare (type No-Tanstore-Character-Error condition))
      (declare (type destination                 stream))
      (format stream "The character \"~c\" cannot be represented in ~
                      the Tanstore encoding system."
        (no-tanstore-character-error-offending-character condition))))
  (:documentation
    "The ``No-Tanstore-Character-Error'' condition type serves in the
     encapsulation of an anomalous situation ensuing from the attempt
     to request the Tanstore code for a character not participating in
     the Tanstore character repertoire."))

;;; -------------------------------------------------------

(define-condition Invalid-Tanstore-Code-Error (Tanstore-Error)
  ((offending-code
    :initarg       :offending-code
    :initform      (error "Missing offending code.")
    :reader        invalid-tanstore-code-error-offending-code
    :type          integer
    :documentation "The attempted character code absent from the valid
                    Tanstore character code range."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Tanstore-Code-Error condition))
      (declare (type destination                 stream))
      (format stream "The identifier ~d does not represents a valid ~
                      Tanstore character code."
        (invalid-tanstore-code-error-offending-code condition))))
  (:documentation
    "The ``Invalid-Tanstore-Code-Error'' condition type serves in the
     encapsulation of an anomalous situation ensuing from the attempt
     to request the character amenable to a code not defined in the
     Tanstore character code range."))



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
;; -- Implementation of encoding and decoding operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-tanstore-code (character)
  "Returns the Tanstore code affiliated with the CHARACTER, or signals
   an error of the type ``No-Tanstore-Character-Error'' upon its
   disrespondency with the Tanstore character repertoire."
  (declare (type character character))
  (the tanstore-code
    (or (position character +TANSTORE-SYMBOLS+ :test #'char-equal)
        (error 'No-Tanstore-Character-Error
          :offending-character character))))

;;; -------------------------------------------------------

(defun get-tanstore-character (code)
  "Returns the character amenable to the Tanstore character CODE, or
   signals an error of the type ``Invalid-Tanstore-Code-Error'' upon
   its disrespondency with the valid Tanstore character code range."
  (declare (type tanstore-code code))
  (the character
    (if (array-in-bounds-p +TANSTORE-SYMBOLS+ code)
      (schar +TANSTORE-SYMBOLS+ code)
      (error 'Invalid-Tanstore-Code-Error :offending-code code))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replicate the message "HELLO WORLD" employing the Tanstore encoding.
(let ((my-tanstore-codes '(8 5 12 12 15 0 23 15 18 12 4)))
  (declare (type list my-tanstore-codes))
  (dolist (current-tanstore-code my-tanstore-codes)
    (format T "~c"
      (get-tanstore-character current-tanstore-code)))
  (values))

;;; -------------------------------------------------------

;; Print the Tanstore codes of the message "HELLO WORLD", namely:
;;   8, 5, 12, 12, 15, 0, 23, 15, 18, 12, 4
(let ((my-message "HELLO WORLD"))
  (declare (type (simple-string 11) my-message))
  (loop for current-character of-type character across my-message do
    (format T "~&~d"
      (get-tanstore-code current-character)))
  (values))
