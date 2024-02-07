;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements those operations in relationship with the
;; indagation and manipulation of characters.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global variables and constants.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 3 3) +ELLIPSIS-EXTENT+))

;;; -------------------------------------------------------

(defparameter +ELLIPSIS-EXTENT+ 3
  "The space in characters appropriated by the ellipsis (\"...\").")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal tab
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun linebreak-character-p (candidate)
  "Determines whether the CANDIDATE represents a linbreak character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (char= candidate #\Newline))))

;;; -------------------------------------------------------

(defun word-jointure-p (candidate)
  "Determines whether the CANDIDATE represents a symbol connable for the
   separation of one word or the jointure of a twain, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (find candidate "-_" :test #'char=))))

;;; -------------------------------------------------------

(defun word-separator-p (candidate)
  "Determines whether the CANDIDATE represents a character covenable to
   a single word's or two accolent word's segregation, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (or (space-character-p     candidate)
          (linebreak-character-p candidate)
          (word-jointure-p       candidate)))))

;;; -------------------------------------------------------

(defun word-jointure-follows-p (source position)
  "Determines whether the a word jointure character exists at the
   POSITION into the SOURCE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (get-boolean-value
      (and
        (array-in-bounds-p source position)
        (word-jointure-p
          (char source position))))))

;;; -------------------------------------------------------

(defun number-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent for a
   signed or unsigned integer literal, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value
      (or (find candidate "+-" :test #'char=)
          (digit-char-p candidate)))))

;;; -------------------------------------------------------

(defun curtail-text (text width)
  "Returns a new string based upon the TEXT with the section incapable
   of its accommodation into the WIDTH being substituted by an ellipsis
   (\"...\")."
  (declare (type string text))
  (let ((text-extent (length text)))
    (declare (type fixnum text-extent))
    (the string
      (cond
        ;; WIDTH accommodates hardly or exactly the space for the
        ;; ellipsis?
        ;; => Simply curtail the text without trailing dots.
        ((<= width +ELLIPSIS-EXTENT+)
          (subseq text 0 width))
        ;; TEXT matches into the WIDTH?
        ;; => No ellipsis required.
        ((<= text-extent width)
          text)
        ;; TEXT is wider than WIDTH?
        ;; => Elide its trail and append the ellipsis ("...").
        (T
          (format NIL "~a..."
            (subseq text 0
              (- width +ELLIPSIS-EXTENT+))))))))
