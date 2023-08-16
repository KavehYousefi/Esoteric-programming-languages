;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "lezy", invented by the Esolang user "Cinnamony" and
;; presented on June 15th, 2023, the only purpose of which resolves to
;; the detection of the term "lezy" in the source code, for any of its
;; occasions an eponymous message is issued.
;; 
;; 
;; Concept
;; =======
;; lezy programs are composed of a sequence of zero or more "lezy"
;; tokens only, admitting whitespaces in the interstices, but
;; vouchsafing no tolerance to any other content.
;; 
;; Each "lezy" token's consumption issues the display of the message
;; "lezy" to the standard output, desisting from the intrusion of
;; further entities, such as separating spaces.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-16
;; 
;; Sources:
;;   [esolang2023lezy]
;;   The Esolang contributors, "lezy", June 15th, 2023
;;   URL: "https://esolangs.org/wiki/Lezy"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Configuration of Common Lisp environment.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unlock the "common-lisp" package in order to homologate the
;; appropriation of the already defined symbol "position" as a global
;; variable.
#+sbcl (sb-ext:unlock-package :common-lisp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun in-source-p ()
  "Determines whether the POSITION designates a valid location in the
   SOURCE, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (special source))
  (declare (special position))
  (the boolean
    (not (null
      (array-in-bounds-p source position)))))

;;; -------------------------------------------------------

(defun on-whitespace-p ()
  "Determines whether the current POSITION in the SOURCE points to a
   whitespace character, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (special source))
  (declare (special position))
  (the boolean
    (not (null
      (and
        (in-source-p)
        (whitespace-character-p
          (char source position)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces ()
  "Proceeding from the current POSITION into the SOURCE, skips a
   sequence of zero or more accolent whitespaces, and returns no value."
  (declare (special source))
  (declare (special position))
  (loop while (on-whitespace-p) do (incf position))
  (values))

;;; -------------------------------------------------------

(defun expect-lezy ()
  "Proceeding from the current POSITION into the SOURCE, expects the
   subsequent four characters to replicate the text \"lezy\", on
   confirmation relocating the POSITION cursor to the location
   immediately succeeding the matching portion and returning no value,
   otherwise eliciting an error of an unspecified type."
  (declare (special source))
  (declare (special position))
  (let ((next-token
          (subseq source position
            (min
              (+ position 4)
              (length source)))))
    (declare (type string next-token))
    (if (string= next-token "lezy")
      (incf position 4)
      (error "Expected the word \"lezy\", but encountered the ~
              token ~s at position ~d."
        next-token position)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-lezy (source)
  "Interprets the piece of lezy SOURCE code and returns no value."
  (declare (special        source))
  (declare (type    string source))
  (let ((position 0))
    (declare (special        position))
    (declare (type    fixnum position))
    (loop
      initially
        (skip-whitespaces)
      while (in-source-p) do
        (expect-lezy)
        (format T "lezy")
        (skip-whitespaces)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "lezy".
(interpret-lezy "lezy")

;;; -------------------------------------------------------

;; Print "lezylezy".
(interpret-lezy "lezy lezy")

;;; -------------------------------------------------------

;; Print "lezylezylezylezylezy".
(interpret-lezy "lezy
  lezylezy
lezy               lezy")
