;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the lexer, or lexical analyzer, its status that
;; of a warklume for the extraction of pertinent information from a
;; piece of Tuvars source code committed in a string form.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-word-boundary (source start)
  "Determines, proceeding from the START position into the SOURCE, the
   location in the SOURCE of the nearest word terminator, that is,
   either a space or the SOURCE's desinence, and returns the same."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word
   terminated by the nearest space or the SOURCE's desinence, and
   returns two values:
     (1) A string representation of the consumed word.
     (2) The position in the SOURCE immediately succeeding the segment
         occupied by the word."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((word-end-position (find-word-boundary source start)))
    (declare (type fixnum word-end-position))
    (the (values string fixnum)
      (values
        (subseq source start word-end-position)
        word-end-position))))

;;; -------------------------------------------------------

(defun find-number-boundary (source start)
  "Determines, proceeding from the START position into the SOURCE, the
   location in the SOURCE of nearest numerical literal terminator, that
   is, any content not encompassing a mathematical signum or a decimal
   digit, and returns the same."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'number-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-integer (source start)
  "Proceeding from the START position into the SOURCE, reads a signed or
   unsigned integer literal, and returns two values:
     (1) An integer representation of the consumed numerical literal.
     (2) The position in the SOURCE immediately succeeding the segment
         occupied by the numerical literal."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((number-end-position (find-number-boundary source start)))
    (declare (type fixnum number-end-position))
    (the (values integer fixnum)
      (values
        (parse-integer source :start start :end number-end-position)
        number-end-position))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION into the SOURCE, or ``NIL''
   upon a transcendence of the SOURCE's bournes."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun read-variable-name (source start)
  "Proceeding from the START position into the SOURCE, reads one of the
   two recognized variable names --- either \"a\" or \"b\" --- and
   returns two values:
     (1) The verbatim character representation of the variable name.
     (2) The position in the SOURCE immediately succeeding the segment
         occupied by the variable name."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((character-at-position (get-character-at source start)))
    (declare (type (or null character) character-at-position))
    (the (values character fixnum)
      (case character-at-position
        ((NIL)
          (error "Expected a variable name, but encountered ~
                  end of file at position ~d on the line."
            character-at-position))
        ((#\a #\b)
          (values character-at-position
            (1+ start)))
        (otherwise
          (error "Expected a variable name, but encountered the ~
                  character \"~c\" at position ~d."
            character-at-position start))))))

;;; -------------------------------------------------------

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, returns the
   location in the SOURCE of the nearest non-space character, or, if
   none such could be detected, the SOURCE length itself."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun comment-follows-p (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, a comment, introduced via the sequence \"//\", follows,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (the boolean
    (get-boolean-value
      (string= source "//"
        :start1 start
        :end1   (min (+ start 2)
                     (length source))))))

;;; -------------------------------------------------------

(defun skip-comment (source start)
  "Proceeding from the START position into the SOURCE, skips an optional
   line comment, if such, exists, returning upon confirmation the length
   of the SOURCE, otherwise the START location itself."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or
      (and (comment-follows-p source start)
           (length source))
      start)))

;;; -------------------------------------------------------

(defun expect-end-of-line (line start)
  "Determines, proceeding from the START position into the LINE, whether
   the subsequent segment does not contain any effective content, which
   incorporates in the tolerated diorism spaces and a comment, on
   confirmation simply returning no value, otherwise signaling an error
   of an unspecified type."
  (declare (type string line))
  (declare (type fixnum start))
  (let ((position 0))
    (declare (type fixnum position))
    (setf position (skip-spaces  line start))
    (setf position (skip-comment line position))
    (when (< position (length line))
      (error "Expected the end of the line succeeding a command, ~
              but encountered ~s starting at position ~d."
        (subseq line position) position)))
  (values))
