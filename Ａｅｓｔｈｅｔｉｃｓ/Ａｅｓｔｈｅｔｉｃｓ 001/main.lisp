;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Ａｅｓｔｈｅｔｉｃｓ", invented by the Esolang user Wallace
;; Dutra and presented on January 16th, 2021, its entheus the vaporwave
;; subculture, in particular the artist Ramona Andra Langley, known by
;; the nom de guerre "Vektroid", visual arts, and music, concluding in
;; its programs' expression in Japanese characters and certain phrases.
;; 
;; 
;; Concept
;; =======
;; The Ａｅｓｔｈｅｔｉｃｓ programming language, alternatively norned
;; "a e s t h e t i c s" or "Ａ Ｅ Ｓ Ｔ Ｈ Ｅ Ｔ Ｉ Ｃ S", constitutes
;; a joke language, a ludibund species from the programming realm, its
;; indicia imputed acquisitions emanating from chatbots and the social
;; phenomenon of "hikikomori", an ilk of reclusion autochthonous to the
;; Japanese culture. The instructions' agnomination issues from a
;; confounded amalgam of Japanese glyphs and certain phrases desumed
;; the vaporwave subgenre of electronic music.
;; 
;; 
;; Instructions
;; ============
;; A quadruple manifestation of componency, Ａｅｓｔｈｅｔｉｃｓ's
;; competences exclusively incorporate popular output-oriented programs,
;; as well as the mateotechny involved in an otiose accumulator's
;; augmentation.
;; 
;; == OVERVIEW ==
;; An apercu concerning the language's operational facilities shall
;; impart a basic mete of nortelry:
;; 
;;   ------------------------------------------------------------------
;;   Command               | Effect
;;   ----------------------+-------------------------------------------
;;   リサフランク420 / 現代のコンピュー! | Prints the message "Hello,World!" to
;;                         | the standard output.
;;   ..................................................................
;;   Ａｅｓｔｈｅｔｉｃｓ            | Prints the lyrics of the song
;;                         | "99 Bottles of Beer" to the standard
;;                         | output.
;;   ..................................................................
;;   ＭＡＣＩＮＴＯＳＨ　ＰＬＵＳ        | Prints the program's source
;;                         | code to the standard output; that is,
;;                         | affords a quine.
;;   ..................................................................
;;   蒸気波                   | Increments the hidden accumulator by
;;                         | one.
;;   ------------------------------------------------------------------
;; 
;; The dioristic measurements innate to the several Unicode glyphs may
;; contribute, in certain fonts at least, to a detriment in the aboon
;; layout's aesthetics; hence, an alternative design shall be
;; supplemented for those insuffiently served by the preterit
;; exposition:
;; 
;;   ------------------------------------------------------------------
;;    _________________________________________________________________
;;   |Command
;;   |> Effect
;;   |_________________________________________________________________
;;    __
;;   |リサフランク420 / 現代のコンピュー!
;;   |> Prints the message "Hello,World!" to the standard output.
;;    __
;;   |Ａｅｓｔｈｅｔｉｃｓ
;;   |> Prints the lyrics of the song "99 Bottles of Beer" to the
;;   |  standard output.
;;    __
;;   |ＭＡＣＩＮＴＯＳＨ　ＰＬＵＳ
;;   |> Prints the program's source code to the standard output;
;;   |  that is, affords a quine.
;;    __
;;   |蒸気波
;;   |> Increments the hidden accumulator by one.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in the programming language
;; Common Lisp, the mode of its administration deliberately chosen in a
;; cavalier fashion that should pursue the Ａｅｓｔｈｅｔｉｃｓ language's
;; liberal haecceity.
;; 
;; Concerning the program code printer, also known as the "quine",
;; please heed that the same constitutes a "cheating" variant, which
;; simply repeats the source code in an ipsissima verba mode.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle ([christensen2013lispcabinet035]).
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-09-27
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Ａｅｓｔｈｅｔｉｃｓ]
;;   The Esolang contributors, "Ａｅｓｔｈｅｔｉｃｓ", January 10th, 2023
;;   URL: "https://esolangs.org/wiki/%EF%BC%A1%EF%BD%85%EF%BD%93%EF%BD%94%EF%BD%88%EF%BD%85%EF%BD%94%EF%BD%89%EF%BD%83%EF%BD%93"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype association-list-of (&optional (indicator-type '*)
                                        (value-type     '*))
  "The ``association-list-of'' type defines an associaton list, or
   alist, composed of zero or more entries, each indicator, or key, of
   which conforms to the INDICATOR-TYPE and associates with a value of
   the VALUE-TYPE, both defaulting to the generic ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element
                       `(cons ,indicator-type ,value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variation of
   instructions."
  '(member
    :print-hello-world
    :print-99-bottles-of-beer
    :print-quine
    :increment-accumulator))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping from instruction
   identifiers to representative objects, realized an association list,
   or alist, whose string keys contribute the former, and relate to the
   ``instruction'' objects for the latter."
  '(association-list-of simple-string instruction))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   which conform to the ELEMENT-TYPE, the same defaults to the generic
   ``*'' sentinel."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (eq element-type '*)
              (loop
                for    element of-type T in (the list candidate)
                always (typep element element-type))))))
    `(satisfies ,predicate)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("リサフランク420 / 現代のコンピュー!" . :print-hello-world)
    ("Ａｅｓｔｈｅｔｉｃｓ"         . :print-99-bottles-of-beer)
    ("ＭＡＣＩＮＴＯＳＨ　ＰＬＵＳ"  . :print-quine)
    ("蒸気波"                      . :increment-accumulator))
  "Associates the recognized identifiers with representative instruction
   objects.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Aesthetics-Program".                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Aesthetics-Program
  (:constructor make-aesthetics-program (source instructions)))
  "The ``Aesthetics-Program'' class serves in the encapsulation of a
   parsed Ａｅｓｔｈｅｔｉｃｓ program, entailing both its source code and
   its extract instruction sequence, as well as the hidden accumulator."
  (source       (error "Missing source.") :type string)
  (instructions NIL                       :type (list-of instruction))
  (accumulator  0                         :type integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces, and returns the location into
   the SOURCE of the first non-whitespace character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p
                (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun identifier-found-at-p (identifier source position)
  "Determines whether, anchored at the specified POSITION into the
   SOURCE, the subsequent characters match the IDENTIFIER, on
   confirmation returning a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type simple-string identifier))
  (declare (type string        source))
  (declare (type fixnum        position))
  (the boolean
    (not (null
      (string= source identifier
        :start1 position
        :end1   (min (+ position (length identifier))
                     (length source))
        :start2 0
        :end2   (length identifier))))))

;;; -------------------------------------------------------

(defun read-instruction (source position)
  "Commencing at the POSITION into the SOURCE, reads an
   Ａｅｓｔｈｅｔｉｃｓ instruction, recognized by its identifier, and
   returns two values:
     (1) The extracts ``instruction''.
     (2) The location into the SOURCE immediately succeeding the
         extracted instruction."
  (declare (type string source))
  (declare (type fixnum position))
  (the (values instruction fixnum)
    (loop
      for (identifier . instruction)
        of-type (simple-string . instruction)
        in      +IDENTIFIERS+
      when (identifier-found-at-p identifier source position) do
        (return
          (values instruction
            (+ position (length identifier))))
      finally
        (error "Invalid character \"~c\" at position ~d."
          (char source position) position))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Extracts from the SOURCE the incorporated Ａｅｓｔｈｅｔｉｃｓ
   instructions and returns an ``Aesthetics-Program'' representation
   thereof."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((consume-instruction (instruction new-position)
            "Updates the current POSITION to the NEW-POSITION,
             contingently, as a parhedral activity, skipping subsequent
             whitespaces, and returns the INSTRUCTION verbatim."
            (declare (type instruction instruction))
            (declare (type fixnum      new-position))
            (setf position
              (skip-whitespaces source new-position))
            (the instruction instruction)))
      (the Aesthetics-Program
        (loop
          initially
            (setf position (skip-whitespaces source position))
          while
            (< position (length source))
          collect
            (multiple-value-call #'consume-instruction
              (read-instruction source position))
          into
            instructions
          finally
            (return
              (make-aesthetics-program source instructions)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type null print-hello-word))
(declaim (type null print-99-bottles-of-beer))

;;; -------------------------------------------------------

(define-symbol-macro print-hello-world
  (progn
    (let ((|what do I say?| 10334410032597754628216218952))
      (declare (type (unsigned-byte 96) |what do I say?|))
      (loop for bit-offset of-type (integer 0 96) from 0 to 95 by 8 do
        (write-char
          (code-char
            (ldb (byte 8 bit-offset)
                 |what do I say?|)))))
    (values)))

;;; -------------------------------------------------------

(define-symbol-macro print-99-bottles-of-beer
  (prog ((remaining-bottles 99))
    (declare (type (integer 0 99) remaining-bottles))
    sing-again
      (format T "~&~d bottle~:p of beer on the wall," remaining-bottles)
      (format T "~&~d bottle~:p of beer,"             remaining-bottles)
      (format T "~&Take one down pass it around,")
      (decf remaining-bottles)
      (if (plusp remaining-bottles)
        (go more-bottles)
        (go no-more-bottles))
    more-bottles
      (format T "~&~d bottle~:p of beer on the wall." remaining-bottles)
      (format T "~2%")
      (go sing-again)
    no-more-bottles
      (format T "~&No bottles of beer on the wall.~%")
      (format T "~2%")
      (go home)
    home
      (values)))

;;; -------------------------------------------------------

(defun process-program (program)
  "Processes the Ａｅｓｔｈｅｔｉｃｓ PROGRAM and returns no value."
  (declare (type Aesthetics-Program program))
  (symbol-macrolet ((print-quine
                      (progn
                        (format T "~&~a"
                          (aesthetics-program-source program))
                        (values)))
                     (increment-accumulator
                      (progn
                        (incf (aesthetics-program-accumulator program))
                        (values))))
    (declare (type null print-quine))
    (declare (type null increment-accumulator))
    (dolist (instruction (aesthetics-program-instructions program))
      (case instruction
        (:print-hello-world        print-hello-world)
        (:print-99-bottles-of-beer print-99-bottles-of-beer)
        (:print-quine              print-quine)
        (:increment-accumulator    increment-accumulator)
        (otherwise (error "Invalid instruction: ~s." instruction)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Ａｅｓｔｈｅｔｉｃｓ (code)
  "Interprets the piece of Ａｅｓｔｈｅｔｉｃｓ source code and returns no
   value."
  (declare (type string code))
  (process-program
    (parse-program code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate all language facilities:
;;   (1) Print "Hello,World!".
;;   (2) Print the lyrics of the song "99 Bottles of Beer".
;;   (3) Print a quine.
;;   (4) Increment the hid accumulator.
(interpret-Ａｅｓｔｈｅｔｉｃｓ
  "リサフランク420 / 現代のコンピュー!
   Ａｅｓｔｈｅｔｉｃｓ
   ＭＡＣＩＮＴＯＳＨ　ＰＬＵＳ
   蒸気波")
