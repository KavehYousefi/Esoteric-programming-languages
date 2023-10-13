;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Miser", invented by the Esolang user "None1" and presented
;; on August 6th, 2023, the format of which is exhausted by an aefauld
;; sentence pattern's stringency, acting in the mimicry of a
;; beseechment for lending a desired amount of money from the eponymous
;; miser, the only causatum begotten therefrom constitutes their
;; denial's declamation, while incorrect programs do not incur an error,
;; but result in an expression of incomprehension.
;; 
;; 
;; Concept
;; =======
;; The Miser programming language constitutes a jocular species desumed
;; from the esoteric ambitus, its conceptual governance the simulation
;; of a person's request to borrow money from a peer, whose indicial
;; euclionism produces a denying message.
;; 
;; == VALID MISER PROGRAMS ADHERE TO A SPECIFIC SENTENTIAL DESIGN ==
;; A program in Miser to whom validity is apportioned must concord with
;; a sequence of zero or more specimens conformant with the following
;; general forbisen:
;; 
;;   I want to borrow $amount from you.
;;                     ******
;; 
;; where {amount} constitutes an unsigned non-negative integer or
;; floating-point number, the mickleness of which is not restricted at
;; all.
;; 
;; The distribution of spacings therein appropriates a requisitum's
;; status; their concrete species and tally, however, does not obey to
;; further obligations, which implies that:
;; 
;;   (1) The spacing may be represented by a simple space (" "), a
;;       horizontal tab, or a newline character.
;;   (2) The spacing's extent must comprehend one or more repetitions
;;       of any of the above specified sepiment symbols, their tally
;;       towards the upper extremum eluding a natural bourne.
;;   (3) The concluding period ("."), ensuing from the desinent word
;;       "you", may encompass an optional series of whitespaces.
;;   (4) The period, utible in a twissel agency as a sentence's
;;       desinence, as well as the demarcation of a potential successor,
;;       may optionally be followed by zero or more whitespaces.
;; 
;; == A SUCCESSFUL MISER COMMAND OUTPUTS THE REQUESTED AMOUNT ==
;; A correct commands's evaluation answers with the following message
;; format:
;; 
;;   $amount is too much.
;;    ******
;; 
;; where the {amount} is substituted by the source code's declaration
;; for such, which please see immediately aboon.
;; 
;; == INVALID SENTENCES RESPOND WITH "I can't understand that." ==
;; A program whose syntaxis infringes on the specification does not
;; elicit a veridical error; instead, the following message is issued to
;; the standard output for a sentence disobedient in this manner:
;; 
;;   I can't understand that.
;; 
;; 
;; Instructions
;; ============
;; Miser's instruction set is delineated by a singularity's purview, a
;; pseudonatural linguistic expression of a request for money, which is,
;; as a consectary of the imploration's recipient, replied with a
;; message of denial.
;; 
;; Any derivation from the expected command design will elicit a
;; response of failed admission, but will never experience a veridical
;; error's obtrusion.
;; 
;; == OVERVIEW ==
;; The aefault command's design and designment shall be following,
;; short, treatise's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Command                            | Effect
;;   -----------------------------------+------------------------------
;;   I want to borrow $amount from you. | Prints to the standard output
;;                     ******           | the message
;;                                      |   $amount is too much.
;;                                      |    ******
;;                                      | where {amount} designates the
;;                                      | parameter's verbatim
;;                                      | reiteration.
;;                                      |------------------------------
;;                                      | The {amount} must be an
;;                                      | unsigned non-negative integer
;;                                      | or floating-point number of
;;                                      | any magnitude.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-12
;; 
;; Sources:
;;   [esolang2023Miser]
;;   The Esolang contributors, "Miser", August 6th, 2023
;;   URL: "https://esolangs.org/wiki/Miser"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Unrecognized-Word-Condition (simple-condition)
  ()
  (:documentation
    "The ``Unrecognized-Word-Condition'' type serves to signal an
     anomalous situation the etiology of which germinates from an
     unexpected character or word encountered during the parsing of a
     piece of Miser source code."))

;;; -------------------------------------------------------

(defun signal-unrecognized-word ()
  "Signals a condition of the type ``Unrecognized-Word-Condition''."
  (signal 'Unrecognized-Word-Condition
    :format-control "I can't understand that."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whethe the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a word constituent, a
   diorism which amplects Latin letters only, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alpha-char-p candidate)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Scanner
  (:constructor make-scanner
                  (source
                   &aux (position 0)
                        (character
                          (when (array-in-bounds-p source position)
                            (char source position))))))
  "The ``Scanner'' class furnishes an entity whose competence entails
   the extraction of tokens from a piece of Miser source code."
  (source    (error "Missing source.") :type string)
  (position  0                         :type fixnum)
  (character NIL                       :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-scanner ((scanner) &body body)
  "Furnishes a convenient warklume for accessing a ``Scanner'''s slots
   by evaluating the SCANNER, binding its slot ``source'' to the local
   symbol macro ``$source'', its ``position'' to ``$position'', and its
   ``character'' to ``$character'', while evaluating the BODY granting
   access to these bindings, and returning the last evaluated BODY
   form's results."
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Scanner ,evaluated-scanner))
       (declare (ignorable    ,evaluated-scanner))
       (symbol-macrolet
           (($source
             (the string
               (scanner-source ,evaluated-scanner)))
            ($position
             (the fixnum
               (scanner-position ,evaluated-scanner)))
            ($character
             (the (or null character)
               (scanner-character ,evaluated-scanner))))
         (declare (type string              $source))
         (declare (ignorable                $source))
         (declare (type fixnum              $position))
         (declare (ignorable                $position))
         (declare (type (or null character) $character))
         (declare (ignorable                $character))
         ,@body))))

;;; -------------------------------------------------------

(defun consume-character (scanner)
  "Returns the SCANNER's current character, or ``NIL'' if its source is
   exhausted, and advances its position cursor to the next character in
   the source, if possible."
  (declare (type Scanner scanner))
  (the (or null character)
    (with-scanner (scanner)
      (prog1 $character
        (setf $character
          (when (array-in-bounds-p $source (1+ $position))
            (char $source
              (incf $position))))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more whitespaces and returns no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (loop while (and $character (whitespace-character-p $character)) do
      (consume-character scanner)))
  (values))

;;; -------------------------------------------------------

(defun read-next-word (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more whitespaces, consumes the subsequent word,
   and returns the same as a string."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (the string
    (with-output-to-string (word)
      (declare (type string-stream word))
      (with-scanner (scanner)
        (loop while (and $character (word-character-p $character)) do
          (write-char (consume-character scanner) word))))))

;;; -------------------------------------------------------

(defun expect-word (scanner expected-word)
  "Consumes the next word from the SCANNER's source, contingently
   preceded by whitespaces, and determines whether the same matches the
   EXPECTED-WORD, on confirmation returning no value, otherwise issuing
   a signal of the type ``Unrecognized-Word-Condition''."
  (declare (type Scanner scanner))
  (declare (type string  expected-word))
  (if (string= (read-next-word scanner) expected-word)
    (skip-whitespaces scanner)
    (signal-unrecognized-word))
  (values))

;;; -------------------------------------------------------

(defun expect-character (scanner expected-character)
  "Determines whether the SCANNER's current character represents the
   EXPECTED-CHARACTER, on confirmation returning no value, otherwise
   issuing a signal of the type ``Unrecognized-Word-Condition''."
  (declare (type Scanner   scanner))
  (declare (type character expected-character))
  (with-scanner (scanner)
    (if (and $character (char= $character expected-character))
      (consume-character scanner)
      (signal-unrecognized-word)))
  (values))

;;; -------------------------------------------------------

(defun expect-digit (scanner)
  "Determines whether the SCANNER's current character represents a
   decimal digit, on confirmation returning no value, otherwise issuing
   a signal of the type ``Unrecognized-Word-Condition''."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (unless (and $character (digit-char-p $character))
      (signal-unrecognized-word)))
  (values))

;;; -------------------------------------------------------

(defun expect-amount (scanner)
  "Proceeding from the current position into the SCANNER's source,
   consumes and returns an unsigned real number in its verbatim string
   form, preceded by zero or more ignored whitespaces and aan imperative
   dollar sign, the same eludes its capture in the response object."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (expect-character scanner #\$)
  (expect-digit     scanner)
  (the string
    (with-output-to-string (amount)
      (declare (type string-stream amount))
      (with-scanner (scanner)
        ;; Read the integral portion.
        (loop while (and $character (digit-char-p $character)) do
          (write-char (consume-character scanner) amount))
        ;; Read the optional fractional segment.
        (when (and $character (char= $character #\.))
          (write-char (consume-character scanner) amount)
          (loop while (and $character (digit-char-p $character)) do
            (write-char (consume-character scanner) amount)))
        ;; Ascertain the amount token's termination on a word boundary.
        (unless (or (null $character)
                    (whitespace-character-p $character))
          (signal-unrecognized-word))))))

;;; -------------------------------------------------------

(defun expect-period (scanner)
  "Determines whether the SCANNER's current character represents a
   period (\".\"), contingently preceded by whitespaces, on confirmation
   returning no value, otherwise issuing a signal of the type
   ``Unrecognized-Word-Condition''."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (expect-character scanner #\.)
  (values))

;;; -------------------------------------------------------

(defun conclude-sentence (scanner)
  "Processes the coda of the SCANNER's current sentence, expected to
   follow a period (\".\"), by skipping zero or more trailing
   whitespaces, and returns no value."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (values))

;;; -------------------------------------------------------

(defun move-to-end-of-sentence (scanner)
  "Relocates the SCANNER's position cursor beyond the end of the current
   sentence, the limbation of which is designated by a period (\".\"),
   or the end of the source, if none such can be detected, in any case
   returning no value."
  (declare (type Scanner scanner))
  (with-scanner (scanner)
    (setf $position
      (or (position #\. $source :start $position :test #'char=)
          (length   $source)))
    (setf $character
      (when (array-in-bounds-p $source $position)
        (char $source $position)))
    ;; If a period (".") has been found, skip the same in order to
    ;; process a contingent next sentence.
    (when (and $character (char= $character #\.))
      (consume-character scanner)
      (conclude-sentence scanner)))
  (values))

;;; -------------------------------------------------------

(defun source-exhausted-p (scanner)
  "Determines whether the SCANNER's source is exhausted, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Scanner scanner))
  (the boolean
    (with-scanner (scanner)
      (null $character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-source (scanner)
  "Interprets the SCANNER's complete source and returns no value."
  (declare (type Scanner scanner))
  (loop until (source-exhausted-p scanner) do
    (handler-case
      (progn
        (expect-word scanner "I")
        (expect-word scanner "want")
        (expect-word scanner "to")
        (expect-word scanner "borrow")
        (let ((amount (expect-amount scanner)))
          (declare (type string amount))
          (expect-word        scanner "from")
          (expect-word        scanner "you")
          (expect-period      scanner)
          (conclude-sentence  scanner)
          (format T "~&$~a is too much." amount)))
      (Unrecognized-Word-Condition (condition)
        (format T "~&~a" condition)
        (move-to-end-of-sentence scanner))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Miser (code)
  "Interprets the piece of Miser source CODE and returns no value."
  (declare (type string code))
  (process-source
    (make-scanner code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate an integer amount.
(interpret-Miser "I want to borrow $10 from you.")

;;; -------------------------------------------------------

;; Demonstrate a floating-point amount.
(interpret-Miser "I want to borrow $100.59 from you.")

;;; -------------------------------------------------------

;; Demonstrate three valid requests in immediate succession.
(interpret-Miser
  "I want to borrow $10 from you.
   I want to borrow $500.005 from you.
   I want to borrow $0 from you.")

;;; -------------------------------------------------------

;; Demonstrate two valid requests, succeeded by an erroneous specimen,
;; and concluded by an admissible last.
(interpret-Miser
  "I want to borrow $10 from you.
   I want to borrow $500.005 from you.
   I want to borrow a kitten from you.
   I want to borrow $0 from you.")
