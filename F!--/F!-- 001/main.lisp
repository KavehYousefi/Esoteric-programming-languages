;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "F!--", invented by the Esolang user "None1" and presented
;; on August 1st, 2023, the entheus of which resides in the same
;; author's language "F!", itself a derivative of Jonathan Todd
;; Skinner's "Deadfish", while "F!--" replicates the latter's faculties
;; in a guise conflating with the former's donat.
;; 
;; 
;; Concept
;; =======
;; The F!-- language's provenance bifurcates into an amalgam of its
;; F! companion and the precedent Deadfish specimen, desuming from the
;; former the syntactical peculiarities, while the latter contributes
;; the operational foundry.
;; 
;; 
;; Architecture
;; ============
;; F!--'s cleronomy allocates an aefauld accumulator to whom an unsigned
;; byte's capacity, thus an integral range of [0, 255], is vouchsafed.
;; Initialized to the lower bourne of zero (0) at the program's
;; inchoation, upon any of its bournes' transgression, the state
;; automatically wraps around to ascertain the correct interval.
;; 
;; 
;; Data Type
;; =========
;; F!--'s type system is already exhausted by a single species'
;; participation: the unsigned octet covering the range [0, 255].
;; 
;; 
;; Syntax
;; ======
;; An F!-- program consists of zero or more instructions, each such a
;; twissel of a Latin majuscle and an concluding exphoneme, segregated
;; from its compernage by at least one whitespace. Unrecognized tokens
;; are simply ignored.
;; 
;; 
;; Instructions
;; ============
;; F!--'s instruction set intrines arithmetic facilities for the
;; singular byte accumulator's incrementation, deduction, and squaring,
;; and complements the membership to a tetrad by adminiculum of a single
;; output instrument capacitated to reproduce the salvatory's state in
;; its verbatim numeric format.
;; 
;; Tokens not subsumable into the recognized command roster are
;; encountered with a mete equipollent in its distribution of leniency
;; as the concomitant neglect.
;; 
;; == OVERVIEW ==
;; The following apercu shall educate about the language's operational
;; competences:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   F!      | Increments the accumulator by one (1). If the new value
;;           | exceeds the upper march of 255, the state is reset to
;;           | the minimum of zero (0).
;;   ..................................................................
;;   U!      | Decrements the accumulator by one (1). If the new value
;;           | descends below the lower march of zero (0), the state is
;;           | reset to the maximum of 255.
;;   ..................................................................
;;   C!      | Squares the accumulator value. If the new value exceeds
;;           | the upper march of 255, the state is reset to the
;;           | minimum of zero (0).
;;   ..................................................................
;;   K!      | Prints to the standard output the accumulator's value in
;;           | its numeric form.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp, directed at simplicity as its cynosure.
;; 
;; A kenspeckle, and aiblins kensback, attribute of its realization, the
;; surrogate roles for parameters have been assigned to special
;; variables, akin to globally active, but only locally accessible
;; references, entalenting the program with a clandestine alternative
;; for explicit global variables, while concomitantly curtailing the
;; function signatures.
;; 
;; Special variables share some characteristics of static variables in
;; the programming language C, enjoying a global extent in manners of
;; lifetime, but restricted in their visibility to select occasions that
;; require express injuction.
;; 
;; It constitutes a peisant element of gnarity to remember that special
;; variables, ligated into a consanguinity with global variables as a
;; general species, and exacerbated by their implicit and contingently
;; arbitrary declarations, merit the wite of encumbering programs with
;; superfluous complexity. For a more detailed treatise on the
;; contingency for detriments incurred by this feature please refer to
;; [stackoverflow2019q56725814].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-15
;; 
;; Sources:
;;   [esolang2023F!]
;;   The Esolang contributors, "F!", August 1st, 2023
;;   URL: "https://esolangs.org/wiki/F!"
;;   Notes:
;;     - Specification of the "F!" programming language, ligated to the
;;       subject by consanguinity.
;;   
;;   [esolang2023F!--]
;;   The Esolang contributors, "F!--", October 15th, 2023
;;   URL: "https://esolangs.org/wiki/F!--"
;;   
;;   [stackoverflow2012q41091118]
;;   The Stack Overflow contributors,
;;     "What's the canonical way to join strings in a list?", 2012
;;   URL: "https://stackoverflow.com/a/41091118"
;;   Notes:
;;     - Demonstrates the usance of special variables in the context of
;;       the ``format'' function.
;;   
;;   [stackoverflow2019q56725814]
;;   The Stack Overflow contributors, "Using Local Special Variables",
;;     2019
;;   URL: "https://stackoverflow.com/questions/56725814/
;;         using-local-special-variables"
;;   Notes:
;;     - Discusses the disadvantages of special variables, which
;;       comprehend:
;;        o Lack of referential transparency, ...
;;          ... which renders it more difficult to reason functionally
;;          about one's code, meaning that functions may produce
;;          different results with syntactically equivalent calls.
;;        o Introduction of bugs, ...
;;          ... as lexical variable at other locations in the code,
;;          e.g. in a system function, will be overwritten.
;;        o Confusion ...
;;          .. for readers unacquainted with special (dynamic) binding
;;        o Dubious necessity, ...
;;          ... as lexical binding or even anaphoric macros may be
;;          utilized instead.
;;   
;;   [tutorialspoint2023assemblyaddrmodes]
;;   The Tutorials Point contributors, "Assembly - Addressing Modes",
;;     2023
;;   URL: "https://www.tutorialspoint.com/assembly_programming/
;;         assembly_addressing_modes.htm"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command ()
  "The ``command'' type enumerates the recognized variation on F!--
   operations, admitting the ``:nop'' sentinel as a piece of
   supererogation no-operation tokens, and the advenient ``:eof'' member
   as the program exhaustion designator."
  '(member
    :increment
    :decrement
    :square
    :print
    :nop
    :eof))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight
   accolent bits, thus spanning the integral range [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of global bindings.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type  boolean                        source-exhausted-p))
(declaim (type  character                      current-character))

(declaim (ftype (function (string)    command)  get-command-for))
(declaim (ftype (function (character) boolean)  whitespace-character-p))
(declaim (ftype (function (character) boolean)  word-character-p))
(declaim (ftype (function ()          *)        skip-whitespaces))
(declaim (ftype (function ()          string)   read-word))
(declaim (ftype (function ()          command)  get-next-command))
(declaim (ftype (function (string)    *)        set-source))
(declaim (ftype (function (&optional string) *) interpret-F!--))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of command table.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-for (word)
  "Returns the command associated with the WORD, or the ``:nop''
   sentinel upon its disrespondency."
  (declare (type string word))
  (the command
    (or (and (string= word "F!") :increment)
        (and (string= word "U!") :decrement)
        (and (string= word "C!") :square)
        (and (string= word "K!") :print)
        :nop)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun word-character-p (candidate)
  "Determines whether the CANDIDATE represents a word constitutent,
   returning on confirmation a ``boolean'' value of ``T', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (whitespace-character-p candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-symbol-macro source-exhausted-p
  (locally
    (declare (special source))
    (declare (special location))
    (the boolean
      (not (array-in-bounds-p source location)))))

;;; -------------------------------------------------------

(define-symbol-macro current-character
  (locally
    (declare (special source))
    (declare (special location))
    (the character
      (char source location))))

;;; -------------------------------------------------------

(defun skip-whitespaces ()
  "Proceeding from the current LOCATION into the SOURCE, skips a
   sequence of zero or more accolent whitespaces and returns no value."
  (declare (special source))
  (declare (special location))
  (loop
    while (and (not source-exhausted-p)
               (whitespace-character-p current-character))
    do    (incf location))
  (values))

;;; -------------------------------------------------------

(defun read-word ()
  "Proceeding from the current LOCATION into the SOURCE, reads a word,
   delimited by a whitespace or the SOURCE's exhaustion, and returns its
   content as a string."
  (declare (special source))
  (declare (special location))
  (the string
    (with-output-to-string (word)
      (declare (type string-stream word))
      (loop
        while (and (not source-exhausted-p)
                   (word-character-p current-character))
        do    (write-char current-character word)
              (incf location)))))

;;; -------------------------------------------------------

(defun get-next-command ()
  "Returns the next command from the SOURCE, while advancing its
   LOCATION cursor.
   ---
   Upon the SOURCE's exhaustion, the ``:eof'' sentinel will be
   produced."
  (declare (special source))
  (declare (special location))
  (skip-whitespaces)
  (the command
    (if source-exhausted-p
      :eof
      (get-command-for
        (read-word)))))

;;; -------------------------------------------------------

(defun set-source (new-source)
  "Sets the global SOURCE to the NEW-SOURCE, resets the LOCATION cursor,
   and returns no value."
  (declare (special source))
  (declare (special location))
  (declare (type string new-source))
  (setf source   new-source)
  (setf location 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-F!-- (&optional (initial-program ""
                                  initial-program-supplied-p))
  "Executes the F!-- interpreter, contingently evaluating the
   INITIAL-PROGRAM as its inchoate source, and repeates the process
   until a completely empty line is committed, finally returning no
   value."
  (declare (type string  initial-program))
  (declare (type T       initial-program-supplied-p))
  
  (prog ((source          initial-program)
         (location        0)
         (accumulator     0)
         (current-command :nop))
    (declare (special      source))
    (declare (type string  source))
    (declare (special      location))
    (declare (type fixnum  location))
    (declare (type octet   accumulator))
    (declare (type command current-command))
    
    process-initial-program
      (if initial-program-supplied-p
        (go read-next-command)
        (go query-for-input))
    
    query-for-input
      (format T "~&>> ")
      (finish-output)
      (set-source (read-line))
      (when (zerop (length source))
        (go away))
      (clear-input)
    
    read-next-command
      (setf current-command (get-next-command))
      (case current-command
        (:increment (go F!))
        (:decrement (go U!))
        (:square    (go C!))
        (:print     (go K!))
        (:nop       (go read-next-command))
        (:eof       (go query-for-input))
        (otherwise  (go anomaly)))
    
    F!
      (setf accumulator (mod (1+ accumulator) 256))
      (go   read-next-command)
    
    U!
      (setf accumulator (mod (1- accumulator) 256))
      (go   read-next-command)
    
    C!
      (setf accumulator (mod (* accumulator accumulator) 256))
      (go   read-next-command)
    
    K!
      (format T "~&~d~%" accumulator)
      (go read-next-command)
    
    anomaly
      (error "Unrecognized command: ~s." current-command)
    
    away
      NIL)
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate the wrapping behavior of the accumulator by reducing its
;; value twice from the default of zero, thus accomplishing the result
;; 254.
(interpret-F!-- "U! U! K!")

;;; -------------------------------------------------------

;; Reproduce the "XKCD random number", the integer 4, and print the
;; same.
(interpret-F!-- "F! F! F! F! K!")
