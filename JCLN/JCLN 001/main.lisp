;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "JCLN", invented by the Esolang user "A" and presented on
;; April 30th, 2019, its haecceity's emergence defined by the
;; juxtaposition of the current instruction line's numeric antecedent
;; with a line counter, and the potential for a consequential relocation
;; to the prescribed destination line.
;; 
;; 
;; Concept
;; =======
;; The JCLN programming language subsumes into the category of the
;; "One-Instruction Set Computer" (OISC), its entire set of competences
;; registering in its provenance a single expression whose multifarious
;; parameter options beget the distinct behaviors --- in this particular
;; case comprehending merely the probing of an instruction with the
;; current line number and a conditional or unconditional jumping to
;; another line as the docimasy's ultimity.
;; 
;; == JCLN: [J]UMP [C]ONDITIONALLY TO A [L]I[N]E ==
;; The JCLN programming language's stevening already serves in the
;; bewrayment of its purpose, this quadruple letter composite founding
;; an abbreviation of "Jump Conditionally to a LiNe".
;; 
;; == JCLN: AN AEFAULD INSTRUCTION AS A REDIRECTION CONTROL ==
;; Its status as an OISC conditions a singular species of instruction
;; expression's gendrure:
;; 
;;   jcln guardLineNumber, destinationLineNumber
;;        ***************  *********************
;; 
;; The {guardLineNumber} stipulates the instruction's activation
;; antecedent; if the interpreter's current line number can be
;; equiparated with this guard, the instruction pointer (IP) is
;; redirected to the {destinationLineNumber}. Otherwise the instruction
;; pointer simply advances to the subsequent line.
;; 
;; The value -1 as the {guardLineNumber} applies itself to a sentinel
;; role, as its employment always signifies the probed line's
;; eligibility, hence installing in this aspect a "wildcard" line
;; number.
;; 
;; An explication entalented with a superior mete of formality shall
;; address the operative bailiwick in a pseudocode formulation.
;; 
;; As a warklume of parasceuastic value, the following definitions will
;; install a cursory acquaintance's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Identifier           | Role
;;   ---------------------+--------------------------------------------
;;   length (program)     | The tally of instruction lines comprising
;;                        | the {program}.
;;   ..................................................................
;;   program (lineNumber) | The line at the one-based {lineNumber}
;;                        | comprising the JCLN program.
;;   ..................................................................
;;   guard (line)         | The line number of the {line} serving as a
;;                        | guard: If the currently processed line
;;                        | line number matches thilk, the {line}'s
;;                        | destination index shall serve as the new
;;                        | instruction pointer (IP) location.
;;   ..................................................................
;;   destination (line)   | The line number to locate the instruction
;;                        | pointer to if the probed {line}'s guard
;;                        | matches the currently processed line
;;                        | number.
;;   ------------------------------------------------------------------
;; 
;; Proceeding from the aboon diorisms' establishments, the following
;; pseudocode delineation applies to a JCLN program's evaluation:
;; 
;;   let numberOfProgramLines <- length (program)
;;   let currentLineNumber    <- 1
;;   
;;   while currentLineNumber <= numberOfProgramLines do
;;     let currentInstruction <- program (currentLineNumber)
;;     let lineGuard          <- guard (currentInstruction)
;;     let currentLineMatches <- false
;;     
;;     if ((lineGuard = -1) or (currentLineNumber = lineGuard)) then
;;       currentLineMatches <- true
;;     else
;;       currentLineMatches <- false
;;     end if
;;     
;;     if currentLineMatches then
;;       currentLineNumber <- destination (currentInstruction)
;;     else
;;       currentLineNumber <- currentLineNumber + 1
;;     end if
;;     
;;     if currentLineNumber is no valid position then
;;       terminate program
;;     end
;;   end while
;; 
;; == NO DATA, NO ARCHITECTURE ==
;; The simplicity's woning in the language deprives its necessary
;; investments from any designment of architecture of data types.
;; 
;; 
;; Syntax
;; ======
;; A homogeneity of patration's most kenspeckle tier governs the JCLN
;; programming language's syntactical aspect, its programs being
;; distributed along line, with each non-blank row's parasceve
;; manifested in the "jcln" keyword, whence follows the signed integer
;; guard line number, segregated by an aefauld comma's mediation from
;; the destinatino line index.
;; 
;; == PROGRAMS: LINES OF ZERO OR ONE INSTRUCTION ==
;; A JCLN program's composition derives from an ordered sequence of
;; zero or more lines, everichon among these a comomrancy to at most
;; one instruction.
;; 
;; == INSTRUCTIONS: ADHERENTS TO A STRICT FORBISEN ==
;; The sole forbisen adhibited tolerance follows the delination of the
;; introducing "jcln" keyword, ere a decimal integer as the antecedent
;; or guard partakes of its specification, segregated from the
;; consequent destination line number --- iterum as scion of the signed
;; or unsigned integer species --- by exactly one comma (",").
;; 
;; == COMMENTS ==
;; A provision for comments resides in the language's dation, its
;; introduction affiliated with the hash sign, "#", and its dispansion
;; advancing to the end of the accommodating line.
;; 
;; == GRAMMAR ==
;; An Extended Backus-Naur Form (EBNF) description of the JCLN
;; programming shall furnish a more stringent elucidation:
;; 
;;   program     := { innerLine } , [ lastLine ] ;
;;   innerLine   := lineContent , newlines ;
;;   lastLine    := lineContent ;
;;   lineContent := [ command ] , [ comment ] ;
;;   command     := "jcln" , lineNumber , "," , lineNumber ;
;;   comment     := "#" , { character - newline } ;
;;   lineNumber  := [ "+" | "-" ] , digit , { digit } ;
;;   newlines    := newline , { newline } ;
;;   newline     := "\n" ;
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, the adhibition of effect from the
;; source code string to the executing entity amplecting an intermediate
;; conversion stage into a vector of instructions, thilk admit in their
;; compass the expected line number and the ensuing destination line.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-03
;; 
;; Source:
;;   [esolang2024JCLN]
;;   The Esolang contributors, "JCLN", April 1st, 2024
;;   URL: "https://esolangs.org/wiki/JCLN"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype progress-listener ()
  "The ``progress-listener'' type defines a functional object dedicated
   to the notification about a JCLN program's interpretation progress,
   in this agency receiving a ``Program-Step'', while returning an
   arbitrary, ultimately ignored, response.
   ---
   As a corollary, the function signature conforms with:
     function (Program-Step) => ignored-result"
  '(function (Program-Step) *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Produces a veridicous Boolean tantamount of the OBJECT construed as
   a \"generalized boolean\", returning for a non-``NIL'' input a
   ``boolean'' value of ``T''; otherwise responding with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' class furnishes an encapsulation of a JCLN
   operation, its componency's membership laying its amplectation around
   the definition line number, the activating line number, and the
   destination inspired by the previent constituent's affirmation."
  (definition-line-number  0 :type (integer 0 *) :read-only T)
  (guard-line-number       0 :type (integer * *) :read-only T)
  (destination-line-number 0 :type (integer * *) :read-only T))

;;; -------------------------------------------------------

(defun instruction-line-matches-p (instruction probed-line-number)
  "Determines whether the INSTRUCTION's line in the program matches
   the PROBED-LINE-NUMBER, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (declare (type integer     probed-line-number))
  (the boolean
    (get-boolean-value-of
      (or (= probed-line-number -1)
          (= probed-line-number
             (instruction-guard-line-number instruction))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun integer-character-p (candidate)
  "Determines whether the CANDIDATE represents a character homologated
   to partake in a signed or unsigned decimal integer literal, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (digit-char-p candidate)
          (char=        candidate #\+)
          (char=        candidate #\-)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces or horizontal tabs and returns the
   index into the SOURCE immediately succeeding the omitted parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun substring-starts-at-p (source desideratum start)
  "Determines whether the substring DESIDERATUM commences in the
   SOURCE at the START position, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type string desideratum))
  (declare (type fixnum start))
  (the boolean
    (get-boolean-value-of
      (string= source desideratum
        :start1 start
        :end1   (min (+ start (length desideratum))
                     (length source))))))

;;; -------------------------------------------------------

(defun expect-substring (source desideratum start)
  "Proceeding from the START position into the SOURCE, determines
   whether the DESIDERATUM commences, returning on confirmation the
   index immediately succeeding the matching segment; otherwise signals
   an error of an unspecified type."
  (declare (type string source))
  (declare (type string desideratum))
  (declare (type fixnum start))
  (the fixnum
    (or (and (substring-starts-at-p source desideratum start)
             (+ start (length desideratum)))
        (error "Expected the substring \"~a\" to occur at the ~
                position ~d."
          desideratum start))))

;;; -------------------------------------------------------

(defun expect-jcln-keyword (source start)
  "Proceeding from the START position into the SOURCE, determines
   whether the keyword \"jcln\" follows, returning on confirmation the
   index immediately succeeding the matched keyword; otherwise signals
   an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (expect-substring source "jcln" start)))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character located at the POSITION into the SOURCE, or
   responds with ``NIL'' upon the index' transgression of the valid
   marches."
  (declare (type string source))
  (declare (type fixnum position))
  (the (or null character)
    (when (array-in-bounds-p source position)
      (char source position))))

;;; -------------------------------------------------------

(defun expect-spaces (source start)
  "Proceeding from the START position into the SOURCE, determines
   whether a catena compact of one or more accolent spaces ensues,
   returning on confirmation the index immediately succeeding the
   matching tmema; otherwise signals an error of an unspecified type."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-character (get-character-at source start)))
    (declare (type (or null character) current-character))
    (the fixnum
      (cond
        ((null current-character)
          (error "Expected one or more spaces commencing at ~
                  position ~d, but found the source exhausted."
            start))
        ((not (space-character-p current-character))
          (error "Expected one or more spaces commencing at ~
                  position ~d, but encountered the character \"~c\"."
            start current-character))
        (T
          (skip-spaces source start))))))

;;; -------------------------------------------------------

(defun expect-comma (source position)
  "Determines whether the character at the POSITION into the SOURCE
   represents a comma (\",\"), on confirmation returning the index
   immediately succeeding thilk; otherwise an error of an unspecified
   type is signaled."
  (declare (type string source))
  (declare (type fixnum position))
  (let ((current-character (get-character-at source position)))
    (declare (type (or null character) current-character))
    (the fixnum
      (cond
        ((null current-character)
          (error "Expected a comma (\",\") at position ~d,
                  position but found the source exhausted."
            position))
        ((char/= current-character #\,)
          (error "Expected a comma (\",\") at position ~d, ~
                  but encountered the character \"~c\"."
            position current-character))
        (T
          (1+ position))))))

;;; -------------------------------------------------------

(defun locate-end-of-integer (source start)
  "Proceeding from the START position into the SOURCE, returns the
   index of the first character not representing a constituent in a
   signed or unsigned decimal integer literal."
  (the fixnum
    (or (position-if-not #'integer-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-line-number (source start)
  "Proceeding from the START position into the SOURCE, reads a signed
   or unsigned decimal integer literal and returns two values:
     (1) The parsed integer number.
     (2) The position into the SOURCE immediately succeeding the
         detected number."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values integer fixnum)
    (parse-integer source
      :start start
      :end   (locate-end-of-integer source start))))

;;; -------------------------------------------------------

(defun invalid-position-in-string (source probed-position)
  "Determines whether the PROBED-POSITION transcends the SOURCE's
   valid bournes, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum probed-position))
  (the boolean
    (not (array-in-bounds-p source probed-position))))

;;; -------------------------------------------------------

(defun comment-starts-p (source position)
  "Determines whether a line comment, introduced via the \"#\" symbol,
   commences at the POSITION into the SOURCE, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (get-boolean-value-of
      (and (array-in-bounds-p source position)
           (char= (char source position) #\#)))))

;;; -------------------------------------------------------

(defun end-of-line-p (source start)
  "Proceeding from the START position into the SOURCE, determines
   whether any effective content's absence is delivered to attestation,
   with spaces and a comment tallying among the homologated members,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-position (skip-spaces source start)))
    (declare (type fixnum current-position))
    (the boolean
      (or (invalid-position-in-string source current-position)
          (comment-starts-p           source current-position)))))

;;; -------------------------------------------------------

(defun expect-end-of-line (source start)
  "Proceeding from the START position into the SOURCE, determines
   whether any effective content's absence is delivered to attestation,
   with spaces and a comment tallying among the homologated members,
   returning on confirmation no value; otherwise an error of an
   unspecified type is signaled."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((current-position (skip-spaces source start)))
    (declare (type fixnum current-position))
    (unless (end-of-line-p source current-position)
      (error "Expected the line to conclude, but encountered the ~
              character \"~c\" at position ~d."
        (char source current-position) current-position)))
  (values))

;;; -------------------------------------------------------

(defun parse-line (current-line-number source)
  "Parses the SOURCE line, located at the one-based CURRENT-LINE-NUMBER,
   and either returns an ``Instruction'' repreesentation of its
   ensconced operation, if not blank or merely commentary; otherwise
   produces the ``NIL'' value."
  (declare (type (integer 1 *) current-line-number))
  (declare (type string        source))
  (let ((current-position (skip-spaces source 0)))
    (declare (type fixnum current-position))
    (the (or null Instruction)
      (unless (end-of-line-p source current-position)
        (setf current-position
          (expect-spaces source
            (expect-jcln-keyword source current-position)))
        (let ((guard-line-number       0)
              (destination-line-number 0))
          (declare (type integer guard-line-number))
          (declare (type integer destination-line-number))
          ;; Read the guard line number.
          (setf (values guard-line-number current-position)
            (read-line-number source current-position))
          ;; Read a comma, ...
          (setf current-position
            (skip-spaces source
              (expect-comma source
                (skip-spaces source current-position))))
          ;; ... succeeded by the destination line number.
          (setf (values destination-line-number current-position)
            (read-line-number source current-position))
          ;; No further non-comment content may ensue.
          (expect-end-of-line source current-position)
          (make-instruction
            :definition-line-number  current-line-number
            :guard-line-number       guard-line-number
            :destination-line-number destination-line-number))))))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Parses the piece of JCLN SOURCE code and returns a one-dimensional
   simple array of its comprehended instructions."
  (declare (type string source))
  (with-input-from-string (input-stream source)
    (declare (type string-stream input-stream))
    (coerce
      (loop
        for current-line
          of-type (or null string)
          =       (read-line input-stream NIL NIL)
        with current-line-number
          of-type (integer 1 *)
          =       1
        while current-line append
          (let ((current-instruction
                  (parse-line current-line-number current-line)))
            (declare (type (or null Instruction) current-instruction))
            (when current-instruction
              (incf current-line-number)
              (list current-instruction))))
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Program-Step".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program-Step
  (:constructor make-program-step (prevenient-line-number
                                   current-line-number
                                   current-instruction
                                   has-jumped-p)))
  "The ``Program-Step'' class serves in the encapsulation of a JCLN
   interpretation process' progress, pursuing its deployment in the
   notification of a ``progress-listener'' instance."
  (prevenient-line-number (error "Missing prevenient line number.")
                          :type      (integer 0 *)
                          :read-only T)
  (current-line-number    (error "Missing current line number.")
                          :type      (integer 0 *)
                          :read-only T)
  (current-instruction    (error "Missing current line's instruction.")
                          :type      (or null Instruction)
                          :read-only T)
  (has-jumped-p           (error "Missing jump flag.")
                          :type      boolean
                          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +DEFAULT-PROGRESS-LISTENER+
  #'(lambda (step)
      (declare (type Program-Step step))
      (format T "~&~:[Advanced~;Jumped~] from line no. ~d to the ~
                   current line ~d containing ~
                   ~:[no instruction~;the instruction ~:*~a~]."
        (program-step-has-jumped-p           step)
        (program-step-prevenient-line-number step)
        (program-step-current-line-number    step)
        (program-step-current-instruction    step))
      (values))
  "Defines the default progress listener, utilized during a JCLN
   program's interpretation as a means of communicating the
   advancement.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-program
    (program
     &optional (listener +DEFAULT-PROGRESS-LISTENER+))
  "Interprets the JCLN PROGRAM, utilizing the LISTENER function for
   communicating notifications anent the progress, and returns no
   value."
  (declare (type (simple-array Instruction (*)) program))
  (declare (type progress-listener              listener))
  (let ((ip                   0)
        (previous-line-number 0))
    (declare (type integer ip))
    (declare (type integer previous-line-number))
    (symbol-macrolet
        ((current-instruction
          (the Instruction
            (aref program ip)))
         (current-line-number
          (the integer
            (1+ ip)))
         (current-instruction-matches-p
          (the boolean
            (instruction-line-matches-p
              current-instruction
              current-line-number)))
         (program-completed-p
          (the boolean
            (get-boolean-value-of
              (>= ip (length program))))))
      (declare (type Instruction current-instruction))
      (declare (type integer     current-line-number))
      (declare (type boolean     current-instruction-matches-p))
      (declare (type boolean     program-completed-p))
      (flet ((notify-progess-listener (has-jumped-p)
              "Invokes the LISTENER function employing the current
               program state in conjunction with the HAS-JUMPED-P flag
               and returns no value."
              (declare (type boolean has-jumped-p))
              (funcall listener
                (make-program-step
                  previous-line-number
                  current-line-number
                  (and (not program-completed-p)
                       current-instruction)
                  has-jumped-p))
              (values)))
        (loop
          initially
            (unless program-completed-p
              (notify-progess-listener NIL))
          until program-completed-p do
            (setf previous-line-number current-line-number)
            (cond
              (current-instruction-matches-p
                (setf ip
                  (1- (instruction-destination-line-number
                      current-instruction)))
                (notify-progess-listener T))
              (T
                (incf ip)
                (notify-progess-listener NIL)))))))
  (values))

;;; -------------------------------------------------------

(defun interpret-JCLN (code
                       &optional (listener +DEFAULT-PROGRESS-LISTENER+))
  "Interprets the piece of JCLN source CODE, , utilizing the LISTENER
   function for communicating notifications anent the progress, and
   returns no value."
  (declare (type string            code))
  (declare (type progress-listener listener))
  (execute-program
    (parse-program code)
    listener)
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate jumping and normal advancing of the instruction pointer
;; (IP).
(interpret-JCLN
  "jcln   1, 2
   jcln   2, 3
   jcln 100, 4
   jcln   4, 5"
  +DEFAULT-PROGRESS-LISTENER+)

;;; -------------------------------------------------------

;; Only navigate to odd-numbered lines: 1, 3, 5.
(interpret-JCLN
  "jcln 1, 3
   jcln 2, 0
   jcln 3, 5
   jcln 4, 0"
  #'(lambda (step)
      (declare (type Program-Step step))
      (format T "~&~d"
        (program-step-current-line-number step))
      (values)))
