;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Grounded", invented by the Esolang user "Cinnamony" and
;; presented on June 19th, 2023, the subject of whose operations
;; relates to a bairn's confinement inside of their own room by their
;; parents' punitive action, which imposes upon the tholing wight an
;; hourly engagement in activities in order to pass the wite of at least
;; three days, each such diurnal dispansion commencing at 07:30 AM and
;; concluding at 09:30 PM.
;; 
;; 
;; Concept
;; =======
;; The Grounded programming language's telos comprises the expression of
;; its programs in the form of a person's hourly routines in order to
;; pass the time encumbering them with a parental confinement to the
;; personal chamber.
;; 
;; == GROUNDED PROGRAMS ARE ARRANGED IN LINES ==
;; A Grounded program's conformation proceeds by means of lines, each
;; such, if not vacant, at most a single instruction's woning. A comment
;; may be introduced at any position by two accolent slashes' ("//")
;; mediation, in which case the descant extends until the row's
;; termination.
;; 
;; == INSTRUCTIONS ENUMERATE ACTIVITIES ==
;; The operational identifiers serve in the description of activities
;; that tally among the chronophagous investments in the pursuit of the
;; temporal impositions' alleviation.
;; 
;; == BASIC PRINCIPLES ==
;; A Grounded program is subservient to certain stipulations:
;; 
;;   (1) THE PROGRAM STARTS ON A FRIDAY, AT 07:30 O'CLOCK
;;       The confinement's inchoation concurs with a Friday,
;;       established, siclike to any day's commencement, at
;;       07:30 o'clock.
;;   
;;   (2) EVERY DAY IS DIVIDED INTO HOURLY ACTIVITIES
;;       Each day's division, commencing at 07:30 o'clock and
;;       concluding at 21:30 o'clock, forms an hourly arrangement,
;;       where every single hour ought to be associated with exactly
;;       one activity.
;;   
;;   (3) A DAY MUST CONCLUDE WITH A "SLEEP" AT 21:30 O'CLOCK
;;       Any day, including the desinent one, must conclude with a
;;       "sleep" command invocation at exactly 21:30 o'clock;
;;       homologating no other command at this temporal point, nor
;;       a deviation in the instruction assigned to this occasion.
;;   
;;   (4) THE DEFAULT CONFINEMENT OF THREE DAYS MAY BE EXTENDED
;;       The confinement's default duration, tallying the three days
;;       from inclusive Friday, starting at 07:30 o'clock, to inclusive
;;       Sunday, concluding at 21:30 o'clock, may be extended at any
;;       instant which homologates an arbitrary instruction by aide of
;;       the "bother parents" behest. This adscititious dispansion
;;       incurs a further day upon the extant imposition, introducing
;;       the capacity for fifteen additional activities, ranging iterum
;;       in the time betwixt 07:30 o'clock and 21:30 o'clock, including
;;       the mandatory "sleep" coda.
;; 
;; == SUPERNUMERARY ACTIVITIES ARE IGNORED, LACUNAE ARE CRITICAL ==
;; An essential mete of gravity applies to the diurnal activities'
;; presence, bifurcating into discrepancies in the response to
;; surfeiture and indigence.
;; 
;; Supererogatory elements' participation does not encumber the system
;; with a negative response, yielding an ultimity tantamount in its
;; grade to neglect.
;; 
;; The abstinence from a sufficiency in the activities' provision, on
;; the other hand, inflicts the program with a critical flaw, the same
;; in its peisant causatum aborts with an error of the type
;; "UninspiredError". 
;; 
;; == SPECIAL EVENTS ==
;; A certain set of fixated events' involvement permits or requires
;; special attendance, the same shall be the following tabular
;; exposition's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Time  | Day      | Activity
;;   ------+----------+------------------------------------------------
;;   07:30 | Every    | A new day commences. The program starts on
;;         |          | Friday at this hour.
;;   ..................................................................
;;   12:30 | Saturday | One may use the command "tech lab" in order to
;;         |          | squander a single hour, concomitant to which
;;         |          | holds no epiphenomenon.
;;   ..................................................................
;;   21:30 | Every    | Sleeping time; the "sleep" command is required
;;         |          | to conclude the day.
;;   ------------------------------------------------------------------
;; 
;; == THE PROGRAM MEMORY: A BILATERALLY INFINITE TAPE OF INTEGERS ==
;; The language operates on a tape compact of signed integer cells,
;; infinite in their extent along both axes, upon whom also the
;; amenability to a pointer is bestowed, such selects at any instant the
;; currently active cell, the entity solely admissive to perquisitions
;; and manipulations.
;; 
;; == GROUNDED EMPLOYS A BESPOKE CHARACTER REPERTOIRE ==
;; The Grounded programming language's contingency for the internal as
;; well as external maintenance of characters constitutes a very
;; restricted ilk, desumed from a subset of ASCII's, such permits merely
;; the case-insensitive form of Latin letters to be appropriated.
;; 
;; The following character code map to the avail of symbols. Please note
;; that, under the governance of the canonical, yet not mandative
;; design, only the majuscular letter's reproduction transpires:
;; 
;;   --------------------------
;;   Character code | Character
;;   ---------------+----------
;;   1              | A
;;   ..........................
;;   2              | B
;;   ..........................
;;   3              | C
;;   ..........................
;;   4              | D
;;   ..........................
;;   5              | E
;;   ..........................
;;   6              | F
;;   ..........................
;;   7              | G
;;   ..........................
;;   8              | H
;;   ..........................
;;   9              | I
;;   ..........................
;;   10             | J
;;   ..........................
;;   11             | K
;;   ..........................
;;   12             | L
;;   ..........................
;;   13             | M
;;   ..........................
;;   14             | N
;;   ..........................
;;   15             | O
;;   ..........................
;;   16             | P
;;   ..........................
;;   17             | Q
;;   ..........................
;;   18             | R
;;   ..........................
;;   19             | S
;;   ..........................
;;   20             | T
;;   ..........................
;;   21             | U
;;   ..........................
;;   22             | V
;;   ..........................
;;   23             | W
;;   ..........................
;;   24             | X
;;   ..........................
;;   25             | Y
;;   ..........................
;;   26             | Z
;;   --------------------------
;; 
;; 
;; Instructions
;; ============
;; A duodecimal cardinality's governance is exercised on the Grounded
;; language's instruction set, such capacitates the program memory's
;; navigation, manipulation, the confinement's prolongation, as well as
;; input and output facilities.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a cursory gnarity's attendance
;; in relations to the language's operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command                   | Effect
;;   --------------------------+---------------------------------------
;;   watch steven universe     | Moves the cell pointer one step to the
;;                             | right.
;;   ..................................................................
;;   watch adventure time      | Moves the cell pointer one step to the
;;                             | left.
;;   ..................................................................
;;   make plushie movie        | Increments the current cell value by
;;                             | one (1).
;;   ..................................................................
;;   make plushie movie series | Increments the current cell value by
;;                             | ten (10).
;;   ..................................................................
;;   uninstall app on tablet   | Decrements the current cell value by
;;                             | one (1).
;;   ..................................................................
;;   daydream                  | Multiplies the current cell value by
;;                             | itself.
;;   ..................................................................
;;   read                      | Queries the standard input for a
;;                             | character and stores its Grounded
;;                             | character code in the current cell.
;;                             |---------------------------------------
;;                             | If the input character violates the
;;                             | potentials of the bespoke Grounded
;;                             | character repertoire, an error of the
;;                             | type "CharacterEncodingError" is
;;                             | signaled.
;;                             |---------------------------------------
;;                             | Please consult the subsection
;;                             | "GROUNDED EMPLOYS A BESPOKE CHARACTER
;;                             | REPERTOIRE", subsumed in the section
;;                             | "Concept", for further intelligence.
;;   ..................................................................
;;   make book                 | Prints the character whose Grounded
;;                             | character code corresponds to the
;;                             | current cell value to the standard
;;                             | output.
;;                             |---------------------------------------
;;                             | If the cell value violates the code
;;                             | range imposed by Grounded's bespoke
;;                             | character repertoire, the value is
;;                             | wrapped around into the admissible
;;                             | interval prior to the transcription
;;                             | into a symbolic form.
;;                             |---------------------------------------
;;                             | Please consult the subsection
;;                             | "GROUNDED EMPLOYS A BESPOKE CHARACTER
;;                             | REPERTOIRE", subsumed in the section
;;                             | "Concept", for further intelligence.
;;   ..................................................................
;;   tech lab                  | Exercises no causatum, except for the
;;                             | common appropriation of one hour's
;;                             | time.
;;                             |---------------------------------------
;;                             | This command is to own's avail only on
;;                             | Saturdays at 12:30 o'clock.
;;                             |---------------------------------------
;;                             | If this command is invoked on an
;;                             | inappropriate day or at an unexpected
;;                             | time, an error of the type
;;                             | "LabClosedError" is signaled.
;;   ..................................................................
;;   wait                      | Exercises no causatum, except for the
;;                             | common appropriation of one hour's
;;                             | time.
;;                             |---------------------------------------
;;                             | This command must always be stated
;;                             | twice in immediate succession.
;;                             |---------------------------------------
;;                             | If this command does not appear as a
;;                             | jumelle with its paregal, an error of
;;                             | the type "Impatience-Error" is
;;                             | signaled.
;;   ..................................................................
;;   bother parents            | Adds a further day to the confinement,
;;                             | which imposes a further 15 commands'
;;                             | appendage to the program, including
;;                             | the terminating "sleep" behest.
;;   ..................................................................
;;   sleep                     | Concludes the current day, and, if
;;                             | the confinement yet perpetuates,
;;                             | continues with the next one, resetting
;;                             | the time to 07:30 o'clock.
;;                             |---------------------------------------
;;                             | This command must be invoked on every
;;                             | day, including the desinent one, at
;;                             | 21:00 o'clock.
;;                             |---------------------------------------
;;                             | If this command is invoked at any
;;                             | other instant than 21:30 o'clock, an
;;                             | error of the type "SomnolenceError" is
;;                             | signaled.
;;                             |---------------------------------------
;;                             | If any other command is invoked at
;;                             | 21:30 o'clock, an error of the type
;;                             | "InsomniacError" is signaled.
;;   ..................................................................
;; 
;; 
;; Implementation
;; ==============
;; This project's implementation manifests in the programming language
;; Common Lisp, the extraction of the Grounded commands proceeding in an
;; immediate fashion from the source code string, as a consequence of
;; its prior segregation into lines.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-27
;; 
;; Sources:
;;   [esolang2023Grounded]
;;   The Esolang contributors, "Grounded", September 18th, 2023
;;   URL: "https://esolangs.org/wiki/Grounded"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of hash table operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-hash-table-entry-satisfies-p (predicate candidate)
  "Determines whether every entry of the CANDIDATE hash table satisfies
   the dyadic PREDICATE, applied to the key and value in this order,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type (function (* *) *) predicate))
  (declare (type hash-table         candidate))
  (the boolean
    (not (null
      (loop
        for    key of-type T being the hash-keys in candidate
        using  (hash-value value)
        always (funcall predicate key value))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or elements,
   each member of which conforms to the ELEMENT-TYPE, the same defaults
   to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type T)
                                        (value-type     T))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each such represented by a
   cons, the sinistral compartment of which entails an object of the
   INDICATOR-TYPE, while the dextral moeity assumes the VALUE-TYPE, for
   both of which holds the default ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element
                    `(cons ,indicator-type ,value-type)))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (every-hash-table-entry-satisfies-p
              #'(lambda (key value)
                  (declare (type T key))
                  (declare (type T value))
                  (and (typep key   key-type)
                       (typep value value-type)))
              (the hash-table candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized variants on
   Grounded operations."
  '(member
    :sleep
    :bother-parents
    :tech-lab
    :wait
    :read
    :watch-steven-universe
    :watch-adventure-time
    :make-book
    :make-plushie-movie
    :make-plushie-movie-series
    :uninstall-app-on-tablet
    :daydream))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a Grounded program as an ordered list
   composed of zero or more ``instruction'' instances."
  '(simple-array instruction (*)))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of cells, amenable
   to signed integer indices which respond to elements desumed from the
   same space as a hash table composed of zero or more entries, each key
   of which contributes an ``integer'' index and allies with an
   ``integer'' value."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype day ()
  "The ``day'' type enumerates the weekday identifiers."
  '(member
    :friday
    :saturday
    :sunday
    :monday
    :tuesday
    :wednesday
    :thursday))

;;; -------------------------------------------------------

(deftype 12-hour-time ()
  "The ``12-hour-time'' defines an hour in the 12-hour temporal format
   as a positive integral value commorant in the range [1, 12]."
  '(integer 1 12))

;;; -------------------------------------------------------

(deftype 24-hour-time ()
  "The ``24-hour-time'' defines an hour in the 24-hour temporal format
   as a non-negative integral value commorant in the range [0, 23]."
  '(integer 0 23))

;;; -------------------------------------------------------

(deftype time-period ()
  "The ``time-perod'' type enumerates the valid specimens of time
   periods governing the 12-hour time convention."
  '(member :am :pm))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which lays its amplectation around siccan functions as
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype prefix-parameters ()
  "The ``prefix-parameters'' type defines prefix parameters pursuant to
   the ``format'' and ``error'' directives as a list composed of zero or
   more character or integer elements."
  '(list-of (or character integer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a ``boolean'' representation of the OBJECT, which amounts to
   ``T'' for a non-``NIL'' input, and to ``NIL'' otherwise."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE belong to the species of space
   characters, a category which comprehends the twain of spaces and
   horizontal tabs, returning on confirmation a ``boolean'' value of
   ``T'', otherwise responding with ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-strings (words)
  "Concatenates the WORDS into a single string, each twain conjoined by
   a single space, and returns the thus resulting object."
  (declare (type (list-of string) words))
  (the string
    (format NIL "~{~a~^ ~}" words)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of array operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-program (instructions)
  "Returns a representation of the INSTRUCTIONS as a ``program'', a
   paregal to a one-dimensional simple array of ``instruction''
   elements."
  (declare (type (list-of instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of identifier table.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of string instruction) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("sleep"                     . :sleep)
    ("bother parents"            . :bother-parents)
    ("tech lab"                  . :tech-lab)
    ("wait"                      . :wait)
    ("read"                      . :read)
    ("watch steven universe"     . :watch-steven-universe)
    ("watch adventure time"      . :watch-adventure-time)
    ("make book"                 . :make-book)
    ("make plushie movie"        . :make-plushie-movie)
    ("make plushie movie series" . :make-plushie-movie-series)
    ("uninstall app on tablet"   . :uninstall-app-on-tablet)
    ("daydream"                  . :daydream))
  "Associates the recognized command identifiers in their canonical form
   to instruction keywords.")

;;; -------------------------------------------------------

(defun parse-instruction (identifier)
  "Returns an ``instruction'' representation of the IDENTIFIER, or, upon
   its disrespondency, signals an error of an unspecified type."
  (declare (type string identifier))
  (the instruction
    (or (cdr (assoc identifier +IDENTIFIERS+ :test #'string=))
        (error "Invalid instruction identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position into the
   SOURCE immediately succeeding the skipped section."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun character-at-equals-p (source position expected-character)
  "Determines whether the POSITION into the SOURCE designated a valid
   index and the character located at the same equals the
   EXPECTED-CHARACTER in a case-sensitive manner, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string    source))
  (declare (type fixnum    position))
  (declare (type character expected-character))
  (the boolean
    (get-boolean-value-of
      (and (array-in-bounds-p source position)
           (char= (char source position) expected-character)))))

;;; -------------------------------------------------------

(defun comment-starts-p (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, a comment, introduced via the character twissel \"//\",
   commences, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (the boolean
    (get-boolean-value-of
      (and (character-at-equals-p source start #\/)
           (character-at-equals-p source start #\/)))))

;;; -------------------------------------------------------

(defun find-end-of-word (source start)
  "Proceeding from the START position in the SOURCE, finds and returns
   the index into the same of the first non-word constituent."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'alpha-char-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-next-word (source start)
  "Proceeding from the START position into the SOURCE, and contingently
   skipping any leading spaces, attempts to find the next word,
   returning two values:
     (1) If a word could be detected, a string representation of the
         same, otherwise ``NIL''.
     (2) If a word could be detected, the position into the SOURCE
         immediately succeeding the desinent word character; otherwise
         the length of the source."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((position (skip-spaces source start)))
    (declare (type fixnum position))
    (the (values (or null string) fixnum)
      (cond
        ;; End of file?
        ((>= position (length source))
          (values NIL
            (length source)))
        ;; Start of comment?
        ((comment-starts-p source position)
          (values NIL
            (length source)))
        ;; Invalid character?
        ((not (alpha-char-p (char source position)))
          (error "Unexpected character \"~c\" at position ~d."
            (char source position)
            position))
        ;; Valid word constituent?
        (T
          (let ((end-position (find-end-of-word source position)))
            (declare (type fixnum end-position))
            (values
              (subseq source position end-position)
              end-position)))))))

;;; -------------------------------------------------------

(defun read-phrase (source)
  "Extracts from the SOURCE its words in a canonical representation as a
   single string, each two subsequent words segregated by a single
   space, and returns the same, obeying the Grounded rules for comments
   as omitted segments.
   ---
   If no significant content could be detected, an empty string exhausts
   the response."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (flet ((process-word (word new-position)
            "If the WORD does not equal ``NIL'', returns a singleton
             list encompassing as its aefauld constituent the WORD,
             otherwise returning ``NIL'', and in any case updates the
             POSITION cursor to the NEW-POSITION."
            (declare (type (or null string) word))
            (declare (type fixnum           new-position))
            (the (list-of string)
              (prog1
                (and word (list word))
                (setf position new-position)))))
      (the string
        (concatenate-strings
          (loop while (< position (length source)) append
            (multiple-value-call #'process-word
              (read-next-word source position))))))))

;;; -------------------------------------------------------

(defun extract-instructions (source)
  "Extracts and returns from the piece of Grounded SOURCE code
   ``program'' representation encapsulating its instructions."
  (declare (type string source))
  (with-input-from-string (input-stream source)
    (declare (type string-stream input-stream))
    (the program
      (convert-into-program
        (loop
          for current-line
            of-type (or null string)
            =       (read-line input-stream NIL NIL)
          while current-line
            for current-phrase
              of-type string
              =       (read-phrase current-line)
            when (plusp (length current-phrase))
              collect (parse-instruction current-phrase))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of preprocessor.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-wait-pairing (program)
  "Ascertains that each \"wait\" instruction in the PROGRAM is succeeded
   by its jumelle in immediate adjacency, upon confirmation returning
   the PROGRAM itself, otherwise signaling an error of an unspecified
   type."
  (declare (type program program))
  (labels
      ((wait-instruction-follows-p (probed-position)
        "Determines whether the instruction at the PROBED-POSITION in
         the PROGRAM constitutes a \"wait\" operation, on confirmation
         returning a ``boolean'' value of ``T'', otherwise ``NIL''."
        (declare (type fixnum probed-position))
        (the boolean
          (get-boolean-value-of
            (and (array-in-bounds-p program probed-position)
                 (eq (aref program probed-position) :wait)))))
       (expect-wait-instruction (probed-position)
        "Determines whether the instruction at the PROBED-POSITION in
         the PROGRAM constitutes a \"wait\" operation, on confirmation
         returning no value, otherwise signaling an error of an
         unspecified type."
        (declare (type fixnum probed-position))
        (unless (wait-instruction-follows-p probed-position)
          (error 'Impatience-Error :offending-line
            (1+ probed-position)))
        (values)))
    (loop
      with ip of-type fixnum = 0
      while (< ip (length program))
        if (eq (aref program ip) :wait) do
          (expect-wait-instruction (1+ ip))
          (incf ip 2)
        else do
          (incf ip)))
  (the program program))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character encoding and decoding.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string +CHARACTER-TABLE+))

;;; -------------------------------------------------------

(defparameter +CHARACTER-TABLE+
  " ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "Enumerates the available character by mediation of the indices into
   this string.")

;;; -------------------------------------------------------

(defun decode-character (character-code)
  "Returns the character corresponding ot the Grounded CHARACTER-CODE."
  (declare (type fixnum character-code))
  (the character
    (schar +CHARACTER-TABLE+
      (mod character-code
        (length +CHARACTER-TABLE+)))))

;;; -------------------------------------------------------

(defun encode-character (character)
  "Returns the numeric Grounded code for the CHARACTER, or signals, upon
   its disrespondency to the imposed repertoire, an error of the type
   ``Character-Encoding-Error''.
   ---
   Pursuant to Grounded's restricted character repertoire, the code
   detection proceeds in a case-insensitive fashion."
  (declare (type character character))
  (the fixnum
    (or (position character +CHARACTER-TABLE+ :test #'char-equal)
        (error 'Character-Encoding-Error
          :offending-character character))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of day operations.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric get-next-day (current-day)
  (:documentation
    "Returns the weekday corresponding to the CURRENT-DAY's morrow.")
  
  (:method ((current-day (eql :friday)))
    (declare (type day current-day))
    (the day :saturday))
  
  (:method ((current-day (eql :saturday)))
    (declare (type day current-day))
    (the day :sunday))
  
  (:method ((current-day (eql :sunday)))
    (declare (type day current-day))
    (the day :monday))
  
  (:method ((current-day (eql :monday)))
    (declare (type day current-day))
    (the day :tuesday))
  
  (:method ((current-day (eql :tuesday)))
    (declare (type day current-day))
    (the day :wednesday))
  
  (:method ((current-day (eql :wednesday)))
    (declare (type day current-day))
    (the day :thursday))
  
  (:method ((current-day (eql :thursday)))
    (declare (type day current-day))
    (the day :friday)))

;;; -------------------------------------------------------

(defun day-name (destination
                 day
                 colon-modifier-supplied-p
                 at-sign-modifier-supplied-p
                 &rest prefix-parameters)
  "Prints the DAY's name to the DESTINATION, ignoring the flags
   COLON-MODIFIER-SUPPLIED and AT-SIGN-MODIFIER-SUPPLIED-P, as well as
   the PREFIX-PARAMETERS, and returns no value."
  (declare (type destination       destination))
  (declare (type day               day))
  (declare (type T                 colon-modifier-supplied-p))
  (declare (ignore                 colon-modifier-supplied-p))
  (declare (type T                 at-sign-modifier-supplied-p))
  (declare (ignore                 at-sign-modifier-supplied-p))
  (declare (type prefix-parameters prefix-parameters))
  (declare (ignore                 prefix-parameters))
  (format destination "~a"
    (case day
      (:friday    "Friday")
      (:saturday  "Saturday")
      (:sunday    "Sunday")
      (:monday    "Monday")
      (:tuesday   "Tuesday")
      (:wednesday "Wednesday")
      (:thursday  "Thursday")
      (otherwise  (error "Invalid day: ~s." day)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of temporal operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-12-hour-format (24-hour-time)
  "Converts the 24-HOUR-TIME hour specification into its 12-hour
   equivalency and returns two values:
     (1) The hour as an integer number in the closed interval [1, 12]
         representing the 24-HOUR-TIME's 12-hour tantamount.
     (2) The period suffix ``:am'' or ``:pm''."
  (declare (type 24-hour-time 24-hour-time))
  (the (values 12-hour-time time-period)
    (case 24-hour-time
      (0
        (values 12 :am))
      ((1 2 3 4 5 6 7 8 9 10 11)
        (values 24-hour-time :am))
      (12
        (values 12 :pm))
      ((13 14 15 16 17 18 19 20 21 22 23)
        (values (mod 24-hour-time 12) :pm))
      (otherwise
        (error "Invalid 24-hour time: ~s." 24-hour-time)))))

;;; -------------------------------------------------------

(defun in-12-hour-format (destination
                          24-hour-time
                          colon-modifier-supplied-p
                          at-sign-modifier-supplied-p
                          &rest prefix-parameters)
  "Prints the 24-HOUR-TIME's 12-hour format representation to the
   DESTINATION, ignoring the flags COLON-MODIFIER-SUPPLIED and
   AT-SIGN-MODIFIER-SUPPLIED-P, and, if the PREFIX-PARAMETERS comprehend
   as the first element an integer number, appropriating the same as the
   minutes specification, otherwise defaulting to the standard of zero
   minutes (\"00\"), finally returns no value."
  (declare (type destination       destination))
  (declare (type 24-hour-time      24-hour-time))
  (declare (type T                 colon-modifier-supplied-p))
  (declare (ignore                 colon-modifier-supplied-p))
  (declare (type T                 at-sign-modifier-supplied-p))
  (declare (ignore                 at-sign-modifier-supplied-p))
  (declare (type prefix-parameters prefix-parameters))
  (multiple-value-bind (12-hour-time period)
      (convert-into-12-hour-format 24-hour-time)
    (declare (type 12-hour-time 12-hour-time))
    (declare (type time-period  period))
    (format destination "~2,'0d:~2,'0d ~a" 12-hour-time
      (or (first prefix-parameters)
          0)
      period))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Grounded-Error (error)
  ()
  (:documentation
    "The ``Grounded-Error'' condition type establishes the common
     substrate for all conditions dedicated to the communication of
     anomalous situations arising during the interpretation of a
     Grounded program."))

;;; -------------------------------------------------------

(define-condition Character-Encoding-Error (Grounded-Error)
  ((offending-character
    :initarg       :offending-character
    :initform      (error "Missing offending character.")
    :reader        character-encoding-error-offending-character
    :type          character
    :documentation "The character whose representation via the Grounded
                    character set cannot be achieved."))
  (:report
    (lambda (condition stream)
      (declare (type Character-Encoding-Error condition))
      (declare (type destination              stream))
      (format stream "Cannot represent the character \"~c\" via the ~
                      Grounded character repertoire."
        (character-encoding-error-offending-character condition))))
  (:documentation
    "The ``Character-Encoding'' condition type serves to communicate the
     erroneous endeavor to query the Grounded-compliant code for a
     character whose representation by this system eludes the requisite
     feasibility."))

;;; -------------------------------------------------------

(define-condition Impatience-Error (Grounded-Error)
  ((offending-line
    :initarg       :offending-line
    :initform      (error "Missing offending line.")
    :reader        impatience-error-offending-line
    :type          (integer 1 *)
    :documentation "The one-based number of the line whose failure to
                    attend to a second \"wait\" instruction has begotten
                    this error's etiology."))
  (:report
    (lambda (condition stream)
      (declare (type Impatience-Error condition))
      (declare (type destination      stream))
      (format stream "A second \"wait\" instruction, expected on ~
                      the ~:r line, is missing."
        (impatience-error-offending-line condition))))
  (:documentation
    "The ``Impatience-Error'' condition type serves to communicate the
     second \"wait\" operation's lacuna in a jumelle."))

;;; -------------------------------------------------------

(define-condition Insomniac-Error (Grounded-Error)
  ((attempted-instruction
    :initarg       :attempted-instruction
    :initform      (error "Missing attempted instruction.")
    :reader        insomniac-error-attempted-instruction
    :type          instruction
    :documentation "The instruction whose intempestive execution has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type insomniac-Error condition))
      (declare (type destination     stream))
      (format stream "Cannot execute the instruction ~s, ~
                      as you are not grounded anymore."
        (insomniac-error-attempted-instruction condition))))
  (:documentation
    "The ``Insomniac-Error'' condition type serves to communicate the
     attempt to execute an during the hour reserved for the sleeping,
     which constitutes 21:00 (09:30 PM) o'clock."))

;;; -------------------------------------------------------

(define-condition Lab-Closed-Error (Grounded-Error)
  ((hour
    :initarg       :hour
    :initform      (error "Missing hour.")
    :reader        lab-closed-error-hour
    :type          24-hour-time
    :documentation "The hour of the violation's transpiration.")
   (day
    :initarg       :day
    :initform      (error "Missing day.")
    :reader        lab-closed-error-day
    :type          day
    :documentation "The day of the violation's transpiration."))
  (:report
    (lambda (condition stream)
      (declare (type Lab-Closed-Error condition))
      (declare (type destination      stream))
      (format stream "Cannot visit the tech lab on ~/day-name/ ~
                      at ~30/in-12-hour-format/."
        (lab-closed-error-day  condition)
        (lab-closed-error-hour condition))))
  (:documentation
    "The ``Lab-Closed-Error'' condition type serves to communicate
     the inadmissible attempt to invoke the \"tech lab\" operation at
     any instant outside of a Saturday at 12:30 (12:30 PM) o'clock."))

;;; -------------------------------------------------------

(define-condition Late-Bird-Error (Grounded-Error)
  ((attempted-instruction
    :initarg       :attempted-instruction
    :initform      (error "Missing attempted instruction.")
    :reader        late-bird-error-attempted-instruction
    :type          instruction
    :documentation "The instruction whose intempestive execution has
                    instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Late-Bird-Error condition))
      (declare (type destination     stream))
      (format stream "Cannot execute the instruction ~s, ~
                      as you are not grounded anymore."
        (late-bird-error-attempted-instruction condition))))
  (:documentation
    "The ``Late-Bird-Error'' condition type serves to communicate the
     attempt to execute an operation in the face of the state not being
     grounded anymore."))

;;; -------------------------------------------------------

(define-condition Somnolence-Error (Grounded-Error)
  ((hour
    :initarg       :hour
    :initform      (error "Missing hour.")
    :reader        early-sleeper-error-hour
    :type          24-hour-time
    :documentation "The intempestively chosen hour to rest."))
  (:report
    (lambda (condition stream)
      (declare (type Somnolence-Error condition))
      (declare (type destination         stream))
      (format stream "Cannot sleep at ~30/in-12-hour-format/ o'clock."
        (early-sleeper-error-hour condition))))
  (:documentation
    "The ``Somnolence-Error'' condition type serves to communicate the
     inadmissible attempt to invoke the \"sleep\" operation at any time
     outside of the 21:30 (09:30 PM) o'clock temporal window."))

;;; -------------------------------------------------------

(define-condition Uninspired-Error (Grounded-Error)
  ((hour
    :initarg       :hour
    :initform      (error "Missing hour.")
    :reader        uninspired-error-hour
    :type          24-hour-time
    :documentation "The hour of the violation's transpiration.")
   (remaining-days
    :initarg       :remaining-days
    :initform      (error "Missing remaining days.")
    :reader        uninspired-error-remaining-days
    :type          (integer 0 *)
    :documentation "The tally of days being grounded."))
  (:report
    (lambda (condition stream)
      (declare (type Uninspired-Error condition))
      (declare (type destination      stream))
      (format stream "The program has terminated ~
                      at ~30/in-12-hour-format/, with ~d day~:p ~
                      remaining, but you are still grounded."
        (uninspired-error-hour           condition)
        (uninspired-error-remaining-days condition))))
  (:documentation
    "The ``Uninspired-Error'' condition type serves to communicate the
     termination of a program, incited by its instruction sequence's
     exhaustion, ere the term of being grounded has lapsed."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :accessor      memory-cells
    :type          cell-table
    :documentation "A sparse vector of integer-valued cells.")
   (pointer
    :initform      0
    :accessor      memory-pointer
    :type          integer
    :documentation "The index (key) of the currently selected cell in
                    the CELLS table."))
  (:documentation
    "The ``Memory'' class realizes the Grounded program memory as a
     bilaterally infinite vector of signed integer cells."))

;;; -------------------------------------------------------

(defun make-memory ()
  "Creates and returns a fresh ``Memory'' object."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun current-cell (memory)
  "Returns the value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the integer
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell and returns no
   value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    new-value)
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory &optional (amount 1))
  "Increments the MEMORY's current cell by the AMOUNT and returns no
   value."
  (declare (type Memory        memory))
  (declare (type (integer 0 *) amount))
  (incf (current-cell memory) amount)
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory &optional (amount 1))
  "Decrements the MEMORY's current cell by the AMOUNT and returns no
   value."
  (declare (type Memory        memory))
  (declare (type (integer 0 *) amount))
  (decf (current-cell memory) amount)
  (values))

;;; -------------------------------------------------------

(defun duplicate-current-cell (memory)
  "Multiplies the MEMORY's current cell value by two, stores the product
   in the current cell, and returns no value."
  (declare (type Memory memory))
  (setf (current-cell memory)
    (* (current-cell memory) 2))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and returns
   no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type 24-hour-time +FIRST-HOUR-OF-DAY+))
(declaim (type 24-hour-time +LAST-HOUR-OF-DAY+))

;;; -------------------------------------------------------

(defparameter +FIRST-HOUR-OF-DAY+ 7
  "The hour, in the 24-hour format, at a day begins.")

(defparameter +LAST-HOUR-OF-DAY+ 21
  "The hour, in the 24-hour format, at which the day ends.")

;;; -------------------------------------------------------

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program.")
    :reader        get-program
    :type          program
    :documentation "The Grounded program to evaluate.")
   (ip
    :initform      0
    :accessor      ip
    :type          fixnum
    :documentation "The position into the PROGRAM of the currently
                    processed instruction.")
   (current-day
    :initform      :friday
    :accessor      current-day
    :type          day
    :documentation "The current weekday's identifier.")
   (current-hour
    :initform      +FIRST-HOUR-OF-DAY+
    :accessor      current-hour
    :type          24-hour-time
    :documentation "The current time's hour, eliding the minutes, which
                    are always assumed as 30, instead retaining the
                    hour.")
   (remaining-days
    :initform      2
    :accessor      remaining-days
    :type          (integer 0 *)
    :documentation "The tally of remaining days, excluding the currently
                    enjoyed one, ere the program's termination.")
   (still-grounded-p
    :initform      T
    :accessor      still-grounded-p
    :type          boolean
    :documentation "A flag which determines whether the quesited person
                    is yet grounded.")
   (memory
    :initform      (make-memory)
    :reader        get-memory
    :type          Memory
    :documentation "The program memory as a sparse vector of
                    integer-valued cells."))
  (:documentation
    "The ``Interpreter'' class' onus is delineated by the assignment of
     actual effect to a parsed Grounded program."))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a new ``Interpreter'' whose dedication is
   established in the Grounded PROGRAM's evaluation."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun sleeping-time-p (interpreter)
  "Determines whether the program's current hour, as maintained by the
   INTERPRETER, constitutes the expected sleeping time, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (= (current-hour interpreter)
         +LAST-HOUR-OF-DAY+))))

;;; -------------------------------------------------------

(defun end-day (interpreter)
  "Concludes the INTERPRETER's currently reckoned day by changing to the
   next one, while concomitantly resetting its hour specification to the
   morning of this new morrow, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (current-hour interpreter) +FIRST-HOUR-OF-DAY+)
  (setf (current-day interpreter)
    (get-next-day
      (current-day interpreter)))
  (if (plusp (remaining-days interpreter))
    (decf (remaining-days interpreter))
    (setf (still-grounded-p interpreter) NIL))
  (values))

;;; -------------------------------------------------------

(defun pass-hour (interpreter)
  "Segues from the current hour as supputated by the INTERPRETER into
   the next one and returns no value."
  (declare (type Interpreter interpreter))
  (incf (current-hour interpreter))
  (values))

;;; -------------------------------------------------------

(defun ascertain-wake-time (interpreter attempted-instruction)
  "Determines whether the INTERPRETER's currently supputated hour
   accounts for a waking spell, upon confirmation returning no value;
   otherwise signals an error of an unspecified type employing for its
   communication the ATTEMPTED-INSTRUCTION."
  (declare (type Interpreter interpreter))
  (declare (type instruction attempted-instruction))
  (when (sleeping-time-p interpreter)
    (error 'Insomniac-Error
      :attempted-instruction attempted-instruction))
  (values))

;;; -------------------------------------------------------

(defun ascertain-still-grounded (interpreter attempted-instruction)
  "Determines whether the INTERPRETER's state confirms it as still
   grounded, upon confirmation returning no value; otherwise an error of
   the type ``Late-Bird-Error'' is signaled."
  (declare (type Interpreter interpreter))
  (declare (type instruction attempted-instruction))
  (unless (still-grounded-p interpreter)
    (error 'Late-Bird-Error
      :attempted-instruction attempted-instruction))
  (values))

;;; -------------------------------------------------------

(defun tech-lab-time-p (interpreter)
  "Determines whether the current hour of the current day, as measured
   by the INTERPRETER's supputations, constitutes a valid spell for a
   sojourn in the tech lab, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (and (eq (current-day  interpreter) :saturday)
           (=  (current-hour interpreter) 12)))))

;;; -------------------------------------------------------

(defun ascertain-tech-lab-availability (interpreter)
  "Determines whether the current hour of the current day, as measured
   by the INTERPRETER's supputations, constitutes a valid spell for a
   sojourn in the tech lab, returning on confirmation no value;
   otherwise signaling an error of an unspecified type."
  (declare (type Interpreter interpreter))
  (unless (tech-lab-time-p interpreter)
    (error 'Lab-Closed-Error
      :hour (current-hour interpreter)
      :day  (current-day  interpreter)))
  (values))

;;; -------------------------------------------------------

(defun get-current-instruction (interpreter)
  "Returns the currently processed instruction of the program maintained
   by the INTERPRETER."
  (declare (type Interpreter interpreter))
  (the instruction
    (aref
      (get-program interpreter)
      (ip          interpreter))))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the Grounded program consigned to the
   INTERPRETER's castaldy is exhausted, that is, the instruction pointer
   (IP) has transcended beyond its desinent operation, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (get-boolean-value-of
      (>= (ip interpreter)
          (length
            (get-program interpreter))))))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the INSTRUCTION in the INTERPRETER's context and returns
     no value.")
  
  (:method ((interpreter Interpreter)
            (instruction (eql :sleep)))
    (declare (type instruction instruction))
    (declare (ignore           instruction))
    (if (sleeping-time-p interpreter)
      (end-day interpreter)
      (error 'Somnolence-Error :hour (current-hour interpreter)))
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :bother-parents)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (incf (remaining-days interpreter))
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :tech-lab)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time             interpreter instruction)
    (ascertain-still-grounded        interpreter instruction)
    (ascertain-tech-lab-availability interpreter)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :wait)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :read)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (format T "~&>> ")
    (finish-output)
    (setf (current-cell (get-memory interpreter))
      (encode-character
        (read-char)))
    (clear-input)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :watch-steven-universe)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (move-cell-pointer-right
      (get-memory interpreter))
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :watch-adventure-time)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (move-cell-pointer-left
      (get-memory interpreter))
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :make-book)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (write-char
      (decode-character
        (current-cell
          (get-memory interpreter))))
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :make-plushie-movie)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (increment-current-cell (get-memory interpreter) 1)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :make-plushie-movie-series)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (increment-current-cell (get-memory interpreter) 10)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :uninstall-app-on-tablet)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (decrement-current-cell (get-memory interpreter) 1)
    (pass-hour interpreter)
    (values))
  
  (:method ((interpreter Interpreter)
            (instruction (eql :daydream)))
    (declare (type Interpreter interpreter))
    (declare (type instruction instruction))
    (ascertain-wake-time      interpreter instruction)
    (ascertain-still-grounded interpreter instruction)
    (duplicate-current-cell
      (get-memory interpreter))
    (pass-hour interpreter)
    (values)))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the program consigned to the INTERPRETER's castaldy and
   returns no value."
  (declare (type Interpreter interpreter))
  (loop
    until
      (or (program-exhausted-p interpreter)
          (not (still-grounded-p interpreter)))
    do
      (process-instruction interpreter
        (get-current-instruction interpreter))
      (incf (ip interpreter)))
  ;; Program exhausted, but still grounded?
  (when (still-grounded-p interpreter)
    (error 'Uninspired-Error
      :hour           (current-hour   interpreter)
      :remaining-days (remaining-days interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Grounded (code)
  "Interprets the piece of Grounded source CODE and returns no value."
  (declare (type string code))
  (interpret-program
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HELLOWORLD".
(interpret-Grounded
  "
  make plushie movie series // 8:30AM Friday
  uninstall app on tablet // 9:30AM Friday
  uninstall app on tablet // 10:30AM Friday
  make book // 11:30AM Friday
  uninstall app on tablet // 12:30PM Friday
  uninstall app on tablet // 1:30PM Friday
  uninstall app on tablet // 2:30PM Friday
  make book // 3:30PM Friday
  watch steven universe // 4:30PM Friday
  make plushie movie series // 5:30PM Friday
  make plushie movie // 6:30PM Friday
  make plushie movie /// 7:30PM Friday
  make book // 8:30PM Friday
  make book // 9:30PM Friday
  sleep // 7:30AM Saturday
  watch steven universe // 8:30AM Saturday
  make plushie movie series // 9:30AM Saturday
  make plushie movie series // 10:30AM Saturday
  uninstall app on tablet // 11:30AM Saturday
  uninstall app on tablet // 12:30PM Saturday
  uninstall app on tablet //  1:30PM Saturday
  uninstall app on tablet // 2:30PM Saturday
  uninstall app on tablet // 3:30PM Saturday
  make book // 4:30PM Saturday
  watch steven universe // 5:30PM Saturday
  make plushie movie series // 6:30PM Saturday
  make plushie movie series // 7:30PM Saturday
  make plushie movie // 8:30PM Saturday
  make plushie movie // 9:30PM Saturday
  sleep // 7:30AM Sunday
  make plushie movie // 8:30AM Sunday
  make book // 9:30AM Sunday
  watch adventure time // 10:30AM Sunday
  make book // 11:30AM Sunday
  make plushie movie // 12:30PM Sunday
  make plushie movie // 1:30PM Sunday
  make plushie movie // 2:30PM Sunday
  make book // 3:30PM Sunday
  uninstall app on tablet // 4:30PM Sunday
  uninstall app on tablet // 5:30PM Sunday
  uninstall app on tablet // 6:30PM Sunday
  uninstall app on tablet // 7:30PM Sunday
  uninstall app on tablet // 8:30PM Sunday
  bother parents // 9:30PM Sunday
  sleep // 7:30AM Monday
  uninstall app on tablet // 8:30PM Monday
  make book // 9:30AM Monday
  watch steven universe // 10:30AM Monday
  watch steven universe // 11:30AM Monday
  make plushie movie // 12:30AM Monday
  make plushie movie // 1:30PM Monday
  make plushie movie // 2:30PM Monday
  make plushie movie // 3:30PM Monday
  make book          // 4:30 PM Monday
  wait               // 5:30 PM Monday
  wait               // 6:30PM Monday
  daydream           // 7:30PM Monday
  daydream           // 8:30PM Monday
  daydream           // 9:30PM Monday
  sleep
  ")

;;; -------------------------------------------------------

;; One-time cat program.
;; 
;; Please heed that inputs ought to obey the Grounded character
;; repertoire's circumference.
(interpret-Grounded
  "
  read
  make book // this is done by 9:30AM on friday
  watch steven universe // 10:30
  daydream // 11
  daydream // 12
  daydream // 1
  daydream // 2
  daydream // 3
  daydream // 4
  daydream // 5
  daydream // 6
  daydream // 7
  daydream // 8
  daydream // 9
  sleep // going to saturday
  daydream // 8
  daydream // 9
  daydream // 10
  daydream // 11
  daydream // 12
  daydream // 1
  daydream // 2
  daydream // 3
  daydream // 4
  daydream // 5
  daydream // 6
  daydream // 7
  daydream // 8
  daydream // 9
  sleep // going to final day
  daydream // 8
  daydream // 9
  daydream // 10
  daydream // 11
  daydream // 12
  daydream // 1
  daydream // 2
  daydream // 3
  daydream // 4
  daydream // 5
  daydream // 6
  daydream // 7
  daydream // 8
  daydream // 9
  sleep
  ")
