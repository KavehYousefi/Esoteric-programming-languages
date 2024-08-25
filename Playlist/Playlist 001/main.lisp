;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Playlist", invented by the Esolang user "A" and presented
;; on July 12th, 2019, the diorism of which harbors its woning in a
;; music playlist's mimicry, the traversal helmed by a triad of
;; instructions in conjunction with a condition twissel.
;; 
;; 
;; Concept
;; =======
;; The playlist programming language operates on a conceived playlist,
;; offering the capacitation for its items' selections by three modes,
;; augmented in this potential via two conditional adminicula.
;; 
;; The tracks' tally does not present a component of deliberation in its
;; designment, being yielded by an equiparation with the installed
;; instructions.
;; 
;; 
;; Architecture
;; ============
;; Its kenspeckle haecceity divorces the Playlist programming language
;; from an architectural requisitum, the aefauld subject of conformation
;; attending its woning in the eponymous playlist, conflating therein
;; with the program instructions.
;; 
;; 
;; Data Structures
;; ===============
;; The playlist items in their exclusive participation in a program
;; vindicate the absence of any further data structure, apart from the
;; elements' listing.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical vista, a Playlist program comprises a sequence of
;; zero or more command lines, each such a composition of a mandatory
;; selection mode and an optional conditional suffix, the latter, if
;; present, segregated by the at least one space.
;; 
;; == WHITESPACES ==
;; The presence of spaces constitutes an optional and tolerated aspect
;; in all localities, except for the interstice betwixt a selection mode
;; and a conditional, in which case is adduction imposes a requisitum.
;; 
;; Newlines, on the other hand, thole the cumbrance of codified
;; stringency: Maugre the patience's administration to blank rows, each
;; twain of subsequent command lines ought to bear at least one
;; linebreak as the sepiment.
;; 
;; == COMMENTS ==
;; No provision for comments has been incorporated in the current
;; language rendition.
;; 
;; == GRAMMAR ==
;; The Playlist language's donet shall enjoy, as a medium for its
;; enhanced formal expression, a treatise complying with the Extended
;; Backus-Naur Form (ENBF):
;; 
;;   program        := padding
;;                  ,  [ firstLine , { subsequentLine } , padding ]
;;                  ;
;;   firstLine      := command ;
;;   subsequentLine := newlines , command ;
;;   command        := selectionMode , [ spaces , condition ] ;
;;   selectionMode  := "Looping" | "Random" | "Sequential" ;
;;   condition      := "Played"  | "Unique" ;
;;   padding        := { space | newline } ;
;;   spaces         := space , { space } ;
;;   space          := " " ;
;; 
;; 
;; Instructions
;; ============
;; The Playlist programming language enumerates three modes and two
;; optional conditions, their coefficiency directed at the underlying
;; playlist's castaldy.
;; 
;; == IDENTIFIERS ==
;; A parvipotent language, Playlist's membership of identifiers,
;; amplecting both selection modes and conditionals, enumerates a mere
;; quintuple quantity.
;; 
;; This set's participant and their dedicated roles shall be the
;; following tabulation's cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Identifier | Shorthand | Role
;;   -----------+-----------+------------------------------------------
;;   Looping    | L         | Selection mode
;;   ..................................................................
;;   Played     | P         | Condition
;;   ..................................................................
;;   Random     | R         | Selection
;;   ..................................................................
;;   Sequential | S         | Selection mode
;;   ..................................................................
;;   Unique     | U         | Condition
;;   ..................................................................
;; 
;; == SELECTION MODES ==
;; The selection mode serves in the establishment of the policy
;; according to whom the next playlist item will be selected:
;; 
;;   ------------------------------------------------------------------
;;   Mode       | Shorthand | Effect
;;   -----------+-----------+------------------------------------------
;;   Sequential | S         | Plays the next item in the playlist.
;;   ..................................................................
;;   Looping    | L         | Plays the first item in the playlist.
;;   ..................................................................
;;   Random     | R         | Plays a random item in the playlist.
;;   ------------------------------------------------------------------
;; 
;; == CONDITIONS ==
;; An execution's dependency upon a certain stipulation may be simulated
;; by a conditional prosthesis to a selection mode, offering for this
;; purpose a twissel of options:
;; 
;;   ------------------------------------------------------------------
;;   Condition | Shorthand | Effect
;;   ----------+-----------+-------------------------------------------
;;   Unique    | U         | Determines whether all items in the
;;             |           | playlist have been played.
;;   ..................................................................
;;   Played    | P         | Determines whether the player is currently
;;             |           | playing an item that has already been
;;             |           | played.
;;   ------------------------------------------------------------------
;; 
;; The governance of modesty in its modes and conditionals'
;; circumference capacitates the various combinations' express
;; delineation:
;; 
;;   ------------------------------------------------------------------
;;   Mode       | Condition | Effect
;;   -----------+-----------+------------------------------------------
;;   Sequential | None      | Plays the next item from the playlist.
;;   ..................................................................
;;   Sequential | Played    | Plays the next item from the playlist
;;              |           | which has not be played yet.
;;   ..................................................................
;;   Sequential | Unique    | Plays the next item from the playlist if
;;              |           | not all items have already been played.
;;   ..................................................................
;;   Looping    | None      | Plays the first item from the playlist.
;;   ..................................................................
;;   Looping    | Played    | Plays the first item from the playlist if
;;              |           | the same has not yet been played.
;;   ..................................................................
;;   Looping    | Unique    | Plays the first item if not all items
;;              |           | have been played yet.
;;   ..................................................................
;;   Random     | None      | Plays a randomly selected item from the
;;              |           | playlist.
;;   ..................................................................
;;   Random     | Played    | Plays a randomly selected item from the
;;              |           | playlist which has not yet been played.
;;   ..................................................................
;;   Random     | Unique    | Plays a randomly selected item from the
;;              |           | playlist if not all items have already
;;              |           | been played.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre the simplicity commorant in the Playlist programming language,
;; its protolog ostends a few inroads of ancipitous nature, a subset
;; desumed therefrom shall comprise the following exposition's cynosure.
;; 
;; == HOW SHALL THE COMPOUND "Looping Unique" EVALUATED? ==
;; A particular forbisen for the language's operation resides in the
;; example
;; 
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;;   Looping Unique
;; 
;; The original author's claim encompasses an iterance potential that
;; produces a tally of 8! = 40320 repetitions in this program.
;; 
;; Given, however, the "Looping" instruction's and "Unique" condition's
;; diorisms --- the former shall return the selection to the first
;; playlist item, while the latter exclusively admits the ultimate
;; chevisance if not all items have been played yet ---, an infinite
;; loop, restricted to the incipient member's processing inflicted with
;; perpetuality, should be imputed.
;; 
;; It has been adjudged to comply to this latter account, as the
;; provenance fails in its corroboration of the desiderated behavior's
;; ratiocination.
;; 
;; 
;; Implementation
;; ==============
;; The interpreter has been developed in the programming language
;; Common Lisp, its procession ensuing from every Playlist statement's
;; encapsulation in a dedicated ``Command'' object, this constituting a
;; jumelle of a selection mode and a execution condition, ere the
;; thus produced sequence's evaluation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-28
;; 
;; Sources:
;;   [esolang2019Playlist]
;;   The Esolang contributors, "Playlist", 25th December, 2019
;;   URL: "https://esolangs.org/wiki/Playlist"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype play-mode ()
  "The ``play-mode'' type enumerates the recognized variants of policies
   according to which the next playlist item selection is committed."
  '(member :looping :random :sequential))

;;; -------------------------------------------------------

(deftype play-condition ()
  "The ``play-condition'' type enumerates the recognized variants of
   predicates whose negation capacitates the next playlist item's
   selection."
  '(member :none :unique :played))

;;; -------------------------------------------------------

(deftype playlist-program ()
  "The ``playlist-program'' type defines an executable Playlist program
   as a vector composed of zero or more ``Command'' objects."
  '(vector Command *))

;;; -------------------------------------------------------

(deftype item-handler ()
  "The ``item-handler'' type defines a callback function notified during
   a Playlist program's evaluation on each instant involving an item
   being played, the communication from the executing environment to the
   handler proceeding by adminiculum of an ``Item-Event'', while the
   function's response does not contribute any causatum.
   ---
   Founded upon the adduced diorism, the following signature is imposed:
     lambda (item-event) => ignored-result"
  '(function (Item-Event) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operation,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of mode and condition table.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-play-mode (identifier)
  "Returns the ``play-mode'' associated with the IDENTIFIER, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type string identifier))
  (the play-mode
    (cond
      ((string= identifier "Looping")    :looping)
      ((string= identifier "Random")     :random)
      ((string= identifier "Sequential") :sequential)
      (T (error "No play mode identifier: ~s." identifier)))))

;;; -------------------------------------------------------

(defun parse-play-condition (identifier)
  "Returns the ``play-condition'' associated with the IDENTIFIER, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type string identifier))
  (the play-condition
    (cond
      ((string= identifier "Unique") :unique)
      ((string= identifier "Played") :played)
      (T (error "No play-condition identifier: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Command ()
  ((play-mode
    :initarg       :play-mode
    :initform      (error "Missing play mode.")
    :reader        get-command-play-mode
    :type          play-mode
    :documentation "The policy applied to choose the next play list
                    item.")
   (play-condition
    :initarg       :play-condition
    :initform      (error "Missing play condition.")
    :reader        get-command-play-condition
    :type          play-condition
    :documentation "The optional condition whose negated predicate shall
                    serve as a gate to this command's execution."))
  (:documentation
    "The ``Command'' class encapsulates a behest issued during a
     Playlist program's execution."))

;;; -------------------------------------------------------

(defun make-command (play-mode play-condition)
  "Returns a new ``Command'' whose selection ensues from the PLAY-MODE,
   the execution being a dependent of the PLAY-CONDITION."
  (declare (type play-mode      play-mode))
  (declare (type play-condition play-condition))
  (the Command
    (make-instance 'Command
      :play-mode      play-mode
      :play-condition play-condition)))

;;; -------------------------------------------------------

(defmethod print-object ((command Command) stream)
  (declare (type Command     command))
  (declare (type destination stream))
  (format stream "(Command mode=~s, condition=~s)"
    (slot-value command 'play-mode)
    (slot-value command 'play-condition)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          string-stream
    :documentation "The piece of PlayList source code as a stream.")
   (current-line
    :initform      NIL
    :type          (or null string)
    :documentation "The currently processed line from the SOURCE.")
   (line-index
    :initform      0
    :type          fixnum
    :documentation "The zero-based index of the CURRENT-LINE in the
                    SOURCE.")
   (column
    :initform      0
    :type          fixnum
    :documentation "The zero-based column index into the CURRENT-LINE
                    of the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the COLUMN of the CURRENT-LINE.")
   (exhausted-p
    :initform      NIL
    :reader        exhausted-p
    :type          boolean
    :documentation "Determines whether the SOURCE is exhausted."))
  (:documentation
    "The ``Scanner'' class applies itself to the lexical analyzation of
     a line of Playlist source code, extracting from the same a command,
     compact of a mandatory selection mode and an optional condition."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((scanner Scanner) &key)
  "Reads from the SCANNER's source stream the first line and the first
   character in the same, and returns the modified SCANNER."
  (declare (type Scanner scanner))
  (with-slots (source current-line) scanner
    (declare (type string-stream    source))
    (declare (type (or null string) current-line))
    ;; Read in the first line.
    (setf current-line
      (when (open-stream-p source)
        (read-line source NIL NIL NIL)))
    ;; Determine whether the source is empty or not.
    (if current-line
      ;; If at least one line exists, read its first character.
      (with-slots (column character) scanner
        (declare (type fixnum              column))
        (declare (type (or null character) character))
        (setf character
          (when (array-in-bounds-p current-line column)
            (char current-line column))))
      ;; If the source is empty, mark it as exhausted.
      (with-slots (exhausted-p) scanner
        (declare (type boolean exhausted-p))
        (close source)
        (setf exhausted-p T))))
  (the Scanner scanner))

;;; -------------------------------------------------------

(defun make-scanner (source)
  "Returns a new ``Scanner'' which evaluates the SOURCE."
  (declare (type string source))
  (the Scanner
    (make-instance 'Scanner :source
      (make-string-input-stream source))))

;;; -------------------------------------------------------

(defun read-next-line (scanner)
  "Reads the next line from the SCANNER' source and returns no value."
  (declare (type Scanner scanner))
  (with-slots (source current-line) scanner
    (declare (type string-stream    source))
    (declare (type (or null string) current-line))
    (setf current-line
      (when (open-stream-p source)
        (read-line source NIL NIL NIL)))
    (if current-line
      (with-slots (line-index column character) scanner
        (declare (type fixnum              line-index))
        (declare (type fixnum              column))
        (declare (type (or null character) character))
        (incf line-index 1)
        (setf column     0)
        (setf character
          (when (array-in-bounds-p current-line column)
            (char current-line column))))
      (with-slots (exhausted-p) scanner
        (declare (type boolean exhausted-p))
        (close source)
        (setf exhausted-p T))))
  (values))

;;; -------------------------------------------------------

(defun advance (scanner)
  "Moves the SCANNER's position cursor to the next column in its current
   line, if possible, and returns the modified SCANNER."
  (declare (type Scanner scanner))
  (with-slots (current-line column character) scanner
    (declare (type (or null string)    current-line))
    (declare (type fixnum              column))
    (declare (type (or null character) character))
    (the (or null character)
      (setf character
        (when (array-in-bounds-p current-line (1+ column))
          (char current-line
            (incf column))))))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces (scanner)
  "Proceeding from the current position in the SCANNER's processed line,
   skips a sequence of zero or more accolent spaces, and returns no
   value."
  (declare (type Scanner scanner))
  (with-slots (character) scanner
    (declare (type (or null character) character))
    (loop while (and character (char= character #\Space)) do
      (advance scanner)))
  (values))

;;; -------------------------------------------------------

(defun read-word (scanner)
  "Proceeding from the current position in the SCANNER's evaluated line,
   reads a word composed of Latin letters and returns a string
   representation thereof."
  (declare (type Scanner scanner))
  (the string
    (with-slots (character) scanner
      (declare (type (or null character) character))
      (with-output-to-string (word)
        (declare (type string-stream word))
        (loop while (and character (alpha-char-p character)) do
          (write-char character word)
          (advance scanner))))))

;;; -------------------------------------------------------

(defun read-play-mode (scanner)
  "Proceeding from the current position in the SCANNER's evaluated line,
   reads a play mode identifier and returns a ``play-mode''
   representation thereof."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the play-mode
    (parse-play-mode
      (read-word scanner))))

;;; -------------------------------------------------------

(defun read-play-condition (scanner)
  "Proceeding from the current position in the SCANNER's evaluated line,
   reads an optional condition and either returns its parsed
   ``play-condition'' equivalent, if present, or the absence sentinel
   ``:none''."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the play-condition
    (if (slot-value scanner 'character)
      (parse-play-condition
        (read-word scanner))
      :none)))

;;; -------------------------------------------------------

(defun expect-end-of-line (scanner)
  "Proceeding from the current column in the SCANNER's processed line,
   determines whether the remaining horizontal extent is devoid of
   effective content, permitting only spaces, on confirmation simply
   returning no value, otherwise signaling an error of an unspecified
   type."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (with-slots (character column line-index) scanner
    (declare (type (or null character) character))
    (declare (type fixnum              column))
    (declare (ignorable                column))
    (declare (type fixnum              line-index))
    (declare (ignorable                line-index))
    (when character
      (error "Expected the end of the line, but encountered \"~c\" ~
              at column ~d of line ~d."
        character column line-index)))
  (values))

;;; -------------------------------------------------------

(defun at-end-of-line-p (scanner)
  "Determines whether the SCANNER has reached the end of its current
   line, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the boolean
    (null (slot-value scanner 'character))))

;;; -------------------------------------------------------

(defun read-command (scanner)
  "Proceeding from the current column in the SCANNER's processed line,
   consumes an aefauld command, composed of exactly one play mode,
   succeeded by zero or one play condition, and returns a ``Command''
   representation thereof."
  (declare (type Scanner scanner))
  (skip-spaces scanner)
  (the Command
    (prog1
      (make-command
        (read-play-mode      scanner)
        (read-play-condition scanner))
      (expect-end-of-line scanner))))

;;; -------------------------------------------------------

(defun extract-commands (scanner)
  "Extracts the commands from the SCANNER's source and returns a
   one-dimensional simple array comprehending these."
  (declare (type Scanner scanner))
  (the playlist-program
    (coerce
      (loop
        until (exhausted-p scanner)
        do    (skip-spaces scanner)
        
        if (at-end-of-line-p scanner) do
          (read-next-line scanner)
        else collect
          (prog1
            (read-command   scanner)
            (read-next-line scanner)))
      '(simple-array Command (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of array operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-simple-bit-vector (size)
  "Creates and returns a fresh simple bit vector of the specified SIZE."
  (declare (type fixnum size))
  (the simple-bit-vector
    (make-array size
      :element-type    'bit
      :initial-element 0
      :adjustable      NIL
      :fill-pointer    NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of playlist.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Playlist ()
  ((length
    :initarg       :length
    :initform      (error "Missing playlist length.")
    :reader        get-playlist-length
    :type          fixnum
    :documentation "The number of tracks in the playlist.")
   (coverage-map
    :initform      (make-simple-bit-vector 0)
    :type          simple-bit-vector
    :documentation "Memorizes the items already played by a bit vector's
                    adminiculum, associating the zero-based item
                    positions with a bit value of zero (0) if not
                    played, and one (1) if already consumed.")
   (position
    :initform      -1
    :type          fixnum
    :reader        get-playlist-position
    :documentation "The zero-based index of the currently selected item
                    in the playlist, represented by its COVERAGE-MAP."))
  (:documentation
    "The ``Playlist'' class serves in the encapsulation of the playlist
     concept as an ordered sequence of fixed items, or \"tracks\",
     amenable to sequential or random access via indices, and
     incorporating the capacity to memorize hitherto played members
     among these."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((playlist Playlist) &key)
  "Initializes the PLAYLIST's coverage map and the random number
   generator and returns no value."
  (declare (type Playlist playlist))
  (setf (slot-value playlist 'coverage-map)
    (make-simple-bit-vector
      (slot-value playlist 'length)))
  (setf *random-state* (make-random-state T))
  (values))

;;; -------------------------------------------------------

(defun make-playlist (length)
  "Creates and returns a new ``Playlist'' comprehending the LENGTH tally
   of items."
  (declare (type fixnum length))
  (the Playlist
    (make-instance 'Playlist :length length)))

;;; -------------------------------------------------------

(defun item-state (playlist id)
  "Returns the played/unplayed flag of the PLAYLIST item amenable to the
   ID as a bit value, the same assumes the zero (0) state if not yet
   being played, otherwise the value one (1)."
  (declare (type Playlist playlist))
  (declare (type fixnum   id))
  (the bit
    (sbit (slot-value playlist 'coverage-map) id)))

;;; -------------------------------------------------------

(defun (setf item-state) (new-state playlist id)
  "Changes the played/unplayed flag of the PLAYLIST item amenable to the
   ID to the NEW-STATE, conveying by a value of zero (0) it being not
   yet played, and by one (1) its procession, and returns no value."
  (declare (type bit      new-state))
  (declare (type Playlist playlist))
  (declare (type fixnum   id))
  (setf (sbit (slot-value playlist 'coverage-map) id) new-state)
  (values))

;;; -------------------------------------------------------

(defun item-played-p (playlist id)
  "Determines whether the PLAYLIST item amenable to the ID has already
   been played, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Playlist playlist))
  (declare (type fixnum   id))
  (the boolean
    (not (zerop (item-state playlist id)))))

;;; -------------------------------------------------------

(defun current-item-played-p (playlist)
  "Determines whether the currently selected PLAYLIST item has already
   been played, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Playlist playlist))
  (the boolean
    (item-played-p playlist
      (slot-value playlist 'position))))

;;; -------------------------------------------------------

(defun mark-item-as-played (playlist id)
  "Designates the PLAYLIST item amenable to the ID as played and returns
   no value."
  (declare (type Playlist playlist))
  (declare (type fixnum   id))
  (setf (item-state playlist id) 1)
  (values))

;;; -------------------------------------------------------

(defun mark-current-item-as-played (playlist)
  "Designates the currently selected PLAYLIST item as played and returns
   no value."
  (declare (type Playlist playlist))
  (mark-item-as-played playlist
    (slot-value playlist 'position))
  (values))

;;; -------------------------------------------------------

(defun all-items-played-p (playlist)
  "Determines whether all PLAYLIST items have already been played,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Playlist playlist))
  (the boolean
    (not (null
      (every #'plusp
        (slot-value playlist 'coverage-map))))))

;;; -------------------------------------------------------

(defun select-first-item (playlist)
  "Selects the first item in the PLAYLIST and returns no value."
  (declare (type Playlist playlist))
  (setf (slot-value playlist 'position) 0)
  (values))

;;; -------------------------------------------------------

(defun select-first-unplayed-item (playlist)
  "Selects the first unplayed item in the PLAYLIST, if possible, and
   returns no value; otherwise, if all tracks have already been played,
   signals an error of an unspecified type."
  (declare (type Playlist playlist))
  (with-slots (position coverage-map) playlist
    (declare (type fixnum            position))
    (declare (type simple-bit-vector coverage-map))
    (setf position
      (or (position 0 coverage-map :test #'=)
          (error "No unplayed item found while searching from the ~
                  start."))))
  (values))

;;; -------------------------------------------------------

(defun last-item-selected-p (playlist)
  "Determines whether currently selected item in the PLAYLIST
   constitutes its desinent track, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Playlist playlist))
  (the boolean
    (not (null
      (>= (get-playlist-position playlist)
          (get-playlist-length   playlist))))))

;;; -------------------------------------------------------

(defun select-next-item (playlist)
  "Advances the PLAYLIST's selection cursor to the next item in its
   list, if possible, and returns no value; otherwise, if the currently
   active track constitutes its desinent one, signals an error of an
   unspecified type."
  (declare (type Playlist playlist))
  (if (last-item-selected-p playlist)
    (error "Cannot advance from the last playlist item at position ~d."
      (get-playlist-position playlist))
    (incf (slot-value playlist 'position)))
  (values))

;;; -------------------------------------------------------

(defun select-next-unplayed-item (playlist)
  "Advances the PLAYLIST's selection cursor to the next unplayed item in
   its list, if possible, and returns no value; otherwise, if all tracks
   have already been played at least once, signals an error of an
   unspecified type."
  (declare (type Playlist playlist))
  (if (last-item-selected-p playlist)
    (error "Cannot advance from the last playlist item at position ~d."
      (get-playlist-position playlist))
    (with-slots (position coverage-map) playlist
      (declare (type fixnum            position))
      (declare (type simple-bit-vector coverage-map))
      (setf position
        (position 0 coverage-map :start position))))
  (values))

;;; -------------------------------------------------------

(defun select-random-unplayed-item (playlist)
  "Selects a random unplayed item from the PLAYLIST, if possible, and
   returns no value; otherwise, if all tracks have already been played
   at least once, signals an error of an unspecified type."
  (declare (type Playlist playlist))
  (loop
    for id
      of-type fixnum
      from    0
      below   (slot-value playlist 'length)
    unless (item-played-p playlist id)
      collect id
        into  available-item-ids
    finally
      (if available-item-ids
        (setf (slot-value playlist 'position)
          (elt available-item-ids
            (random (length available-item-ids))))
        (error "No unplayed item found.")))
  (values))

;;; -------------------------------------------------------

(defun select-random-item (playlist)
  "Selects a random item from the PLAYLIST and returns no value."
  (declare (type Playlist playlist))
  (setf (slot-value playlist 'position)
    (random
      (slot-value playlist 'length)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((playlist Playlist) (stream T))
  (declare (type Playlist    playlist))
  (declare (type destination stream))
  (format stream "Playlist")
  (loop
    for item-id
      of-type fixnum
      from    0
      below   (slot-value playlist 'length)
    for play-state
      of-type bit
      across  (slot-value playlist 'coverage-map)
    do
      (format stream "~&~:[~4t~;~2t* ~]Item #~d~@[ (played)~]"
        (= item-id (slot-value playlist 'position))
        item-id
        (plusp play-state))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of item event.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Item-Event ()
  ((playlist
    :initarg       :playlist
    :initform      (error "Missing playlist.")
    :reader        get-item-event-playlist
    :type          Playlist
    :documentation "The playlist being processed by the program.")
   (selected-item
    :initarg       :selected-item
    :initform      (error "Missing selected item.")
    :reader        get-item-event-selected-item
    :type          fixnum
    :documentation "The identifier of the item from the PLAYLIST being
                    played.")
   (command
    :initarg       :command
    :initform      (error "Missing command.")
    :reader        get-item-event-command
    :type          Command
    :documentation "The command whose procession instigated this
                    event."))
  (:documentation
    "The ``Item-Event'' class serves in the encapsulation of an item
     handler callback function's aefauld argument, which, invoked upon
     an item's processing, amplects the underlying playlist, the just
     selected and played item, and the command responsible for the
     event's instigation."))

;;; -------------------------------------------------------

(defun make-item-event (playlist selected-item command)
  "Creates and returns a new ``Item-Event'' which communicates the
   playing of the SELECTED-ITEM from the PLAYLIST following the
   COMMAND's instigation."
  (declare (type Playlist playlist))
  (declare (type fixnum   selected-item))
  (declare (type Command  command))
  (the Item-Event
    (make-instance 'Item-Event
      :playlist      playlist
      :selected-item selected-item
      :command       command)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of item handler operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-item-handler ((event-name) &body body)
  "Creates and returns a fresh function in a mode concinnous with the
   ``item-handler'' callbeck specification, its aefauld formal parameter
   yclept by the EVENT-NAME, its operations appropriated from the BODY
   forms, returning the desinent form's results."
  `(the function
     #'(lambda (,event-name)
         (declare (type Item-Event ,event-name))
         (declare (ignorable       ,event-name))
         ,@body)))

;;; -------------------------------------------------------

(defun make-default-item-handler ()
  "Returns a new function which represents the default ``item-handler'',
   printing upon each invocation the selected item's index."
  (the function
    (define-item-handler (event)
      (format T "~&~d"
        (get-item-event-selected-item event)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing Playlist program.")
    :type          playlist-program
    :documentation "The Playlist instruction sequence to evaluate.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The index into the PROGRAM of the currently
                    processed command.")
   (playlist
    :initform      (make-playlist 0)
    :reader        get-playlist
    :type          Playlist
    :documentation "Entrusted with the tracks' castaldy, incorporating
                    in this dever the played member's status.")
   (item-handler
    :initarg       :item-handler
    :initform      (make-default-item-handler)
    :type          item-handler
    :documentation "The callback function to invoke upon each playlist
                    item selection."))
  (:documentation
    "The ``Interpreter'' class is apportioned the onus of accompassing
     actual operative value to a sequence of Playlist instructions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Generates and fresh playlist, stores the same in the INTERPRETER,
   and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'playlist)
    (make-playlist
      (length
        (slot-value interpreter 'program))))
  (values))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type playlist-program program))
    (declare (type fixnum           ip))
    (when (array-in-bounds-p program ip)
      (incf ip)))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the Playlist program consigned to the
   INTERPRETER's castaldy has been executed completely, returning on
   confirmation a ``boolan'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type playlist-program program))
    (declare (type fixnum           ip))
    (the boolean
      (not (array-in-bounds-p program ip)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command designated by the INTERPRETER's instruction
   pointer (IP) position."
  (declare (type Interpreter interpreter))
  (with-slots (program ip) interpreter
    (declare (type playlist-program program))
    (declare (type fixnum           ip))
    (the Command
      (aref program ip))))

;;; -------------------------------------------------------

(defun invoke-item-handler (interpreter)
  "Invokes the INTERPRETER's item handler and returns no value."
  (declare (type Interpreter interpreter))
  (funcall
    (slot-value interpreter 'item-handler)
    (make-item-event
      (slot-value interpreter 'playlist)
      (get-playlist-position
        (slot-value interpreter 'playlist))
      (get-current-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun play-current-item (interpreter)
  "Plays the current item, invokes the INTERPRETER's item handler,
   designates the respective track as played, and returns no value."
  (declare (type Interpreter interpreter))
  (invoke-item-handler interpreter)
  (mark-current-item-as-played
    (slot-value interpreter 'playlist))
  (values))

;;; -------------------------------------------------------

(defgeneric dispatch-command (interpreter play-mode play-condition)
  (:documentation
    "Evaluates the combination of the PLAY-MODE and the PLAY-CONDITION
     in the INTERPRETER's context and returns no value."))

;;; -------------------------------------------------------

(defmacro define-command-dispatch
    ((interpreter-variable play-mode play-condition)
     &body body)
  "Defines an implementation of the generic function
   ``dispatch-command'', norning the first argument by the
   INTERPRETER-VARIABLE, selecting for the second an automatically
   generated agnomination, while specializing on an ``eql''-equality
   with the PLAY-MODE, also nevening the third variable in a
   programmatic way, with an ``eql''-dispatch on the PLAY-CONDITION,
   evaluates the BODY forms, and returns no value.
   ---
   The first BODY form, if resolving to a string object, will be the
   target of a documentation string role's adhibition, and will be
   reappropriated for this purpose."
  (let ((play-mode-variable      (gensym))
        (play-condition-variable (gensym)))
    (declare (type symbol play-mode-variable))
    (declare (type symbol play-condition-variable))
    `(defmethod dispatch-command
         ((,interpreter-variable    Interpreter)
          (,play-mode-variable      (eql ,play-mode))
          (,play-condition-variable (eql ,play-condition)))
       (declare (type Interpreter     interpreter))
       (declare (ignorable            interpreter))
       (declare (type play-mode      ,play-mode-variable))
       (declare (ignore              ,play-mode-variable))
       (declare (type play-condition ,play-condition-variable))
       (declare (ignore              ,play-condition-variable))
       ,@body
       (values))))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :looping :none)
  "Unconditionally selects the first item from the INTERPRETER's
   playlist and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (select-first-item playlist)
    (play-current-item interpreter))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :looping :played)
  "Plays the first item in the INTERPRETER's playlist if the same has
   not yet been played and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-first-unplayed-item playlist)
      (play-current-item          interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :looping :unique)
  "Plays the first item in the INTERPRETER's playlist if not all items
   have been played yet and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-first-item playlist)
      (play-current-item interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :sequential :none)
  "Unconditionally selects the next item from the INTERPRETER's playlist
   and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (select-next-item  playlist)
    (play-current-item interpreter))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :sequential :played)
  "Selects the next unplayed item from the INTERPRETER's playlist and
   returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-next-unplayed-item playlist)
      (play-current-item         interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :sequential :unique)
  "Selects the next item from the INTERPRETER's playlist only if not all
   items have been played yet and returns in no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-next-item  playlist)
      (play-current-item interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :random :none)
  "Unconditionally selects a random item from the INTERPRETER's playlist
   and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (select-random-item playlist)
    (play-current-item  interpreter))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :random :played)
  "Randomly selects an item not yet played from the INTERPRETER's
   playlist and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-random-unplayed-item playlist)
      (play-current-item           interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(define-command-dispatch (interpreter :random :unique)
  "Randomly selects an item from tye INTERPRETER's playlist only if not
   all items have been played yet and returns no value."
  (with-slots (playlist) interpreter
    (declare (type Playlist playlist))
    (unless (all-items-played-p playlist)
      (select-random-item playlist)
      (play-current-item  interpreter)))
  (advance-program interpreter))

;;; -------------------------------------------------------

(defun process-command (interpreter command)
  "Evaluates the COMMAND in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Command     command))
  (dispatch-command interpreter
    (get-command-play-mode      command)
    (get-command-play-condition command))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Playlist program consigned to the INTERPRETER's
   castaldy and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Playlist
    (code
     &optional (item-handler (make-default-item-handler)))
  "Interprets the piece of Playlist source CODE, optionally employing
   the specific ITEM-HANDLER for its progress' notification, and returns
   no value."
  (declare (type string       code))
  (declare (type item-handler item-handler))
  (interpret-program
    (make-instance 'Interpreter
      :program
        (extract-commands
          (make-scanner code))
      :item-handler item-handler))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Play each of the octuple track items once in an aleatory order.
(interpret-Playlist
  "Random Played
   Random Played
   Random Played
   Random Played
   Random Played
   Random Played
   Random Played
   Random Played")

;;; -------------------------------------------------------

;; An example of iterance.
(interpret-Playlist
  "Looping Unique
   Looping Unique
   Looping Unique
   Looping Unique
   Looping Unique
   Looping Unique
   Looping Unique
   Looping Unique")

;;; -------------------------------------------------------

;; Play the eight available items in their given order.
(interpret-Playlist
  "Sequential
   Sequential
   Sequential
   Sequential
   Sequential
   Sequential
   Sequential
   Sequential")

;;; -------------------------------------------------------

;; Play the eight available items in their given order, printing, by
;; mediation of a bespoke item handler, selected identifiers in lieu of
;; the track indices.
(interpret-Playlist
  "Sequential
   Sequential
   Sequential
   Sequential"
  (define-item-handler (event)
    (format T "~&~[First item~;~
                   Second item~;~
                   Third item~;~
                   Fourth item~:;~
                   Further item~]"
      (get-item-event-selected-item event))))
