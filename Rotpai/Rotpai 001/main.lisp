;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Rotpai", invented by the Esolang user
;; "Mipinggfxgbtftybfhfyhfn" and presented on July 4th, 2019, its
;; haecceity's entelechy manifests in the internal rotation and external
;; rearrangement of characters pairs.
;; 
;; 
;; Concept
;; =======
;; The Rotpai programming language founds upon the notions of character
;; pairs, the ordonnance's modulation constituting a dependency upon
;; the immediate neighborhood, homologating internal rotations, as well
;; as their location's cambistry among eligible near-dwellers.
;; 
;; == ROTPAI PROGRAMS: LINES OF CHARACTER PAIRS.
;; Each Rotpai program's conformation proceeds by means of zero or more
;; lines, any siccan a separately operating entity. Every line's
;; constitution tallies zero or more character pairs, their identifiers'
;; sepiment one or more spaces.
;; 
;; == PAIRS ENDING IN PERIODS CONTINUE LINES ==
;; A pair whose right symbol presents a period ("."), and which occurs
;; at a line's immediate desinence, serves in the designation of the
;; line's continuation into the next horizontal expanse.
;; 
;; The code fragment
;; 
;;   ab cd e.
;;   fg hi
;; 
;; as an illustrative forbisen, does not represent two lines, but an
;; aefauld specimen, tantamount in its formation, by the "e." pair's
;; mediation, to
;; 
;;   ab cd e. fg hi
;; 
;; A counterdistinguishment, however, shall be the following emphasis'
;; cynosure, that periods on a pair's sinistral side, as well as such
;; not partaking of the line's desinence, do not involve this
;; potential's entelechy. The code sample
;; 
;;   ab cd .e
;;   fg hi
;; 
;; retains its verbatim two-lines structure, as ".e" incites no
;; coalescence along the bournes.
;; 
;; Siclike, the allocation of "e." in the center of this example
;; 
;;   ab e. cd
;;   fg hi
;; 
;; lacks the nexible puissance of a trailing period.
;; 
;; == ROTPAI PROGRAMS: INTERNAL ROTATION OR PAIR SUBSTITUTIONS ==
;; A Rotpai program iterates through its lines and applies to the
;; following principle to each of its pairs in succession:
;; 
;;   (1) NO MATCHING NEIGHBORS: ROTATE INTERNALLY
;;       If the current pair p[i] does not match any of its neighbors
;;       p[i-1]'s and p[i+1]'s adjacent character it is rotated
;;       internally; that is, its characters are swapped.
;;   
;;   (2) ONE MATCHING NEIGHBOR: EXCHANGE POSITIONS
;;       If the current pair p[i] matches either its sinistral neighbor
;;       p[i-1]'s second character or its dextral near-dweller p[i+1]'s,
;;       left character, the two compatible pairs exchange their
;;       positions.
;;       Note that this does not hold for the case of both lateralities'
;;       convenableness, which please see instead below (3).
;;   
;;   (3) TWO MATCHING NEIGHBORS: APPLY NO EFFECT
;;       If the current pair p[i] matches both of its neighbors p[i] and
;;       and p[i+1], no further action is applied.
;; 
;; A pair's character immediately abutting an edge is considered to
;; always match.
;; 
;; The program terminates in the case of no pair's eligibility for
;; further rotations.
;; 
;; The following pseudocode segment shall apply itself to the
;; principle's elucidation, entalented with a more stringent competence:
;; 
;;   { Exchanges the positions of the two characters forming the }
;;   { pair P.                                                   }
;;   procedure rotateInternally (p)
;;     Input:
;;       p: The pair whose two elements shall be rotated; with
;;            p = (p[L], p[R])
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       (p[L], p[R]) <- (p[R], p[L])
;;   end procedure
;;   
;;   
;;   { Evaluates and contingently modifies the position or internal }
;;   { constitution of the pair P[i].                               }
;;   procedure processPair (p[i])
;;     Input:
;;       p[i]:   The pair to evaluate; with p[i] being the tuple
;;                 (p[i,L], p[i,R]).
;;       p[i-1]: The pair immediately to the left  of p[i]; with p[i-1]
;;               being the tuple
;;                 (p[i-1,L], p[i-1,R]).
;;       p[i+1]: The pair immediately to the right of p[i]; with p[i+1]
;;               being the tuple
;;                 (p[i+1,L], p[i+1,R]).
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       { Pair p[i] touches left or both edges? => No action. }
;;       if p[i-1] = nil then
;;         do nothing
;;       { Pair p[i] touches right or both edges? => No action. }
;;       else if p[i+1] = nil then
;;         do nothing
;;       { Pair p[i] possedes two neighbors? => Indagate further. }
;;       else
;;         let matchesOnLeftSide  <- (p[i,L] = p[i-1,R])
;;         let matchesOnLeftRight <- (p[i,R] = p[i+1,L])
;;         let matchesOnBothSides <- matchesOnLeftSide
;;                                   and matchesOnRightSide
;;         
;;         if matchesOnBothSides then
;;           do nothing
;;         else if matchesOnLeftSide then
;;           swap positions of p[i] and p[i-1]
;;         else if matchesOnRightSide then
;;           swap positions of p[i] and p[i+1]
;;         else
;;           rotateInternally(p[i])
;;         end if
;;       end if
;;   end procedure
;;   
;;   
;;   procedure processLine (line)
;;     Input:
;;       line: The line of character pairs to process.
;;     
;;     Output:
;;      None.
;;     
;;     Process:
;;       for each pair p[i] in line
;;         processPair(p[i])
;;       end for
;;   end procedure
;;   
;;   
;;   procedure executeProgram (lines)
;;     Input:
;;       lines: An ordered list of pairs which comprise the Rotpai
;;              program.
;;     
;;     Output:
;;       None.
;;     
;;     Process:
;;       while there exists a pair which can be rotated
;;         for each line l[i] in lines
;;           processLine(l[i])
;;         end for
;;       end while
;;   end procedure
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation in the programming language Common Lisp
;; avails itself with the attendance to its onus by a doubly linked list
;; data structure's accommodation to the Rotpai concept's particular
;; diorisms, namely the perquisition and modification of neighborhood
;; relationships.
;; 
;; Pursuant to this intent, the character pairs are implemented in a
;; fashion akin to doubly linked list nodes, endowed with the castaldy
;; of their sinistral and dextral viciniage.
;; 
;; A line, as an arbitrary tally of pairs' abode, applies itself to this
;; duty siclike to a doubly linked list, its furnishment that entailing
;; an organized and consistent adit to the interrelated pairs.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-04-01
;; 
;; Sources:
;;   [esolang2020Rotpai]
;;   The Esolang contributors, "Rotpai", December 31st, 2020
;;   URL: "https://esolangs.org/wiki/Rotpai"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which embraces, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for
   which holds the comprehensive default of ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype matching-side ()
  "The ``matching-side'' type enumerates the contingies for a character
   pair's lateral matches."
  '(member :none :left :right :both))

;;; -------------------------------------------------------

(deftype line-handler ()
  "The ``line-handler'' type defines a callback function responsive to
   ``Line-Event'' instances as the aefauld argument, the return value,
   if any, being ignored.
   ---
   The function signature, as a corollary, complies to:
     lambda (line-event) => ignored-result"
  '(function (Line-Event) *))

;;; -------------------------------------------------------

(deftype pair-processed-handler ()
  "The ``pair-processed-handler'' type define a callback function
   responsive to ``Pair-Processed-Event'' instances as the aefauld
   argument, the return value, if any, being ignored.
   ---
   The function signature, as a corollary, complies to:
     function (pair-processed-event) => ignored-result"
  '(function (Pair-Processed-Event) *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a veridical Boolean equivalency to the OBJECT as construed in
   the context of a \"generalized boolean\", returning for a non-``NIL''
   input a ``boolean'' value of ``T'', otherwise, for a ``NIL'' OBJECT,
   the ``NIL'' value."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of doubly linked pair.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Pair
  (:constructor make-pair (characters previous next)))
  "The ``Pair'' class encapsulates a character twain, realized akin to a
   node in a doubly linked list, composed of the maintained element and
   two references, one targeted at the optional predecessor pair, the
   other at the contingent successor."
  (characters (error "Missing character pair.")
              :type      (string 2)
              :read-only NIL)
  (previous   (error "Missing pair predecessor.")
              :type      (or null Pair)
              :read-only NIL)
  (next       (error "Missing pair successor.")
              :type      (or null Pair)
              :read-only NIL))

;;; -------------------------------------------------------

(defun rotate-pair (pair)
  "Swaps the PAIR's characters internally and returns no value."
  (declare (type Pair pair))
  (setf (pair-characters pair)
    (nreverse
      (pair-characters pair)))
  (values))

;;; -------------------------------------------------------

(defun get-left-character (pair)
  "Returns the first character stored in the PAIR."
  (declare (type Pair pair))
  (the character
    (char (pair-characters pair) 0)))

;;; -------------------------------------------------------

(defun get-right-character (pair)
  "Returns the second character stored in the PAIR."
  (declare (type Pair pair))
  (the character
    (char (pair-characters pair) 1)))

;;; -------------------------------------------------------

(defun pair-matches-predecessor-p (predecessor probed-pair)
  "Determines whether the PROBED-PAIR's first character matches the
   PREDECESSOR's second one, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Pair predecessor))
  (declare (type Pair probed-pair))
  (the boolean
    (get-boolean-value-of
      (char=
        (get-left-character  probed-pair)
        (get-right-character predecessor)))))

;;; -------------------------------------------------------

(defun pair-matches-successor-p (probed-pair successor)
  "Determines whether the PROBED-PAIR's second character matches the
   SUCCESSOR's first one, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type Pair probed-pair))
  (declare (type Pair successor))
  (the boolean
    (get-boolean-value-of
      (char=
        (get-right-character probed-pair)
        (get-left-character  successor)))))

;;; -------------------------------------------------------

(defun pair-continues-line-p (pair)
  "Determines whether the PAIR conditions the subsequent linebreak's
   omission by bearing in its second position a period (\".\"),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Pair pair))
  (the boolean
    (get-boolean-value-of
      (char= (char (pair-characters pair) 1)
             #\.))))

;;; -------------------------------------------------------

(defun copy-pair-characters (pair)
  "Returns a copy of the PAIR's character twissel."
  (declare (type Pair pair))
  (the (string 2)
    (copy-seq
      (pair-characters pair))))

;;; -------------------------------------------------------

(defmethod print-object ((pair Pair) (stream T))
  (declare (type Pair        pair))
  (declare (type destination stream))
  (format stream "(Pair element=~s)"
    (pair-characters pair)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of line.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Line ()
  ((header
    :initform      (make-pair "__" NIL NIL)
    :accessor      header
    :type          Pair
    :documentation "The header sentinel, this always being the front
                    pair in the doubly linked list, preceding any
                    veridical content pair.")
   (trailer
    :initform      (make-pair "__" NIL NIL)
    :accessor      trailer
    :type          Pair
    :documentation "The trailer sentinel, this always being the rear
                    pair in the doubly linked list, succeeding any
                    veridical content pair.")
   (size
    :initform      0
    :accessor      size
    :type          (integer 0 *)
    :documentation "The number of elements stored in the list."))
  (:documentation
    "The ``Line'' class furnishes an ordered list of character pairs
     whose foundry is established by the principles of the doubly linked
     list, each pair therein is encumbered with the castaldy of two
     pointers, one to its predecessor, the other to its successor, in
     addition to the stored element.
     ---
     This concrete implementation employs two sentinels, the \"header\"
     and the \"trailer\", both of which install pairs, the former of
     which always empight in front of the list, the latter occupying at
     any instant the rear's position, their combined purpose the
     facilitation of pair insertions and deletions."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((line Line) &key)
  "Initializes the LIST's header and trailer pairs, connects these, and
   returns no value."
  (declare (type Line line))
  (setf (header  line) (make-pair "__" NIL           NIL))
  (setf (trailer line) (make-pair "__" (header line) NIL))
  (setf (pair-next (header line)) (trailer line))
  (values))

;;; -------------------------------------------------------

(defun make-line ()
  "Creates and returns a new empty ``Line''."
  (the Line
    (make-instance 'Line)))

;;; -------------------------------------------------------

(defun empty-p (line)
  "Determines whether the doubly linked LIST is empty, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line line))
  (the boolean
    (get-boolean-value-of
      (zerop (size line)))))

;;; -------------------------------------------------------

(defun first-pair-p (line pair)
  "Determines whether the PAIR constitutes the front pair in the doubly
   linked LIST, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Line line))
  (declare (type Pair pair))
  (the boolean
    (get-boolean-value-of
      (eq pair
        (pair-next
          (header line))))))

;;; -------------------------------------------------------

(defun last-pair-p (line pair)
  "Determines whether the PAIR constitutes the rear pair in the doubly
   linked LIST, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Line line))
  (declare (type Pair pair))
  (the boolean
    (get-boolean-value-of
      (eq pair
        (pair-previous
          (trailer line))))))

;;; -------------------------------------------------------

(defun only-pair-p (line pair)
  "Determines whether the PAIR constitutes the only pair in the
   singleton doubly linked LIST, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Line line))
  (declare (type Pair pair))
  (the boolean
    (and (first-pair-p line pair)
         (last-pair-p  line pair))))

;;; -------------------------------------------------------

(defun get-first-pair (line)
  "Returns the first pair in the doubly linked LIST, or, if the same is
   empty, responds with the ``NIL'' value."
  (declare (type Line line))
  (the (or null Pair)
    (unless (empty-p line)
      (pair-next
        (header line)))))

;;; -------------------------------------------------------

(defun get-last-pair (line)
  "Returns the last pair in the doubly linked LIST, or, if the same is
   empty, responds with the ``NIL'' value."
  (declare (type Line line))
  (the (or null Pair)
    (unless (empty-p line)
      (pair-previous
        (trailer line)))))

;;; -------------------------------------------------------

(defun get-previous-pair (line pair)
  "Returns the pair preceding the given PAIR in the doubly linked LIST,
   or ``NIL'' if the same constitutes the first pair, that is, the one
   immediately succeeding the trailer."
  (declare (type Line line))
  (declare (type Pair pair))
  (the (or null Pair)
    (unless (first-pair-p line pair)
      (pair-previous pair))))

;;; -------------------------------------------------------

(defun get-next-pair (line pair)
  "Returns the pair succeeding the given PAIR in the doubly linked LIST,
   or ``NIL'' if the same constitutes the desinent pair, that is, the
   one immediately preceding the trailer."
  (declare (type Line line))
  (declare (type Pair pair))
  (the (or null Pair)
    (unless (last-pair-p line pair)
      (pair-next pair))))

;;; -------------------------------------------------------

(defun add-between (line new-element predecessor successor)
  "Creates a fresh pair for the NEW-ELEMENT, inserts the same in the
   LIST betwixt the extant PREDECESSOR and SUCCESSOR pairs, and returns
   the thus generated pair instance."
  (declare (type Line       line))
  (declare (type (string 2) new-element))
  (declare (type Pair       predecessor))
  (declare (type Pair       successor))
  (let ((new-pair (make-pair new-element predecessor successor)))
    (declare (type Pair new-pair))
    (setf (pair-next     predecessor) new-pair)
    (setf (pair-previous successor)   new-pair)
    (incf (size line))
    (the Pair new-pair)))

;;; -------------------------------------------------------

(defun add-to-rear (line new-element)
  "Inserts the NEW-ELEMENT at the doubly linked LIST's rear and returns
   the pair created for this purpose."
  (declare (type Line       line))
  (declare (type (string 2) new-element))
  (the Pair
    (add-between line new-element
      (pair-previous
        (trailer line))
      (trailer line))))

;;; -------------------------------------------------------

(defun swap-pair-elements (line first-pair second-pair)
  "Exchanges the characters stored in the FIRST-PAIR and the SECOND-PAIR
   of the doubly linked LIST and returns no value."
  (declare (type Line line))
  (declare (ignore    line))
  (declare (type Pair first-pair))
  (declare (type Pair second-pair))
  (rotatef
    (pair-characters first-pair)
    (pair-characters second-pair))
  (values))

;;; -------------------------------------------------------

(defun determine-matching-side (line probed-pair)
  "Returns the matching side for the pair contained in the LINE's
   PROBED-PAIR."
  (declare (type Line line))
  (declare (type Pair probed-pair))
  (let ((left-pair  (get-previous-pair line probed-pair))
        (right-pair (get-next-pair     line probed-pair)))
    (declare (type Pair           probed-pair))
    (declare (type (or null Pair) left-pair))
    (declare (type (or null Pair) right-pair))
    (the matching-side
      (cond
        ;; Touches both edges?
        ((only-pair-p line probed-pair)
          :both)
        ;; Touches left edge?
        ((not left-pair)
          :both)
        ;; Touches right edge?
        ((not right-pair)
          :both)
        ;; Matches both ends?
        ((and left-pair right-pair
              (pair-matches-predecessor-p left-pair   probed-pair)
              (pair-matches-successor-p   probed-pair right-pair))
          :both)
        ;; Matches left side?
        ((and left-pair
              (pair-matches-predecessor-p left-pair probed-pair))
          :left)
        ;; Matches right side?
        ((and right-pair
              (pair-matches-successor-p probed-pair right-pair))
          :right)
        ;; Matches no side?
        (T
          :none)))))

;;; -------------------------------------------------------

(defun line-completed-p (line)
  "Determines whether the LINE's pairs in their entirety have been
   rendered immune to the contingency of rotations, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Line line))
  (the boolean
    (get-boolean-value-of
      (loop
        for current-pair
          of-type (or null Pair)
          =       (get-first-pair line)
          then    (get-next-pair  line current-pair)
        while current-pair
        never
          (eq (determine-matching-side line current-pair) :none)))))

;;; -------------------------------------------------------

(defmethod print-object ((line Line) (stream T))
  (declare (type Line        line))
  (declare (type destination stream))
  (loop
    initially
      (format stream "(Line [")
    
    for current-pair
      of-type Pair
      =       (pair-next (header line))
      then    (pair-next current-pair)
    
    for first-pair-p
      of-type boolean
      =       T
      then    NIL
    
    until (eq current-pair (trailer line)) do
      (format stream "~:[, ~;~]~s" first-pair-p
        (pair-characters current-pair))
    
    finally
      (format stream "])")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal
   tab character, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   a species that subsumes the triad of space, horizontal tab, and
   newline, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char=             candidate #\Newline)
          (space-character-p candidate)))))

;;; -------------------------------------------------------

(defun pair-character-p (candidate)
  "Determines whether the CANDIDATE represents a valid constituent for a
   pair's diorism, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (whitespace-character-p candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer and parser.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position into the
   SOURCE of the first non-space character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-next-word (source start)
  "Proceeding from the START position into the SOURCE, returns the
   position of the nearest word's first character, or, if none such
   could be attested, the SOURCE's length instead."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, returns the
   position immediately succeeding a word's desinent character, or, if
   none such could be attested, the SOURCE's length instead."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun get-next-word (source start)
  "Proceeding from the START position into the SOURCE, extracts the
   nearest word and returns two values:
     (1) A string representation of the extracted word, which may be
         empty.
     (2) The position into the SOURCE immediately succeeding the segment
         occupied by the extracted word."
  (declare (type string source))
  (declare (type fixnum start))
  (let* ((word-start (locate-next-word   source start))
         (word-end   (locate-end-of-word source word-start)))
    (declare (type fixnum word-start))
    (declare (type fixnum word-end))
    (the (values string fixnum)
      (values
        (subseq source word-start word-end)
        word-end))))

;;; -------------------------------------------------------

(defun has-valid-length-for-pair-p (word)
  "Determines whether the WORD's character tally renders its conable as
   a pair representative, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (declare (type string word))
  (the boolean
    (get-boolean-value-of
      (= (length word) 2))))

;;; -------------------------------------------------------

(defun valid-pair-identifier-p (word)
  "Determines whether the WORD's constituents in their entirety subsume
   into that species of characters admissible for a pair's diorism,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string word))
  (the boolean
    (get-boolean-value-of
      (every #'pair-character-p word))))

;;; -------------------------------------------------------

(defun validate-pair-template (word)
  "Determines whether the WORD constitutes a valid candidate for the
   transcription into a ``Pair'' object, returning on confirmation the
   WORD itself, otherwise responding with an error of an unspecified
   type."
  (declare (type string word))
  (the string
    (and
      (or (has-valid-length-for-pair-p word)
          (error "The length of the token ~s, tallying ~d character~:p, ~
                  renders it ineligible as a pair of two constituents."
            word (length word)))
      (or (valid-pair-identifier-p word)
          (error "One or both characters in the token ~s do not ~
                  belong to the set of valid pair characters."
            word))
      word)))

;;; -------------------------------------------------------

(defun parse-program (source)
  "Extracts from the piece of Rotpai SOURCE code a list of its lines and
   returns the same."
  (declare (type string source))
  (let ((lines                     NIL)
        (current-line              NIL)
        (continues-previous-line-p NIL))
    (declare (type (list-of Line) lines))
    (declare (type (or null Line) current-line))
    (declare (type boolean        continues-previous-line-p))
    (with-input-from-string (input-stream source)
      (declare (type string-stream input-stream))
      (loop
        for input-line
          of-type (or null string)
          =       (read-line input-stream NIL NIL)
        while input-line do
          ;; Shall not continue the previous line?
          ;; => Create and collect a new one.
          (unless continues-previous-line-p
            (push (make-line) lines)
            (setf current-line (first lines)))
          ;; Collect all pairs in the CURRENT-LINE.
          (let ((position (skip-spaces input-line 0)))
            (declare (type fixnum position))
            (loop while (< position (length input-line)) do
              (multiple-value-bind (next-word new-position)
                  (get-next-word input-line position)
                (declare (type string next-word))
                (declare (type fixnum new-position))
                (add-to-rear current-line
                  (validate-pair-template next-word))
                ;; Determine whether the current character pair
                ;; terminates in a period ("."), in which case the
                ;; subsequent line would coalesce with the current one.
                (setf continues-previous-line-p
                  (pair-continues-line-p
                    (get-last-pair current-line)))
                (setf position new-position))))
          ;; Discard lines comprehending no pair.
          (when (empty-p current-line)
            (pop lines))))
    (the (list-of Line)
      (nreverse lines))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of events.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Rotpai-Event
  "The ``Rotpai-Event'' interface establishes a common foundry for all
   classes pursuing the encapsulation of events begotten during the
   interpretation of a Rotpai program.")

;;; -------------------------------------------------------

(defstruct (Line-Event
  (:include Rotpai-Event))
  "The ``Line-Event'' class encapsulates the information appertaining to
   a line procession's inchoation or desinence."
  (line (error "Missing terminated line.")
        :type      Line
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Pair-Processed-Event
  (:include Rotpai-Event))
  "The ``Pair-Processed-Event'' class encapsulates the information
   appertaining to a character pair's transformation from a source to a
   destination state."
  (line      (error "Missing processed line.")
             :type      Line
             :read-only T)
  (pair      (error "Missing processed pair.")
             :type      Pair
             :read-only T)
  (old-state (error "Missing old state.")
             :type      (string 2)
             :read-only T)
  (action    (error "Missing adhibited action.")
             :type      matching-side
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of default handlers.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype line-handler           +DEFAULT-LINE-START-HANDLER+))
(declaim (ftype line-handler           +DEFAULT-LINE-END-HANDLER+))
(declaim (ftype pair-processed-handler +DEFAULT-PAIR-HANDLER+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-LINE-START-HANDLER+
  #'(lambda (event)
      (declare (type Line-Event event))
      (format T "~&Line ~a started."
        (line-event-line event)))
  "Implements a default line start handler, printing to the standard
   output a message incorporating the input line's content.")

(defparameter +DEFAULT-LINE-END-HANDLER+
  #'(lambda (event)
      (declare (type Line-Event event))
      (format T "~&Line ~a ended.~2%"
        (line-event-line event)))
  "Implements a default line end handler, printing to the standard
   output a message incorporating the input line's content.")

(defparameter +DEFAULT-PAIR-HANDLER+
  #'(lambda (event)
      (declare (type Pair-Processed-Event event))
      (format T "~&~2t~a -[~a]-> ~a"
        (pair-processed-event-old-state event)
        (pair-processed-event-action event)
        (pair-characters (pair-processed-event-pair event))))
  "Implements a default pair processing handler, printing to the
   standard output the pair's previous and current state, connected by
   the matching side, which is tantamount to the action applied to the
   original pair characters.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Interpreter
  "The ``Interpreter'' class applies itself to the evaluation of the
   line of character pairs."
  (lines                  (error "Missing lines.")
                          :type      (list-of Line)
                          :read-only T)
  (line-start-handler     +DEFAULT-LINE-START-HANDLER+
                          :type      line-handler
                          :read-only T)
  (line-end-handler       +DEFAULT-LINE-END-HANDLER+
                          :type      line-handler
                          :read-only T)
  (pair-processed-handler +DEFAULT-PAIR-HANDLER+
                          :type      pair-processed-handler
                          :read-only T))

;;; -------------------------------------------------------

(defun process-pair (line probed-pair)
  "Evaluates the PROBED-PAIR in the context of its LINE, contingently
   modifying its content and that of one of its neighbors, and returns
   no value."
  (declare (type Line line))
  (declare (type Pair probed-pair))
  (let ((matching-side (determine-matching-side line probed-pair))
        (old-state     (copy-pair-characters probed-pair)))
    (declare (type matching-side matching-side))
    (case matching-side
      (:none
        (rotate-pair probed-pair))
      
      (:left
        (swap-pair-elements line
          (get-previous-pair line probed-pair)
          probed-pair))
      
      (:right
        (swap-pair-elements line probed-pair
          (get-next-pair line probed-pair)))
      
      (:both
        NIL)
      
      (otherwise
        (error "Unrecognized matching side: ~s." matching-side)))
    
    (the (values (string 2) matching-side)
      (values old-state matching-side))))

;;; -------------------------------------------------------

(defun process-line (interpreter line)
  "Processes the LINE in the INTERPRETER's context and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type Line        line))
  (loop
    initially
      (funcall
        (interpreter-line-start-handler interpreter)
        (make-line-event :line line))
    
    for current-pair
      of-type (or null Pair)
      =       (get-first-pair line)
      then    (get-next-pair line current-pair)
    
    while current-pair do
      (multiple-value-bind (old-state action)
          (process-pair line current-pair)
        (declare (type (string 2)    old-state))
        (declare (type matching-side action))
        (funcall
          (interpreter-pair-processed-handler interpreter)
          (make-pair-processed-event
            :line      line
            :pair      current-pair
            :old-state old-state
            :action    action)))
    finally
      (funcall
        (interpreter-line-end-handler interpreter)
        (make-line-event :line line)))
  (values))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the Rotpai program maintained by the INTERPRETER
   is completed, that is, none of its lines' pairs can be rotated,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not (null
      (every #'line-completed-p
        (interpreter-lines interpreter))))))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  "Interprets the Rotpai program governed by the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (dolist (current-line (interpreter-lines interpreter))
      (declare (type Line current-line))
      (process-line interpreter current-line)))
  (values))

;;; -------------------------------------------------------

(defun interpret-Rotpai
    (code
     &key (line-start-handler     +DEFAULT-LINE-START-HANDLER+)
          (line-end-handler       +DEFAULT-LINE-END-HANDLER+)
          (pair-processed-handler +DEFAULT-PAIR-HANDLER+))
  "Interprets the piece of Rotpai source CODE, upon their specification
   registering the LINE-START-HANDLER, LINE-END-HANDLER, and
   PAIR-PROCESSED-HANDLER for the reception of notifications, and
   returns no value."
  (declare (type string                 code))
  (declare (type line-handler           line-start-handler))
  (declare (type line-handler           line-end-handler))
  (declare (type pair-processed-handler pair-processed-handler))
  (interpret-program
    (make-interpreter
      :lines                  (parse-program code)
      :line-start-handler     line-start-handler
      :line-end-handler       line-end-handler
      :pair-processed-handler pair-processed-handler))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Rotpai "ab cb cd")

;;; -------------------------------------------------------

(interpret-Rotpai "a. b.
                   d. .e")
