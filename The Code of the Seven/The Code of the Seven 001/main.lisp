;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "The Code of the Seven", invented by the Esolang user
;; "Robgero" and presented on February 26th, 2018, its purpose being a
;; modulation of Urban Mueller's "brainfuck" by an epic diction's
;; adminicle.
;; 
;; 
;; Concept
;; =======
;; The "The Code of the Seven" programming language constitutes an
;; equivalency to brainfuck, its instruction set's vocabulary being
;; subject to a bartery of its sire's laconic expression with terms of
;; epic evocation.
;; 
;; 
;; Instructions
;; ============
;; Establishing a substitution-based variation on brainfuck, "The Code
;; of the Seven" enjoys an equipollence derived from its stock-father's
;; dation.
;; 
;; == OVERVIEW ==
;; An apercu's adduction shall serve in a cursory mete of gnarity's
;; adhibition anent the language's competences:
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   Father   | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   Mother   | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   Maiden   | Increments the current cell value by one. Upon a
;;            | transcendence beyond its upper bourne of 255, the cell
;;            | state wraps to the minimum of zero (0).
;;   ..................................................................
;;   Cron     | Decrements the current cell value by one. Upon a
;;            | transcendence beyond the lower bourne of zero (0), the
;;            | cell state wraps around to the maximum of 255.
;;   ..................................................................
;;   Warrior  | Prints the character whose ASCII code equals the
;;            | current cell value to the standard output.
;;   ..................................................................
;;   Smith    | Queries the standard input for a character and stores
;;            | its ASCII code in the current cell.
;;   ..................................................................
;;   Stranger | If the current cell value equals zero (0), moves the
;;            | instruction pointer (IP) forward to the position
;;            | immediately succeeding the matching "R'hllor"
;;            | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   R'hllor  | If the current cell value does not equal zero (0),
;;            | moves the instruction pointer (IP) back to the position
;;            | immediately succeeding the matching "Stranger"
;;            | instruction; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-07-12
;; 
;; Sources:
;;   [esolang2023TheCodeoftheSeven]
;;   The Esolang contributors, "The Code of the Seven", March 6th, 2023
;;   URL: "https://esolangs.org/wiki/The_Code_of_the_Seven"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type norned by the TYPE-NAME, its formal parameters
   specified via the LAMBDA-LIST, and founded upon a predicate the
   subject of its docimasy being agnominated through the
   CANDIDATE-VARIABLE, evaluating the BODY forms, the desinent form's
   primary value furnishing the assessment's verdict, construing a
   generalized boolean \"true\" value as a confirmation, otherwise, for
   \"false\", imputing a rejection."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name ,lambda-list
        ,(or (and (stringp (first body))
                  (pop body))
             "")
        (let ((,predicate-variable (gensym)))
          (declare (type symbol ,predicate-variable))
          (setf (symbol-function ,predicate-variable)
            #'(lambda (,candidate-variable)
                (declare (type T    ,candidate-variable))
                (declare (ignorable ,candidate-variable))
                ,@body))
          `(satisfies ,,predicate-variable)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-bespoke-type hash-table-of (candidate
                                    &optional
                                      (key-type   T)
                                      (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   answers to a VALUE-TYPE, both defaulting to the comprehensive ``T''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (typep key key-type)
             (typep value value-type)))))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' defines a mapping from the
   \"The Code of the Seven\" vocabulary to brainfuck's instruction
   tokens, realized as a hash table whose string keys provide the
   former language's items, while the latter identifiers are furnished
   by the entry value characters."
  '(hash-table-of string character))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   matching jump points in a brainfuck program, mediated by adminiculum
   of their zero-based locations inside the same."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, and thus a commorant of the closed interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype cell-table ()
  "The ``cell-table'' type defines a sparse vector of unsigned
   byte-valued cells, amenable to signed integer subscripts, and embued
   with their reification by adminiculum of a hash table, the keys of
   which accommodate the ``integer'' cell positions, affiliated with
   ``octet'' values."
  '(hash-table-of integer octet))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which incorporates in its amplectation such instances as
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a \"generalized boolean\",
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE constitutes a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-whitespace (source start)
  "Proceeding from the START position into the SOURCE, returns the
   location of the nearest whitespace character, or, upon its lacuna,
   responds with the ``NIL'' value."
  (declare (type string source))
  (declare (type fixnum start))
  (the (or null fixnum)
    (position-if #'whitespace-character-p source :start start)))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces and returns the position into
   the SOURCE immediately succeeding the omitted parcel."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, locates and
   returns the index immediately succeeding the accolent word's desinent
   character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (locate-whitespace source start)
        (length source))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, extracts the
   nearest word, contingently skipping any prevenient whitespaces, and
   returns two values:
     (1) The detected word as a fresh string.
     (2) The position into the SOURCE immediately succeeding the
         consumed word."
  (declare (type string source))
  (declare (type fixnum start))
  (let* ((word-start-position
          (skip-whitespaces source start))
         (word-end-position
          (locate-end-of-word source word-start-position)))
    (declare (type fixnum word-start-position))
    (declare (type fixnum word-end-position))
    (the (values string fixnum)
      (values
        (subseq source word-start-position word-end-position)
        word-end-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of translation table.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +DICTIONARY+))

;;; -------------------------------------------------------

(defparameter +DICTIONARY+
  (make-hash-table :test #'equal)
  "Equiparates the recognized \"The Code of the Seven\" identifiers with
   the corresponding brainfuck instruction tokens.")

;;; -------------------------------------------------------

(flet ((register-tocs-word (tocs-word brainfuck-equivalent)
        "Associates the \"The Code of the Seven\" identifier TOCS-WORD
         with the BRAINFUCK-EQUIVALENT in the +DICTIONARY+ and returns
         no value."
        (declare (type simple-string tocs-word))
        (declare (type character     brainfuck-equivalent))
        (setf (gethash tocs-word +DICTIONARY+) brainfuck-equivalent)
        (values)))
  (register-tocs-word "Father"   #\>)
  (register-tocs-word "Mother"   #\<)
  (register-tocs-word "Maiden"   #\+)
  (register-tocs-word "Crone"    #\-)
  (register-tocs-word "Warrior"  #\.)
  (register-tocs-word "Smith"    #\,)
  (register-tocs-word "Stranger" #\[)
  (register-tocs-word "R'hllor"  #\])
  (values))

;;; -------------------------------------------------------

(defun translate-The-Code-of-the-Seven-word (tcots-word)
  "Returns the brainfuck instruction token answering to the
   \"The Code of the Seven\" word TCOTS-WORD, or signals an error of an
   unspecified type upon its disrespondency."
  (declare (type string tcots-word))
  (the character
    (or (gethash tcots-word +DICTIONARY+)
        (error "You wield no valid tongue in ~s." tcots-word))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of translator.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-The-Code-of-the-Seven (source &optional (sink NIL))
  "Translates a piece of \"The Code of the Seven\" SOURCE code into the
   equivalent brainfuck program, writes the same to the SINK, and
   returns for a non-``NIL'' SINK the ``NIL'' value, otherwise
   responding with a fresh string comprehending the output."
  (declare (type string      source))
  (declare (type destination sink))
  (the (or null string)
    (if sink
      (loop
        with position of-type fixnum = 0
        while (< position (length source)) do
          (multiple-value-bind (next-word new-position)
              (read-word source position)
            (declare (type string next-word))
            (declare (type fixnum new-position))
            (write-char
              (translate-The-Code-of-the-Seven-word next-word)
              sink)
            (setf position
              (skip-whitespaces source new-position))))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (translate-The-Code-of-the-Seven source output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table builder.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun connect-jump-points (jump-table start-point end-point)
  "Affiliates the START-POINT and END-POINT in a bilateral fashion,
   stores the association in the JUMP-TABLE, and returns no value."
  (declare (type jump-table jump-table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf (gethash start-point jump-table) end-point
         (gethash end-point   jump-table) start-point)
  (values))

;;; -------------------------------------------------------

(defun build-jump-table (brainfuck-code)
  "Creates and returns a fresh jump table for the piece of
   BRAINFUCK-CODE."
  (declare (type string brainfuck-code))
  (let ((jump-table        (make-hash-table :test #'eql))
        (jump-start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-start-points))
    (loop
      for token    of-type character across brainfuck-code
      and position of-type fixnum    from   0 by 1
      
      if (char= token #\[) do
        (push position jump-start-points)
      else if (char= token #\]) do
        (if jump-start-points
          (connect-jump-points jump-table
            (pop jump-start-points)
            position)
          (error "Unmatched jump end point at position ~d." position))
      end
      
      finally
        (when jump-start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length jump-start-points) jump-start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun get-jump-destination (jump-table jump-point)
  "Returns the obverse jump position answering to the JUMP-POINT in the
   JUMP-TABLE, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type jump-table jump-table))
  (declare (type fixnum     jump-point))
  (the fixnum
    (or (gethash jump-point jump-table)
        (error "No jump destination associated with the position ~d."
          jump-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements a \"The Code of the Seven\" program's
   memory as a bilaterally infinite tape of unsigned byte-valued cells,
   the currently active amenable instance being determined by a motile
   cell pointer."
  (cells   (make-hash-table :test #'eql)
           :type      cell-table
           :read-only NIL)
  (pointer 0
           :type      integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the unsigned byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the octet
    (gethash (memory-pointer memory) (memory-cells memory) 0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's currently, contingently preceded
   by its wrapping in order to accommodate the valid unsigned byte range
   of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf (gethash (memory-pointer memory) (memory-cells memory))
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (memory)
  "Increments the MEMORY's current cell value by one, contingently
   wrapping around its state in order to accommodate the valid unsigned
   byte range of [0, 255], and returns no value."
  (declare (type Memory memory))
  (incf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (memory)
  "Decrements the MEMORY's current cell value by one, contingently
   wrapping around its state in order to accommodate the valid unsigned
   byte range of [0, 255], and returns no value."
  (declare (type Memory memory))
  (decf (current-cell-value memory))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (memory)
  "Translates the MEMORY's cell pointer one step to the right and
   returns no value."
  (declare (type Memory memory))
  (incf (memory-pointer memory))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (memory)
  "Translates the MEMORY's cell pointer one step to the left and
   returns no value."
  (declare (type Memory memory))
  (decf (memory-pointer memory))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of brainfuck interpreter.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-brainfuck (code)
  "Interprets the piece of brainfuck source CODE and returns no value."
  (declare (type string code))
  (let ((ip         0)
        (jump-table (build-jump-table code))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (loop while (< ip (length code)) do
      (case (char code ip)
        (#\+
          (increment-current-cell memory))
        (#\-
          (decrement-current-cell memory))
        (#\>
          (move-cell-pointer-right memory))
        (#\<
          (move-cell-pointer-left memory))
        (#\,
          (format T "~&>> ")
          (finish-output)
          (setf (current-cell-value memory)
            (char-code
              (read-char NIL NIL #\Null)))
          (clear-input))
        (#\.
          (write-char
            (code-char
              (current-cell-value memory)))
          (finish-output))
        (#\[
          (when (zerop (current-cell-value memory))
            (setf ip
              (get-jump-destination jump-table ip))))
        (#\]
          (unless (zerop (current-cell-value memory))
            (setf ip
              (get-jump-destination jump-table ip))))
        (otherwise NIL))
      (incf ip)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "The Code of the Seven" interpreter.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-The-Code-of-the-Seven (code)
  "Interprets the piece of \"The Code of the Seven\" source CODE and
   returns no value."
  (declare (type string code))
  (interpret-brainfuck
    (translate-The-Code-of-the-Seven code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program which terminates on a "null character" input.
(interpret-The-Code-of-the-Seven
  "Smith
   Stranger
     Warrior
     Smith
   R'hllor")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-The-Code-of-the-Seven
  "
  Maiden
  Stranger
  Crone
  Crone
  Father
  Crone
  Stranger
  Father
  Father
  Maiden
  Father
  Crone
  Crone
  Crone
  Crone
  Crone
  Mother
  Mother
  R'hllor
  Mother
  Crone
  Crone
  Mother
  Crone
  Crone
  Crone
  R'hllor
  Father
  Crone
  Warrior
  Father
  Father
  Father
  Maiden
  Warrior
  Father
  Father
  Warrior
  Warrior
  Maiden
  Maiden
  Maiden
  Stranger
  Warrior
  Father
  R'hllor
  Mother
  Mother
  Mother
  Mother
  Warrior
  Maiden
  Maiden
  Maiden
  Warrior
  Crone
  Crone
  Crone
  Crone
  Crone
  Crone
  Warrior
  Mother
  Mother
  Crone
  Warrior
  Father
  Father
  Father
  Father
  Maiden
  Warrior
  ")
