;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "ALMBARC12YO", invented by the Esolang user "Viba1" and
;; presented on December 13th, 2022, the proprium of which resides in
;; its reappropriation of Urban Mueller's brainfuck by a new donat's
;; dation, desumed from the computer game "Among Us".
;; 
;; 
;; Concept
;; =======
;; The ALMBARC12YO programming language constitutes a reformulation of
;; brainfuck, the foundry of which appertains to the octuple instruction
;; agnominations' supersession by a new repertoire of identifiers,
;; selected in allusion to the computer game "Among Us".
;; 
;; == ALMBARC12YO: A SARDONIC STEVENING ==
;; The language's name derives from the author's ironic delineation of
;; the product's conception in relation to the originator themself,
;; presenting the assessment of "[A] [L]anguage [M]ade [B]y [A] [R]eally
;; [C]lever [12] [Y]ear [O]ld".
;; 
;; 
;; Instructions
;; ============
;; ALMBARC12YO's operative contingency derives from a replication of
;; brainfuck's octuple instruction set, the point of deviation merely
;; reified in its identifiers' agnominations.
;; 
;; == OVERVIEW ==
;; An apercu's dever shall be a cursory gnarity's communication anent
;; the operative competences:
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   amogus   | Increments the current cell value by one (1). Upon the
;;            | admissible upper bourne's transgression, imposed by the
;;            | value 255, the new state wraps around to the lower
;;            | extremum of zero (0).
;;   ..................................................................
;;   imposter | Decrements the current cell value by one (1). Upon the
;;            | admissible lower bourne's transgression, imposed by the
;;            | value zero (0), the new state wraps around to the upper
;;            | extremum of 255.
;;   ..................................................................
;;   sus      | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   sussy    | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   us       | Prints the character whose ASCII code corresponds to
;;            | current cell value to the standard output.
;;   ..................................................................
;;   Example  | Queries the standard input for a character and stores
;;            | its ASCII code in the current cell.
;;   ..................................................................
;;   vent     | If the current cell contains the value zero (0),
;;            | moves the instruction pointer (IP) forward to the
;;            | position immediately succeeding the matching "among"
;;            | instruction. Otherwise proceeds as usual.
;;   ..................................................................
;;   among    | If the current cell does not contain the value zero
;;            | (0), moves the instruction pointer (IP) back to the
;;            | position immediately succeeding the matching "vent"
;;            | instruction. Otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; == ALMBARC12YO AND BRAINFUCK ==
;; Its status as a new expression of the brainfuck entheus vindicates
;; the following equiparation of the commands:
;; 
;;   -----------------------
;;   ALMBARC12YO | brainfuck
;;   ------------+----------
;;   amogus      | +
;;   .......................
;;   imposter    | -
;;   .......................
;;   sus         | >
;;   .......................
;;   sussy       | <
;;   .......................
;;   us          | .
;;   .......................
;;   Example     | ,
;;   .......................
;;   vent        | [
;;   .......................
;;   among       | ]
;;   -----------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-05-06
;; 
;; Sources:
;;   [esolang2023ALMBARC12YO]
;;   The Esolang contributors, "ALMBARC12YO", October 12th, 2023
;;   URL: "https://esolangs.org/wiki/ALMBARC12YO"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun every-element-of-type-p (probed-list expected-type)
  "Determines whether every element of the PROBED-LIST conforms to the
   EXPECTED-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type list probed-list))
  (declare (type T    expected-type))
  (the boolean
    (not (null
      (every
        #'(lambda (current-element)
            (declare (type T current-element))
            (typep current-element expected-type))
        probed-list)))))

;;; -------------------------------------------------------

(defmacro define-custom-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type founded upon the ``deftype'' infrasturcture
   and stevened by the TYPE-NAME, with the formal parameters
   appropriated in an ipsissima verba fashion from the LAMBDA-LIST, the
   object being subjected to a docimasy being norned via the
   CANDIDATE-VARIABLE, relying upon the evaluated BODY forms' desinent
   primary value for its assessment of the convenableness, interpreting
   a non-``NIL'' result as the predicate's satisfaction, otherwise, for
   a ``NIL'' response, imputing the candidate's failure to impose a
   compatibility.
   ---
   If resolving to a string object, the first BODY form is construed as
   the derived type's documentation string and reappropriated for this
   purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body)) (pop body))
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

(define-custom-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list whose componency enumerates an
   arbitrary cardinality of members, each in accord with the
   ELEMENT-TYPE, its default empighted on the generic sentinel ``*''."
  (and
    (listp candidate)
    (or
      (and (symbolp element-type)
           (eq      element-type '*))
      (every-element-of-type-p
        (the list candidate)
        element-type))))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, as an ordered list compact of zero or more entries, each such
   constitutes a cons cell, the sinistral compartment of which lends
   harborage to the key, complying to the KEY-TYPE, and the dextral
   moiety is dedicated to the allied value of the VALUE-TYPE, for both
   holds the default in the generic sentinel ``*''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(define-custom-type hash-table-of (candidate
                                   &optional (key-type   '*)
                                             (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose compass
   amplects zero or more entries, each key among the same complies to
   the KEY-TYPE and answers to a value of the VALUE-TYPE, for both
   holding the default of the generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and
          (or
            (and (symbolp key-type) (eq key-type '*))
            (typep key key-type))
          (or
            (and (symbolp value-type) (eq value-type '*))
            (typep value value-type))))))

;;; -------------------------------------------------------

(deftype command-table ()
  "The ``command-table'' defines a mapping of ALMBARC12YO identifiers to
   the represented brainfuck commands, realized as an association list,
   or alist, the keys of which are established via identifier name
   strings, while the values supply ``Command'' object encapsulations
   that entail both the ALMBARC12YO and the brainfuck agnominations."
  '(association-list-of simple-string Command))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an ALMBARC12YO or, paregal in its
   notions, brainfuck program as a one-dimensional simple array of zero
   or more commands."
  '(simple-array Command (*)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bilateral association betwixt the
   forward and back jump points in an ALMBARC12YO program, mediated by
   their zero-based indices into the respective program and realized in
   a hash table whose keys and values both assume fixnum objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of an
   octuple tally of accolent bits, and thus a commorant of the closed
   integral space [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype byte-vector ()
  "The ``byte-vector'' type defines a sparse vector of unsigned
   byte-valued cells, bourneless along both axes and indexed with signed
   integer subscripts, their realization proceeding by means of a hash
   table, assuming signed integer keys that answer to ``octet'' cell
   values."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Command".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (almbarc12yo-name brainfuck-name)))
  "The ``Command'' class applies itself to the encapsulation of an
   operation in the ALMBARC12YO or, equivalently, brainfuck programming
   language."
  (almbarc12yo-name (error "Missing ALMBARC12YO name.")
                    :type      simple-string
                    :read-only T)
  (brainfuck-name   (error "Missing brainfuck name.")
                    :type      character
                    :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of commands.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Command +INCREMENT-COMMAND+))
(declaim (type Command +DECREMENT-COMMAND+))
(declaim (type Command +MOVE-RIGHT-COMMAND+))
(declaim (type Command +MOVE-LEFT-COMMAND+))
(declaim (type Command +INPUT-COMMAND+))
(declaim (type Command +OUTPUT-COMMAND+))
(declaim (type Command +JUMP-FORWARD-COMMAND+))
(declaim (type Command +JUMP-BACK-COMMAND+))

;;; -------------------------------------------------------

(defparameter +INCREMENT-COMMAND+
  (make-command "amogus" #\+)
  "Represents a behest for the current cell value's incrementation.")

(defparameter +DECREMENT-COMMAND+
  (make-command "imposter" #\-)
  "Represents a behest for the current cell value's decrementation.")

(defparameter +MOVE-RIGHT-COMMAND+
  (make-command "sus" #\>)
  "Represents a behest for the cell pointer's dextral translation.")

(defparameter +MOVE-LEFT-COMMAND+
  (make-command "sussy" #\<)
  "Represents a behest for the cell pointer's sinistral translation.")

(defparameter +INPUT-COMMAND+
  (make-command "Example" #\,)
  "Represents a behest for a character's obtention from the standard
   input.")

(defparameter +OUTPUT-COMMAND+
  (make-command "us" #\.)
  "Represents a behest for the output of the current cell value in the
   form of its associated ASCII character.")

(defparameter +JUMP-FORWARD-COMMAND+
  (make-command "vent" #\[)
  "Represents a behest for a conditional forward jump.")

(defparameter +JUMP-BACK-COMMAND+
  (make-command "among" #\])
  "Represents a behest for a conditional back jump.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of command table.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type command-table +COMMANDS+))

;;; -------------------------------------------------------

(defparameter +COMMANDS+
  `(("amogus"   . ,+INCREMENT-COMMAND+)
    ("imposter" . ,+DECREMENT-COMMAND+)
    ("sussy"    . ,+MOVE-LEFT-COMMAND+)
    ("sus"      . ,+MOVE-RIGHT-COMMAND+)
    ("Example"  . ,+INPUT-COMMAND+)
    ("us"       . ,+OUTPUT-COMMAND+)
    ("vent"     . ,+JUMP-FORWARD-COMMAND+)
    ("among"    . ,+JUMP-BACK-COMMAND+))
  "Associates the recognized ALMBARC12YO identifiers with representative
   ``Command'' objects.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean equivalent of the OBJECT when construed in the role
   of a \"generalized boolean\", responding with a ``boolean'' value of
   ``T'' for a non-``NIL'' input, otherwise, in the case of the OBJECT's
   attainment of the ``NIL'' value, delivers the ``NIL'' constant."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substring-starts-at-p (source start expected-string)
  "Determines whether, proceeding from the START position into the
   SOURCE, the subsequent characters therein replicate the
   EXPECTED-STRING, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum start))
  (declare (type string expected-string))
  (the boolean
    (get-boolean-value-of
      (string= source expected-string
        :start1 start
        :end1   (min (+ start (length expected-string))
                     (length source))))))

;;; -------------------------------------------------------

(defun probe-command (source start)
  "Determines whether, proceeding from the START position into the
   SOURCE, any of the recognized ALMBARC12YO identifiers occur, and
   returns two values:
     (1) If an identifier could be recognized, the matching ``Command''
         representation thereof, otherwise ``NIL''.
     (2) If an identifier could be recognized, the position into the
         SOURCE immediately succeeding its occupied segment, otherwise
         the position following the START location."
  (declare (type string source))
  (declare (type fixnum start))
  (flet ((matches-identifier-p (probed-entry)
          "Determines whether PROBED-ENTRY's ALMBARC12YO identifier
           matches the SOURCE commencing from the START position,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type (cons simple-string Command) probed-entry))
          (the (or null Command)
            (and
              (substring-starts-at-p source start
                (command-almbarc12yo-name
                  (cdr probed-entry)))
              (cdr probed-entry)))))
    (let ((matching-command (some #'matches-identifier-p +COMMANDS+)))
      (declare (type (or null Command) matching-command))
      (the (values (or null Command) fixnum)
        (values matching-command
          (+ start
            (if matching-command
              (length (command-almbarc12yo-name matching-command))
              1)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-commands (source)
  "Extracts and returns from the ALMBARC12YO SOURCE code a
   one-dimensional simple array of its entailed commands."
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (the program
      (coerce
        (loop while (< position (length source)) append
          (multiple-value-bind (next-command new-position)
              (probe-command source position)
            (declare (type (or null Command) next-command))
            (declare (type fixnum            new-position))
            (setf position new-position)
            (when next-command
              (list next-command))))
        '(simple-array Command (*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-jump-table (program)
  "Supputates and returns for the ALMBARC12YO PROGRAM a jump table which
   connects its forward and back jump points in a bidirection fashion
   by their locations' adminiculum."
  (declare (type program program))
  (let ((jump-table        (make-hash-table :test #'eql))
        (jump-start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) jump-start-points))
    
    (loop
      for command  of-type Command across program
      and position of-type fixnum  from   0 by 1
      
      if (eq command +JUMP-FORWARD-COMMAND+) do
        (push position jump-start-points)
      else if (eq command +JUMP-BACK-COMMAND+) do
        (if jump-start-points
          (let ((start-point (pop jump-start-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (psetf (gethash start-point jump-table) end-point
                   (gethash end-point   jump-table) start-point))
          (error "Unmatched jump end point at position ~d." position))
      end
      
      finally
        (when jump-start-points
          (error "Unmatched jump start point~p at position~:p ~
                  ~{~d~^, ~}."
            (length jump-start-points)
            jump-start-points)))
    
    (the jump-table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Memory
  (:constructor make-memory ()))
  "The ``Memory'' class implements the program memory as a sparse vector
   of unsigned byte cells, operated upon by a cell pointer."
  (cells   (make-hash-table :test #'eql)
           :type      byte-vector
           :read-only T)
  (pointer 0
           :type      integer
           :read-only NIL))

;;; -------------------------------------------------------

(defun current-cell-value (memory)
  "Returns the byte value stored in the MEMORY's current cell."
  (declare (type Memory memory))
  (the octet
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value memory)
  "Stores the NEW-VALUE in the MEMORY's current cell, contingently
   preceded by its state's adjustment in order to respect the valid
   byte range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (setf
    (gethash
      (memory-pointer memory)
      (memory-cells   memory)
      0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-program (program)
  "Interprets the ALMBARC12YO PROGRAM and returns no value."
  (declare (type program program))
  (let ((ip         0)
        (jump-table (build-jump-table program))
        (memory     (make-memory)))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type Memory     memory))
    (symbol-macrolet
        ((current-command
          (the Command
            (aref program ip))))
      (declare (type Command current-command))
      
      (loop while (< ip (length program)) do
        (case current-command
          (#.+INCREMENT-COMMAND+
            (incf (current-cell-value memory)))
          
          (#.+DECREMENT-COMMAND+
            (decf (current-cell-value memory)))
          
          (#.+MOVE-RIGHT-COMMAND+
            (incf (memory-pointer memory)))
          
          (#.+MOVE-LEFT-COMMAND+
            (decf (memory-pointer memory)))
          
          (#.+OUTPUT-COMMAND+
            (write-char
              (code-char
                (current-cell-value memory))))
          
          (#.+INPUT-COMMAND+
            (format T "~&>> ")
            (finish-output)
            (setf (current-cell-value memory)
              (char-code
                (read-char NIL NIL 0)))
            (clear-input))
          
          (#.+JUMP-FORWARD-COMMAND+
            (when (zerop (current-cell-value memory))
              (setf ip
                (or (gethash ip jump-table)
                    (error "No back jump point associated with the ~
                            position ~d."
                      ip)))))
          
          (#.+JUMP-BACK-COMMAND+
            (unless (zerop (current-cell-value memory))
              (setf ip
                (or (gethash ip jump-table)
                    (error "No forward jump point associated with the ~
                            position ~d."
                      ip)))))
          
          (otherwise
            (error "Invalid command ~s at position ~d."
              current-command ip)))
        
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-ALMBARC12YO (code)
  "Interprets the piece of ALMBARC12YO source CODE and returns no
   value."
  (declare (type string code))
  (interpret-program
    (extract-commands code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-ALMBARC12YO
  "amogus vent imposter imposter sus imposter vent sus sus amogus sus
   imposter imposter imposter imposter imposter sussy sussy among sussy
   imposter imposter sussy imposter imposter imposter among sus imposter
   us sus sus sus amogus us sus sus us us amogus amogus amogus vent us
   sus among sussy sussy sussy sussy us amogus amogus amogus us imposter
   imposter imposter imposter imposter imposter us sussy sussy imposter
   us sus sus sus sus amogus us")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-ALMBARC12YO "Example vent us Example among")
