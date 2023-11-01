;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "!!", invented by the Esolang user "Cinnamony" and presented
;; on June 16th, 2023, the kenspeckle proprium of which wones in its
;; employment of Latin majuscles during the furnishment of several
;; popular programming tasks' solutions.
;; 
;; 
;; Concept
;; =======
;; The !! programming language' subscription to the joke language
;; subspecies of the esoteric realm propines its haecceity with a
;; ludibund ilk, the dioristic proprium commorant inwith its expression
;; of popular programming tasks entirely in Latin majuscles.
;; 
;; 
;; Instructions
;; ============
;; A quintuple coefficiency exhausts the !!'s language's instruction
;; set, employing for both input and output purposes the majuscular
;; case, if possible.
;; 
;; == OVERVIEW ==
;; An apercu's involvement shall serve as the warklume for a cursory
;; nortelry's adhibition concerning the language's operative facilities:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   HW      | Prints the message "HELLO WORLD" to the standard output.
;;   ..................................................................
;;   99      | Prints the lyrics of the song "99 Bottles of Beer" to
;;           | the standard output, reproducing any letter in its
;;           | majuscular form.
;;   ..................................................................
;;   CT      | Executes a one-time line-based cat program, reproducing
;;           | any letter in its majuscular form.
;;   ..................................................................
;;   FB      | Prints the FizzBuzz sequence with a counter ascending in
;;           | the closed interval [1, 100] to the standard output,
;;           | reproducing any letter in its majuscular form.
;;   ..................................................................
;;   ??      | Queries the user for a line of input, generates from the
;;           | same's words an acronym, and prints the result in
;;           | majuscular form to the standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The nimious brevity that is lend a woning in !!'s protolog encumbers
;; the language with a few ambiguous passage, whence a peisant excerpt
;; shall be propined a treatise.
;; 
;; == WHICH FUNCTION DOES THE "??" COMMAND PURSUE? ==
;; An adversary to the pellucid interpretation adhibited to the
;; circumambient instructions, the "??" operation's purpose retains a
;; nithdale's simulacrum in its crepuscular guise, accompanied merely
;; by the "ACRONYM" message in an attempt --- earnest or Barmecide ---
;; for a sense's purveyance.
;; 
;; It has been adjudged to attend to such imputation as which involves
;; an acronym program's nature in the identification: The user shall be
;; queried for a line of text, for the same the acronym is established,
;; and ultimately printed to the standard output.
;; 
;; 
;; Implementation
;; ==============
;; This program has been implementd in the programming language Common
;; Lisp.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-31
;; 
;; Sources:
;;   [esolang2023!!]
;;   The Esolang contributors, "!!", September 4th, 2023
;;   URL: "https://esolangs.org/wiki/!!"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
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

(deftype association-list-of (&optional (indicator-type '*)
                                        (value-type     '*)
                                        (size           '*))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of the SIZE tally of entries, each indicator, or key,
   of which conforms to the INDICATOR-TYPE and associates with a value
   of the VALUE-TYPE, both of which default to the generic sentinel
   ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or (and (symbolp size)
                     (eq size '*))
                (and (integerp size)
                     (= (length (the list candidate))
                        size)))
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element `(cons ,indicator-type ,value-type)))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype identifier-table ()
  "The ``identifier-table'' type defines a mapping of command names to
   actions representing their causata, implemented as an association
   list of a quintuple cardinality, the indicators of which are
   furnished by string, while the actions attain the format of niladic
   functions with no return value."
  '(association-list-of string function 5))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of capabilities.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-hello-world ()
  "Prints the message \"HELLO WORLD\" to the standard output and returns
   no value."
  (format T "~&HELLO WORLD")
  (values))

;;; -------------------------------------------------------

(defun print-99-bottles-of-beer ()
  "Prints the lyrics of the song \"99 Bottles of Beer\" to the standard
   output and returns no value."
  (loop for number-of-bottles of-type (integer 0 99) from 99 downto 1 do
    (format T "~&~d bottle~:p of beer on the wall," number-of-bottles)
    (format T "~&~d bottle~:p of beer."             number-of-bottles)
    (format T "~&Take one down, pass it around,")
    ;; Switch case format argument:
    ;;   (case (1- number-of-bottles)
    ;;     (0 (format T "No ..."))
    ;;     (T (format T "~d ...")))
    (format T "~&~[No~:;~:*~d~] bottle~:p of beer."
      (1- number-of-bottles))
    (format T "~2%"))
  (values))

;;; -------------------------------------------------------

(defun execute-cat-program ()
  "Executes a one-time line-based cat program, the same reverberates its
   output in a majuscular form, and returns no value."
  (format T "~&Cat program: ")
  (finish-output)
  (format T "&~:@(~a~)"
    (read-line))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defun fizzBuzz (destination
                 object-to-format
                 colon-modifier-supplied-p
                 at-sign-modifier-supplied-p
                 &rest prefix-parameters)
  "Depending upon the OBJECT-TO-FORMAT's value, either prints \"FIZZ\",
   \"BUZZ\", or \"FIZZBUZZ\" to the DESTINATION, ignoring the parameters
   COLON-MODIFIER-SUPPLIED-P, AT-SIGN-MODIFIER-SUPPLIED-P, and the
   PREFIX-PARAMETERS, and returns ``NIL'' for a non-``NIL'' DESTINATION,
   otherwise responding with a fresh string comprehending the output.
   ---
   This function's signature renders it eligible for the employment as a
   custom directive in the ``format'' function."
  (declare (type destination     destination))
  (declare (type (integer 1 100) object-to-format))
  (declare (type T               colon-modifier-supplied-p))
  (declare (ignore               colon-modifier-supplied-p))
  (declare (type T               at-sign-modifier-supplied-p))
  (declare (ignore               at-sign-modifier-supplied-p))
  (declare (type list            prefix-parameters))
  (declare (ignore               prefix-parameters))
  (format destination "~&~a"
    (cond
      ((zerop (mod object-to-format 15)) "FIZZBUZZ")
      ((zerop (mod object-to-format  3)) "FIZZ")
      ((zerop (mod object-to-format  5)) "BUZZ")
      (T                                 object-to-format)))
  (values))

;;; -------------------------------------------------------

(defun execute-FizzBuzz ()
  "Executes a FizzBuzz program with a counter ascending from inclusive
   zero (0) to one hundred (100) and returns no value."
  (loop for counter of-type (integer 1 101) from 1 to 100 do
    (format T "~/fizzBuzz/" counter))
  (values))

;;; -------------------------------------------------------

(defun print-acronym (text &key (destination NIL))
  "Generates for the TEXT the corresponding acronym, prints it to the
   DESTINATION, and returns for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responding with a fresh string comprehending the
   result."
  (declare (type string      text))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (let ((position 0))
        (declare (type fixnum position))
        (labels
            ((separator-p (candidate)
              "Determines whether the CANDIDATE represents a word
               sepiment, returning on confirmation a ``boolean'' value
               of ``T'', otherwise ``NIL''."
              (declare (type character candidate))
              (the boolean
                (not (null
                  (member candidate '(#\Space #\Tab #\-)
                    :test #'char=)))))
             
             (locate-separator ()
              "Proceeding from the current POSITION into the TEXT,
               returns the location of the nearest separator character,
               or, if none could be attested, the TEXT's length."
              (the fixnum
                (or (position-if #'separator-p text :start position)
                    (length text))))
             
             (locate-word ()
              "Proceeding from the current POSITION into the TEXT,
               returns the location of the nearest word character, or,
               if none could be attested, the TEXT's length."
              (the fixnum
                (or (position-if-not #'separator-p text :start position)
                    (length text)))))
          
          (tagbody
            ;; Choose whether to end the process, skip the current
            ;; separator character, or collect a word's initial.
            route
              (cond
                ((>= position (length text))
                  (go end))
                ((separator-p (char text position))
                  (go skip-separator))
                (T
                  (go collect-initial)))
            
            ;; Locate the POSITION cursor to the first non-separator
            ;; character, or the end of the TEXT.
            skip-separator
              (setf position (locate-word))
              (go route)
            
            ;; Write the first word character in majuscular form to the
            ;; DESTINATION and relocate the POSITION cursor either to
            ;; the next separator character, or the end of the TEXT.
            collect-initial
              (format destination "~:@(~c~)" (char text position))
              (setf position (locate-separator))
              (go route)
            
            ;; The TEXT has been entirely processed.
            end)))
      (with-output-to-string (output)
        (declare (type string-stream output))
        (print-acronym text :destination output)))))

;;; -------------------------------------------------------

(defun execute-acronym-program ()
  "Queries the user for a line of input, generates and prints the
   corresponding acronym, and returns no value."
  (format T "~&Let us generate an acronym: ")
  (finish-output)
  (let ((input (read-line)))
    (declare (type string input))
    (clear-input)
    (format T "~&~a"
      (print-acronym input :destination NIL)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type identifier-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (list (cons "HW" #'print-hello-world)
        (cons "99" #'print-99-bottles-of-beer)
        (cons "CT" #'execute-cat-program)
        (cons "FB" #'execute-FizzBuzz)
        (cons "??" #'execute-acronym-program))
  "Affiliates the recognized identifiers with functions responsible for
   their causata's exercise.")

;;; -------------------------------------------------------

(defun get-command-entry (token)
  "Returns the entry in the +IDENTIFIERS+ table corresponding to the
   TOKEN, or ``NIL'' if none such exists."
  (declare (type string token))
  (the (or null (cons string function))
    (assoc token +IDENTIFIERS+ :test #'string=)))

;;; -------------------------------------------------------

(defun command-identifier-p (token)
  "Determines whether the TOKEN represents a command name, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string token))
  (the boolean
    (not (null
      (get-command-entry token)))))

;;; -------------------------------------------------------

(defun get-command-action (token)
  "Returns the function associated with the command TOKEN, or signals an
   error of an unspecifeid type upon its disrespondency."
  (declare (type string token))
  (the function
    (or (cdr (get-command-entry token))
        (error "Unrecognized command token: ~s." token))))

;;; -------------------------------------------------------

(defun execute-action (token)
  "Executes the function associated with the command TOKEN, or signals
   an error of an unspecified type upon its disrespondency."
  (declare (type string token))
  (funcall (get-command-action token))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespaces, and returns the location in the
   SOURCE of the first non-whitespace character."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for     position of-type fixnum from start below (length source)
      while   (whitespace-character-p
                (char source position))
      finally (return position))))

;;; -------------------------------------------------------

(defun interpret-!! (code)
  "Interprets the piece of !! source CODE and returns no value."
  (declare (type string code))
  (let ((position 0))
    (declare (type fixnum position))
    (loop
      initially
        (setf position (skip-whitespaces code position))
      while
        (< position (length code))
      for token-end-position
        of-type fixnum
        =       (min (+ position 2)
                     (length code))
      do
        (execute-action
          (subseq code position token-end-position))
        (setf position
          (skip-whitespaces code token-end-position))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HELLO WORLD".
(interpret-!! "HW")

;;; -------------------------------------------------------

;; Print "HELLO WORLD" twice in succession.
(interpret-!! "HWHW")

;;; -------------------------------------------------------

;; Execute the "99 Bottles of Beer", one-time cat, and FizzBuzz
;; programs.
(interpret-!! "99 CT FB")

;;; -------------------------------------------------------

;; Demonstrate the acronym generator.
(interpret-!! "??")
