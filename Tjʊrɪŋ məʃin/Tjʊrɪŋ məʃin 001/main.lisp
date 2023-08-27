;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tjʊrɪŋ məʃin", invented by the Esolang user "Cinnamony" and
;; presented on June 15th, 2023, the telos of its wike directed towards
;; Turing-completeness, whence are the warklumes for input and output
;; as sole instruments begotten.
;; 
;; 
;; Concept
;; =======
;; The Tjʊrɪŋ məʃin programming language's foundry is empighted upon the
;; striving for a Turing-complete status, this pursuit's medium elected
;; as simple reading, writing, and input facilities, operating on
;; integer data maintained in "slots", the same exhibit an amenability
;; to integer subscripts.
;; 
;; 
;; Architecture
;; ============
;; Tjʊrɪŋ məʃin deploys a memory architecture compact of so called
;; "slots", cells amenable to unbounded signed integer indices, which
;; themselves store a scalar numeric value desumed from the same species'
;; liberty.
;; 
;; 
;; Data Types
;; ==========
;; The aefauld instrument of computational expression resides in this
;; language's integer type, unconstrained in relation to sign and
;; magnitude.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical vista, a Tjʊrɪŋ məʃin program is defined by a
;; sequence of zero or more commands, their expression follows a
;; function invocation's simulacrum, with one or two integer arguments
;; in a list amplected by parentheses.
;; 
;; == INSTRUCTIONS ==
;; Instructions are introduced via one of three possible identifiers,
;; these compositions of Latin minuscles solely, and succeeded by one or
;; two arguments, ensconced in a jumelle of parentheses, "(" and ")".
;; 
;; == ARGUMENTS ==
;; A command's argument list is introduced via an opening parenthesis,
;; "(", and followed either by a single integer number or a twain of
;; such, the latter case involves the imperative of a comma (",") as a
;; sepiment. The arguments' conclusion ensues from a matching closing
;; parenthesis, ")".
;; 
;; == WHITESPACES ==
;; The dispension of whitespaces may be exercised in concord with own's
;; personal deliberation; their presence does not account for a
;; mandatory implement.
;; 
;; == COMMENT ==
;; No provisions for comments are accommodated to the current language
;; rendition.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (ENBF) description shall
;; further elucidate the language's donet:
;; 
;;   program        := { optionalSpaces | command } ;
;;   command        := inputCommand | readCommand | writeCommand ;
;;   inputCommand   := "input" , optionalSpaces
;;                  ,  "("     , optionalSpaces
;;                  ,  integer , optionalSpaces
;;                  ,  ")"
;;                  ;
;;   readCommand    := "read"  , optionalSpaces
;;                  ,  "("     , optionalSpaces
;;                  ,  integer , optionalSpaces
;;                  ,  ")"
;;                  ;
;;   writeCommand   := "write" , optionalSpaces
;;                  ,  "("     , optionalSpaces
;;                  ,  integer , optionalSpaces
;;                  ,  ","     , optionalSpaces
;;                  ,  integer , optionalSpaces
;;                  ,  ")"
;;                  ;
;;   integer        := [ "+" | "-" ] , digit , { digit };
;;   digit          := "0" | "1" | "2" | "3" | "4"
;;                  |  "5" | "6" | "7" | "8" | "9"
;;                  ;
;;   optionalSpaces := { whitespace } ;
;;   whitespace     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The Tjʊrɪŋ məʃin offers a mere treble of instructions, whose entirety
;; operates in conjunction with input and output conduits.
;; 
;; == OVERVIEW ==
;; An apercu shall be adduced whose pursuit considers the communication
;; of a basic mete of nortelry about the commands.
;; 
;; Please heed that placeholder segments are underlined via asterisks
;; ("*"), denoting entities mandating their substitution by actual
;; Tjʊrɪŋ məʃin code.
;; 
;;   ------------------------------------------------------------------
;;   Command             | Effect
;;   --------------------+---------------------------------------------
;;   input (slot)        | Queries the user for a signed or unsigned
;;         ****          | integer number and stores it in the {slot}.
;;                       |---------------------------------------------
;;                       | The {slot} must be a signed or unsigned
;;                       | integer.
;;   ..................................................................
;;   read (slot)         | Prints the integer value stored in the
;;                       | {slot} to the standard input.
;;                       |---------------------------------------------
;;                       | The {slot} must be a signed or unsigned
;;                       | integer.
;;                       |---------------------------------------------
;;                       | If the {slot} has not explicitly set yet, it
;;                       | responds with the default value of zero (0).
;;   ..................................................................
;;   write (value, slot) | Writes the {value} into the {slot}.
;;          *****  ****  |---------------------------------------------
;;                       | The {value} must be a signed or unsigned
;;                       | integer.
;;                       |---------------------------------------------
;;                       | The {slot} must be a signed or unsigned
;;                       | integer.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The simplicity commorant in Tjʊrɪŋ məʃin disencumbers it from most
;; conspicuous ambiguities.
;; 
;; 
;; Implementation
;; ==============
;; This interpreter has been implemented in the programming language
;; Common Lisp. Chosen two commit itself to the adherence to a simple
;; solution, commands are extracted and executed in approximate
;; collaterality.
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
;; Date:   2023-08-27
;; 
;; Sources:
;;   [christensen2013lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang2023Tjʊrɪŋ məʃin]
;;   The Esolang contributors, "Tjʊrɪŋ məʃin", June 15th, 2023
;;   URL: "https://esolangs.org/wiki/
;;         Tj%CA%8Ar%C9%AA%C5%8B_m%C9%99%CA%83in"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype command-type ()
  "The ``command-type'' enumerates the recognized variations on
   command in the Tjʊrɪŋ məʃin programming language."
  '(member :input :read :write))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table as a composition of
   keys that conform to the KEY-TYPE and associated values of the
   VALUE-TYPE, for both the standard assumes the comprehensive ``T''
   species."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type represents the program memory, an association of
   integer slot names to integer slot values, as a hash table mapping
   betwixt the same types."
  '(hash-table-of integer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command
                  (type first-argument
                   &optional (second-argument NIL))))
  "The ``Command'' class provides a generic view on a Tjʊrɪŋ məʃin
   command, differentiated in its species by a type, and invested with
   further details by one or two integer arguments' mediation."
  (type           (error "Missing command type.")
                  :type command-type)
  (first-argument (error "Missing first argument.")
                  :type integer)
  (second-argument NIL
                   :type (or null integer)))



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

(defun sign-character-p (candidate)
  "Determines whether the CANDIDATE amounts to a mathematical sign, a
   compass whose perimeter both includes the plus (\"+\") and minus
   (\"-\") members, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command table.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-command-type (identifier)
  "Returns the ``command-type'' corresponding to the IDENTIFIER, or
   signals an error of an unspecified type if no such affiliation
   exists."
  (declare (type string identifier))
  (the command-type
    (cond
      ((string= identifier "input") :input)
      ((string= identifier "read")  :read)
      ((string= identifier "write") :write)
      (T (error "Invalid command name: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of scanner.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Scanner ()
  ((source
    :initarg       :source
    :initform      (error "Missing source.")
    :type          string
    :documentation "The piece of TURING MACHINE source CODE to
                    analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Scanner'' class supplies an adminicle for the extraction of
     objects from a piece of Tjʊrɪŋ məʃin source code communicated in
     string form."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((scanner Scanner) &key)
  "Initializes the SCANNER's current character to that at the first
   location in its source, if possible, and returns the modified
   SCANNER."
  (declare (type Scanner scanner))
  (with-slots (source position character) scanner
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position)))))

;;; -------------------------------------------------------

(defun make-scanner (source)
  "Creates and returns a new ``Scanner'' ordered to analyze the SOURCE."
  (declare (type string source))
  (the Scanner
    (make-instance 'Scanner :source source)))

;;; -------------------------------------------------------

(defun advance (scanner)
  "Returns the SCANNER's current character, ere moving the SCANNER's
   position cursor to the next character in its source."
  (declare (type Scanner scanner))
  (with-slots (source position character) scanner
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (the (or null character)
      (prog1 character
        (setf character
          (when (array-in-bounds-p source (1+ position))
            (char source (incf position))))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (scanner)
  "Proceeding from the current position into the SCANNER's source, skips
   a sequence of zero or more accolent whitespaces, and returns no
   value."
  (declare (type Scanner scanner))
  (with-slots (character) scanner
    (declare (type (or null character) character))
    (loop while (and character (whitespace-character-p character)) do
      (advance scanner)))
  (values))

;;; -------------------------------------------------------

(defun read-command-name (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a command name and returns an appropriate ``command-type''
   representation."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (the command-type
    (get-command-type
      (with-slots (character) scanner
        (declare (type (or null character) character))
        (with-output-to-string (identifier)
          (declare (type string-stream identifier))
          (loop while (and character (alpha-char-p character)) do
            (write-char (advance scanner) identifier)))))))

;;; -------------------------------------------------------

(defun expect-character (scanner expected-character)
  "Determines whether the character at the SCANNER's current position
   equals the EXPECTED-CHARACTER, on confirmation returning the SCANNER
   character, while concomitantly advancing it state; otherwise an error
   of an unspecified type is signaled."
  (declare (type Scanner   scanner))
  (declare (type character expected-character))
  (skip-whitespaces scanner)
  (with-slots (character position) scanner
    (declare (type (or null character) character))
    (declare (type fixnum              position))
    (declare (ignorable                position))
    (cond
      ((null character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered an exhausted source instead."
          expected-character position))
      ((char/= character expected-character)
        (error "Expected the character \"~c\" at position ~d, ~
                but encountered \"~c\" instead."
          expected-character position character))
      (T
        (advance scanner)))))

;;; -------------------------------------------------------

(defun read-integer (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a signed or unsigned integer number and returns its parsed value."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (the integer
    (with-slots (character) scanner
      (parse-integer
        (with-output-to-string (digits)
          (declare (type string-stream digits))
          (when (and character (sign-character-p character))
            (write-char (advance scanner) digits))
          (loop while (and character (digit-char-p character)) do
            (write-char (advance scanner) digits)))))))

;;; -------------------------------------------------------

(defun parse-input-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   consumes a \"input\" command's parameter list and returns a
   ``Command'' representation of the \"input\" command."
  (declare (type Scanner scanner))
  (the Command
    (make-command :input
      (prog2
        (expect-character scanner #\()
        (read-integer scanner)
        (expect-character scanner #\))))))

;;; -------------------------------------------------------

(defun parse-read-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   consumes a \"read\" command's parameter list and returns a
   ``Command'' representation of the \"read\" command."
  (declare (type Scanner scanner))
  (the Command
    (make-command :read
      (prog2
        (expect-character scanner #\()
        (read-integer scanner)
        (expect-character scanner #\))))))

;;; -------------------------------------------------------

(defun parse-write-command (scanner)
  "Proceeding from the current position into the SCANNER's source,
   consumes a \"write\" command's parameter list and returns a
   ``Command'' representation of the \"write\" command."
  (declare (type Scanner scanner))
  (the Command
    (make-command :write
      (prog2
        (expect-character scanner #\()
        (read-integer scanner)
        (expect-character scanner #\,))
      (prog1
        (read-integer scanner)
        (expect-character scanner #\))))))

;;; -------------------------------------------------------

(defun read-command (scanner)
  "Proceeding from the current position into the SCANNER's source, reads
   a command and returns an appropriate ``Command'' representation."
  (declare (type Scanner scanner))
  (let ((command-type (read-command-name scanner)))
    (declare (type command-type command-type))
    (case command-type
      (:input    (parse-input-command scanner))
      (:read     (parse-read-command  scanner))
      (:write    (parse-write-command scanner))
      (otherwise (error "Invalid command type: ~s." command-type)))))

;;; -------------------------------------------------------

(defun scanner-exhausted-p (scanner)
  "Determines whether the SCANNER is exhausted, a case whose
   assumption's entelechy conflates with its position cursor's
   transcendence of its source's boundaries, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Scanner scanner))
  (skip-whitespaces scanner)
  (the boolean
    (null (slot-value scanner 'character))))

;;; -------------------------------------------------------

(defun interpret-Tjʊrɪŋ-məʃin (code)
  "Interprets the piece of Tjʊrɪŋ məʃin source CODE and returns no
   value."
  (declare (type string code))
  (let ((scanner (make-scanner code))
        (memory  (make-hash-table :test #'eql)))
    (declare (type Scanner scanner))
    (declare (type memory  memory))
    (loop
      until (scanner-exhausted-p scanner)
      for current-command of-type Command = (read-command scanner)
      do
        (case (command-type current-command)
          (:input
            (let ((slot (command-first-argument current-command)))
              (declare (type integer slot))
              (format T "~&Please input an integer: ")
              (force-output)
              (let ((input (parse-integer (read-line))))
                (declare (type integer input))
                (setf (gethash slot memory) input))))
          
          (:read
            (let ((slot (command-first-argument current-command)))
              (declare (type integer slot))
              (format T "~d "
                (gethash slot memory 0))))
          
          (:write
            (let ((number (command-first-argument  current-command))
                  (slot   (command-second-argument current-command)))
              (declare (type integer number))
              (declare (type integer slot))
              (setf (gethash slot memory) number)))
          
          (otherwise
            (error "Invalid command: ~s." current-command)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the ASCII codes of the message "Hello world!".
(interpret-Tjʊrɪŋ-məʃin
  "
  write(72,1)
  write(101,2)
  write(108,3)
  write(108,4)
  write(111,5)
  write(32,6)
  write(119,7)
  write(111,8)
  write(114,9)
  write(108,10)
  write(100,11)
  write(33,12)
  read(1)
  read(2)
  read(3)
  read(4)
  read(5)
  read(6)
  read(7)
  read(8)
  read(9)
  read(10)
  read(11)
  read(12)
  ")

;;; -------------------------------------------------------

;; One-time numeric cat program.
(interpret-Tjʊrɪŋ-məʃin
  "input(1)
   read(1)")
