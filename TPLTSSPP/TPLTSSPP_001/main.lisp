;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TPLTSSPP", presented by the Esolang user "Esolang1" in the
;; year 2021, and based upon an instruction set embracing common
;; programs and problems from the fields of computer science and
;; arithmetics.
;; 
;; 
;; Concepts
;; ========
;; The TPLTSSPP programming language defines its programs as a sequence
;; of zero or more procedure invocations, to this end operating on
;; either numeric, string or list parameters.
;; 
;; == TPLTSSPP: A LANGUAGE FOR COMMON PROBLEMS ==
;; Its bailiwick's bewrayal, the agnomination "TPLTSSPP" extends into
;; the unabbreviated full name "The Programming Language to Solve Simple
;; Programming Problems", intiminating the intended deployment for the
;; purpose of handling common computer science and arithmetics related
;; tasks.
;; 
;; == PROCEDURES EXHAUST THE INSTRUCTION SET ==
;; The language may be subsumed in some aspect into a purely functional
;; ilk, as no side effects preside in its only object species, the
;; procedures --- the same do not return a value, however, by which
;; criterion the lack of functions disturbs the "functional" claim. Each
;; instruction in this language constitutes a routine's invocation,
;; the actual parameters depending on the respective use case.
;; 
;; == NUMBERS, STRINGS AND LISTS DEFINE THE PROGRAMS' CURRENCY ==
;; A rather generous administration of object types participates in
;; TPLTSSPP, embracing signed or unsigned integers, floating-point data,
;; strings, as well as lists compact of the aforementioned numeric
;; articles.
;; 
;; 
;; Data Types
;; ==========
;; The type variants commorant in TPLTSSPP enumerate a rather mickle
;; system:
;; 
;;   - Natural numbers, defined as any integer greater than or equal to
;;     one (1), but without an upper bourne.
;;   - Non-negative integers, embracing all integers greater than or
;;     equal to zero (0), but without an upper bourne.
;;   - Signed integers of no lower or upper limitation.
;;   - Signed floating-point numbers without intrinsic constraint anenst
;;     their precision, minimum, or maximum values.
;;   - Strings composed of zero or more characters.
;;   - Lists of signed integers or signed floating-point numbers.
;; 
;; 
;; Architecture
;; ============
;; TPLTSSPP's stateless nature, ensuing from its programs comprehending
;; merely procedure invocations desitute of side effects, negates its
;; reliance upon any architecture.
;; 
;; 
;; Instructions
;; ============
;; Enumerating in its circumference thirteen members, the instruction
;; set constitutes a participant in several rather simple programming
;; topics, such as the famous "Hello, World!" program, as well as
;; subjects responding to mathematical problems of varying
;; sophistication, spanning the unassuming plain of addition and the
;; computational predicaments of prime factorization.
;; 
;; == OVERVIEW ==
;; A summarizing treatise regarding the operational capabilities shall
;; be dedicated to the following table:
;; 
;;   ------------------------------------------------------------------
;;   Command         | Effect
;;   ----------------+-------------------------------------------------
;;   add(a, b)       | Prints to the standard output the sum of a + b,
;;                   | where a and b represent real numbers.
;;   ..................................................................
;;   checkprime(n)   | Checks whether the positive integer n represents
;;                   | a prime number, printing on confirmation to the
;;                   | standard output "true", otherwise "false".
;;   ..................................................................
;;   bottlesofbeer() | Prints to the standard output the lyrics of the
;;                   | song "99 Bottles of Beer on the Wall".
;;   ..................................................................
;;   factor(n)       | Prints to the standard output the prime factors
;;                   | comprimising the positive integer n.
;;   ..................................................................
;;   factorial(n)    | Prints to the standard output the factorial n!
;;                   | of the positive integer n.
;;   ..................................................................
;;   fibonacci(n)    | Prints to the standard output the first n
;;                   | Fibonacci numbers, with n being a non-negative
;;                   | integer.
;;   ..................................................................
;;   fizzbuzz(n)     | Prints to the standard output the first n
;;                   | members of the FizzBuzz sequence, with n being a
;;                   | non-negative integer.
;;   ..................................................................
;;   helloworld()    | Prints to the standard output the message
;;                   | "Hello, World!".
;;   ..................................................................
;;   palindrome(s)   | Checks whether the string s represents a
;;                   | palindrome, printing on confirmation to the
;;                   | standard output the message "true", otherwise
;;                   | "false".
;;   ..................................................................
;;   randnum(a, b)   | Prints to the standard output a randomly
;;                   | selected integer from the closed range [a, b],
;;                   | where both a and b are integers.
;;   ..................................................................
;;   randreal()      | Prints to the standard output a real number from
;;                   | the closed range [0.0, 1.0].
;;   ..................................................................
;;   sort(x)         | Prints to the standard output the sorted list of
;;                   | real numbers x.
;;   ..................................................................
;;   sum(n)          | Prints to the standard output the sum of the
;;                   | first n integers, with n being a positive
;;                   | integers.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The proterolog's inchoate nature inflicts the language with some
;; inroads of ambiguity, a subset of which shall be the following
;; treatise's cynosure.
;; 
;; == WHICH ELEMENTS CAN BE SUPPLIED TO "sort"? ==
;; The "sort" procedure's purpose applies to the sorting of a list of
;; items. Whereas a conjecture about its order, extrapolated from the
;; common wont of ascending manifestations, can be conjectured, the
;; topic concerning the admissive types remains in a crepuscle.
;; 
;; It has been adjudge to homologate merely numeric elements, that is,
;; integers or floating-point values, in order to obviate convolute
;; elaborations about the precedence and intrinsics appertaining to the
;; admixture of numeric and lexicographic objects.
;; 
;; == WHAT DOES "fizzbuzz(0)" PRINT? ==
;; The "fizzbuzz(n)" operation commits to the display of the FizzBuzz
;; sequence's first n members, where n >= 0. The widespread consuetude
;; fixates the sequence's parameters to a start value of one (1) and an
;; inclusive end equal to one-hundred (100). It is not stated in an
;; explicit declamation what effect shall answer to a value of zero (0),
;; which, per stringent construe of the calcuation rule, should always
;; print "FizzBuzz".
;; 
;; It has been chosen to assign no effect to a parameter of n = 0, which
;; means that no output will be issued.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-01-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/TPLTSSPP"
;;   -> "https://stackoverflow.com/questions/69703679/how-to-generate-random-numbers-in-0-1-0-in-common-lisp"
;;       o Generating random real numbers in the closed interval
;;         [0.0, 1.0].
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' tyep defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
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

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associating with
   a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype number-type ()
  "The ``number-type'' type enumerates the recognized variations of
   numbers encountered during the lexical analyzation of a piece of
   TPLTSSPP source code."
  '(member :integer :floating-point))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized instruction variants."
  '(member
    :add
    :bottlesofbeer
    :checkprime
    :factor
    :factorial
    :fibonacci
    :fizzbuzz
    :helloworld
    :palindrome
    :randnum
    :randreal
    :sort
    :sum))

;;; -------------------------------------------------------

(deftype tpltsspp-program ()
  "The ``tpltsspp-program'' type defines a TPLTSSPP program assembled
   from a piece of source code in string form as a list of zero or more
   instructions."
  '(list-of Instruction))

;;; -------------------------------------------------------

(deftype natural-number ()
  "The ``natural-number'' type defines the mathematical concept of
   natural numbers as any integer greater than or equal to one (1)."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-type'' type defines the mathematic concept of
   natural numbers including the member zero (0) as any integer greater
   than or equal to zero (0)."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype tpltsspp-object ()
  "The ``tpltsspp-object'' type defines the valid variants of TPLTSSPP
   objects, which comprehends real numbers, strings, and lists of the
   aforementioned twain."
  '(or real string (list-of (or real string))))

;;; -------------------------------------------------------

(deftype actual-parameter-list ()
  "The ``actual-parameter-list'' type defines an actual parameter list
   appertaining to an instruction, resolving to a list of zero or more
   ``tpltsspp-object''s."
  '(list-of tpltsspp-object))

;;; -------------------------------------------------------

(deftype formal-parameter-list ()
  "The ``formal-parameter-list'' type defines a list of zero or more
   elements which are expected to be type specifiers, amenable to a
   comparison against an actual object during a ``typep'' check."
  '(list-of T))

;;; -------------------------------------------------------

(deftype signature-table ()
  "The ``signature-table'' type defines an association of command types
   to the expected formal parameters in the form of a hash table whose
   keys assume ``command'' objects, each such mapping to a
   ``formal-parameter-list''."
  '(hash-table-of command formal-parameter-list))

;;; -------------------------------------------------------

(deftype prime-number ()
  "The ``prime-number'' type defines an integer value compatible with
   the notion of a prime number.
   ---
   Please note that this type specification does not encompass an
   inquiry into an object's actual realization of the prime predicate;
   instead, a conformity is already imputed if an integer occupies the
   valid range [2, +infinity]. In the light of this, the type specifier
   might more correctly be called a designator for a 'prime candidate'."
  '(integer 2 *))

;;; -------------------------------------------------------

(deftype prime-generator ()
  "The ``prime-generator'' type defines a niladic function which,
   starting at the value two (2), returns upon each invocation the next
   prime number.
   ---
   Unbounded towards the upper march, and fortified by the Common Lisp
   ``integer'' type's artibrary mickleness, this function conceptually
   responds to an infinite quantity of requests, albeit the employed
   system's memory might eventually claim a denial in its services."
  '(function () prime-number))

;;; -------------------------------------------------------

(deftype prime-factors ()
  "The ``prime-factors'' type defines a sequence of an input number's
   prime factors, represented by a list of zero or more ``prime-number''
   instances"
  '(list-of prime-number))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class implements a significant object extracted from a
   piece of TPLTSSPP source code as a result of its lexical
   analyzation."
  (type  (error "Missing token type.") :type keyword)
  (value NIL                           :type T))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of identifiers.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type hash-table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-hash-table :test #'equal)
  "Associates the recognized function names with representative
   ``Token'' objects.")

;;; -------------------------------------------------------

(flet ((register-identifier (name command)
        "Associates the identifier NAME with the COMMAND in the global
         +IDENTIFIERS+ table by creating a token of the type
         ``:function'' affiliated with the COMMAND as its type, and
         returns no value."
        (declare (type string  name))
        (declare (type command command))
        (setf (gethash name +IDENTIFIERS+)
              (make-token :function command))
        (values)))
  (register-identifier "add"           :add)
  (register-identifier "bottlesofbeer" :bottlesofbeer)
  (register-identifier "checkprime"    :checkprime)
  (register-identifier "factor"        :factor)
  (register-identifier "factorial"     :factorial)
  (register-identifier "fibonacci"     :fibonacci)
  (register-identifier "fizzbuzz"      :fizzbuzz)
  (register-identifier "helloworld"    :helloworld)
  (register-identifier "palindrome"    :palindrome)
  (register-identifier "randnum"       :randnum)
  (register-identifier "randreal"      :randreal)
  (register-identifier "sort"          :sort)
  (register-identifier "sum"           :sum)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of function signatures.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type signature-table +FUNCTION-SIGNATURES+))

;;; -------------------------------------------------------

(defparameter +FUNCTION-SIGNATURES+
  (make-hash-table :test #'eql)
  "Associates each command (function) with a list of formal
   parameters.")

;;; -------------------------------------------------------

(flet ((register-signature (command formal-parameters)
        "Associates the FORMAL-PARAMETERS with the COMMAND in the global
         +FUNCTION-SIGNATURES+ table, replacing any previous entry for
         the COMMAND if extant, and returns no value."
        (declare (type command               command))
        (declare (type formal-parameter-list formal-parameters))
        (setf (gethash command +FUNCTION-SIGNATURES+) formal-parameters)
        (values)))
  (register-signature :add           '(real real))
  (register-signature :bottlesofbeer  ())
  (register-signature :checkprime    '(natural-number))
  (register-signature :helloworld     ())
  (register-signature :factor        '(natural-number))
  (register-signature :factorial     '(non-negative-integer))
  (register-signature :fibonacci     '(non-negative-integer))
  (register-signature :fizzbuzz      '(non-negative-integer))
  (register-signature :palindrome    '(string))
  (register-signature :randnum       '(integer integer))
  (register-signature :randreal       ())
  (register-signature :sort          '((list-of real)))
  (register-signature :sum           '(natural-number))
  (values))

;;; -------------------------------------------------------

(defun get-signature (command)
  "Returns the formal parameters associated with the COMMAND, or signals
   an error of an unspecified type upon a disrespondency."
  (declare (type command command))
  (multiple-value-bind (formal-parameters contains-command-p)
      (gethash command +FUNCTION-SIGNATURES+)
    (declare (type (or null formal-parameter-list) formal-parameters))
    (declare (type T                               contains-command-p))
    (the formal-parameter-list
      (if contains-command-p
        formal-parameters
        (error "No signature defined for the command ~s." command)))))

;;; -------------------------------------------------------

(defun matches-signature-p (actual-parameters formal-parameters)
  "Checks whether the ACTUAL-PARAMETERS are compatible with the
   FORMAL-PARAMETERS, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type actual-parameter-list actual-parameters))
  (declare (type formal-parameter-list formal-parameters))
  (the boolean
    (not (null
      (and
        (= (length formal-parameters)
           (length actual-parameters))
        (every #'typep actual-parameters formal-parameters))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member (char-code candidate) '(9 10 13 32) :test #'=)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Checks whether the CANDIDATE represents a mathematical signum (\"+\"
   or \"-\"), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\+ #\-) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Checks whether the CANDIDATE represents an identifier name
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alpha-char-p candidate)))))

;;; -------------------------------------------------------

(defun get-identifier-token (identifier)
  "Returns the token answering to the IDENTIFIER, or signals an error of
   an unspecified type upon its absence."
  (declare (type string identifier))
  (the Token
    (or (gethash identifier +IDENTIFIERS+)
        (error "Unrecognized identifier: ~s." identifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Lexer
  (:constructor make-lexer
    (source
     &aux (position 0)
          (character
            (when (array-in-bounds-p source position)
              (char source position))))))
  "The ``Lexer'' class provides a lexical analyzer, capacitated to
   extract significant objects from a piece of TPLTSSPP source code in
   the form of tokens."
  (source    (error "Missing lexer source.") :type string)
  (position  0                               :type fixnum)
  (character NIL                             :type (or null character)))

;;; -------------------------------------------------------

(defmacro with-lexer ((lexer) &body body)
  "Evaluates the LEXER, binds its slots ``source'', ``position'' and
   ``character'' to eponymous local symbol macros, evaluates the BODY
   forms, and returns the last processed form's results."
  (let ((evaluated-lexer (gensym)))
    (declare (type symbol evaluated-lexer))
    `(let ((,evaluated-lexer ,lexer))
       (declare (type Lexer ,evaluated-lexer))
       (declare (ignorable  ,evaluated-lexer))
       (symbol-macrolet
           ((source
             (the string
               (lexer-source ,evaluated-lexer)))
            (position
             (the fixnum
               (lexer-position ,evaluated-lexer)))
            (character
             (the (or null character)
               (lexer-character ,evaluated-lexer))))
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         ,@body))))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER's position cursor to the next character in the
   source, if possible, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-skip-whitespaces (lexer)
  "Starting at the LEXER's current position, skips zero or more adjacent
   whitespaces and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (loop while (and character (whitespace-character-p character)) do
      (lexer-advance lexer)))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-identifier-name (lexer)
  "Starting at the LEXER's current position, consumes a function
   identifier and returns it."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the string
      (with-output-to-string (content)
        (declare (type string-stream content))
        (loop
          while (and character (identifier-character-p character))
          do
            (write-char character content)
            (lexer-advance lexer))))))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the LEXER's current position, reads a function identifier
   and returns a token representation thereof."
  (declare (type Lexer lexer))
  (the Token
    (get-identifier-token
      (lexer-read-identifier-name lexer))))

;;; -------------------------------------------------------

(defun lexer-read-digits (lexer)
  "Starting at the LEXER's current position, consumes a signed or
   unsigned integer or floating-point number, returning two values:
     (1) the string portion encompassing the number
     (2) the number type."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (let ((number-type :integer))
      (declare (type number-type number-type))
      (the (values string number-type)
        (values
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            (flet
                ((read-optional-sign ()
                  "If the current CHARACTER represents a mathematical
                   signum, writes it to the DIGITS stream, advances the
                   POSITION cursor, and returns no value."
                  (when (and character (sign-character-p character))
                    (write-char character digits)
                    (lexer-advance lexer))
                  (values))
                 
                 (expect-digit ()
                  "Checks whether the current CHARACTER represents a
                   decimal digit, on confirmation simply returning no
                   value; otherwise an error of an unspecified type is
                   signaled."
                  (unless (and character (digit-char-p character))
                    (error "Expected a decimal digit, but ~
                            encountered ~s at position ~d."
                      character position))
                  (values))
                 
                 (read-digits ()
                  "Starting at the current POSITION, reads a sequence of
                   zero or more adjacent decimal digits, writes these to
                   the DIGITS stream, advances the POSITION cursor, and
                   returns no value."
                  (loop
                    while (and character (digit-char-p character))
                    do
                      (write-char character digits)
                      (lexer-advance lexer))
                  (values))
                 
                 (decimal-point-consumed-p ()
                  "Checks whether the current CHARACTER represents a
                   decimal point (\".\"), on confirmation writing the
                   same to the DIGITS stream, advances the POSITION
                   cursor, and returns the ``boolean'' value ``T'';
                   otherwise simply returns ``NIL''."
                  (the boolean
                    (when (and character (char= character #\.))
                      (write-char character digits)
                      (lexer-advance lexer)
                      T))))
              
              (read-optional-sign)
              (expect-digit)
              (read-digits)
              
              (when (decimal-point-consumed-p)
                (setf number-type :floating-point)
                (read-digits))))
          number-type)))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the LEXER's current position, reads a signed or unsigned
   floating-point or integer number and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (multiple-value-bind (content number-type)
      (lexer-read-digits lexer)
    (declare (type string      content))
    (declare (type number-type number-type))
    (the Token
      (make-token :number
        (case number-type
          (:integer
            (parse-integer content))
          (:floating-point
            (read-from-string content))
          (otherwise
            (error "Invalid number type: ~s." number-type)))))))

;;; -------------------------------------------------------

(defun lexer-read-string (lexer)
  "Starting at the LEXER's current position, and expected to reside on a
   double quote, consumes a string and returns a token representation
   thereof."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (make-token :string
        (with-output-to-string (content)
          (declare (type string-stream content))
          (lexer-advance lexer)
          (loop do
            (case character
              ((NIL)
                (error "Unterminated string at position ~d." position))
              (#\"
                (lexer-advance lexer)
                (loop-finish))
              (otherwise
                (write-char character content)
                (lexer-advance lexer)))))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to every request
   with a fresh instance of an end-of-file (EOF) token."
  (declare (type Lexer lexer))
  (with-lexer (lexer)
    (the Token
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-skip-whitespaces lexer)
          (lexer-get-next-token   lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((find character "+-" :test #'char=)
          (lexer-read-number lexer))
        
        ((char= character #\()
          (prog1
            (make-token :left-parenthesis character)
            (lexer-advance lexer)))
        
        ((char= character #\))
          (prog1
            (make-token :right-parenthesis character)
            (lexer-advance lexer)))
        
        ((char= character #\[)
          (prog1
            (make-token :left-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\])
          (prog1
            (make-token :right-bracket character)
            (lexer-advance lexer)))
        
        ((char= character #\,)
          (prog1
            (make-token :comma character)
            (lexer-advance lexer)))
        
        ((char= character #\")
          (lexer-read-string lexer))
        
        (T
          (error "Invalid character ~c at position ~d."
            character position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type parameters)))
  "The ``Instruction'' class serves to encapsulate all pertinent
   information concerning a function invocation, specified by the
   command identification and a list of zero or more actual parameters."
  (type
    (error "Missing instruction type.")
    :type command)
  (parameters
    NIL
    :type actual-parameter-list))

;;; -------------------------------------------------------

(defun instruction-signature-matches-p (instruction)
  "Checks whether the INSTRUCTION's actual parameters match the formal
   parameters corresponding to the represented command, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Instruction instruction))
  (let ((formal-parameters (get-signature
                             (instruction-type instruction)))
        (actual-parameters (instruction-parameters instruction)))
    (declare (type formal-parameter-list formal-parameters))
    (declare (type actual-parameter-list actual-parameters))
    (the boolean
      (not (null
        (and
          (= (length formal-parameters)
             (length actual-parameters))
          (every #'typep actual-parameters formal-parameters)))))))

;;; -------------------------------------------------------

(defun instruction-signature-check (instruction)
  "Checks whether the INSTRUCTION's actual parameters match the formal
   parameters corresponding to the represented command, returning on
   confirmation no value, otherwise signaling an error of an unspecified
   type."
  (declare (type Instruction instruction))
  (unless (instruction-signature-matches-p instruction)
    (error "Invalid actual parameters for the instruction ~s. ~
            Expected the formal parameter list ~s, but encountered ~s."
      instruction
      (get-signature (instruction-type instruction))
      (instruction-parameters instruction)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (lexer
                             &aux (current-token
                                    (lexer-get-next-token lexer)))))
  "The ``Parser'' class assembles a series of tokens into a list of
   instructions which represent an executable TPLTSSPP program."
  (lexer         (error "Missing lexer for parser.") :type Lexer)
  (current-token (make-token :eof NIL)               :type Token))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) *) parser-parse-actual-parameter))

;;; -------------------------------------------------------

(defmacro with-parser ((parser) &body body)
  "Evaluates the PARSER, binds its slots ``lexer'' and ``current-token''
   to eponymous local symbol macros, evaluates the BODY forms, and
   returns the last processed form's results."
  (let ((evaluated-parser (gensym)))
    (declare (type symbol evaluated-parser))
    `(let ((,evaluated-parser ,parser))
       (declare (type Parser ,evaluated-parser))
       (declare (ignorable   ,evaluated-parser))
       (symbol-macrolet
           ((lexer
             (the Lexer
               (parser-lexer ,evaluated-parser)))
            (current-token
             (the Token
               (parser-current-token ,evaluated-parser))))
         (declare (type Lexer lexer))
         (declare (type Token current-token))
         (declare (ignorable  lexer))
         (declare (ignorable  current-token))
         ,@body))))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token, ere
   quering the next one from the internally managed LEXER; otherwise an
   error of an unspecified type is signaled."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-parser (parser)
    (the Token
      (if (token-type-p current-token expected-token-type)
        (prog1 current-token
          (setf current-token
            (lexer-get-next-token lexer)))
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-token-type current-token)))))

;;; -------------------------------------------------------

(defun parser-parse-comma-separated-values (parser
                                            terminating-token-type)
  "Parses a sequence of zero or more objects separated by a comma
   (\",\") until the PARSER's curren token assumes the
   TERMINATING-TOKEN-TYPE, and returns a list of the thus gathered
   objects."
  (declare (type Parser  parser))
  (declare (type keyword terminating-token-type))
  (let ((actual-parameters NIL))
    (declare (type actual-parameter-list actual-parameters))
    (with-parser (parser)
      (unless (token-type-p current-token terminating-token-type)
        (push (parser-parse-actual-parameter parser) actual-parameters)
        (loop while (token-type-p current-token :comma) do
          (parser-eat parser :comma)
          (push (parser-parse-actual-parameter parser)
                actual-parameters)))
      (the actual-parameter-list
        (nreverse actual-parameters)))))

;;; -------------------------------------------------------

(defun parser-parse-actual-parameter (parser)
  "Parses an actual parameter using the PARSER and returns it."
  (declare (type Parser parser))
  (with-parser (parser)
    (the tpltsspp-object
      (case (token-type current-token)
        (:number
          (token-value (parser-eat parser :number)))
        (:string
          (token-value (parser-eat parser :string)))
        (:left-bracket
          (parser-eat parser :left-bracket)
          (prog1
            (parser-parse-comma-separated-values parser :right-bracket)
            (parser-eat parser :right-bracket)))
        (otherwise
          (error "Invalid actual parameter token: ~s."
            current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-actual-parameters (parser)
  "Parses a sequence of zero or more actual parameters using the PARSER
   and returns a list containing the thus gathered objects."
  (declare (type Parser parser))
  (with-parser (parser)
    (parser-eat parser :left-parenthesis)
    (the actual-parameter-list
      (prog1
        (parser-parse-comma-separated-values parser :right-parenthesis)
        (parser-eat parser :right-parenthesis)))))

;;; -------------------------------------------------------

(defun parser-parse-function-call (parser)
  "Parses a function invocation as a composition of an identifier and
   zero or more actual parameters ensconced in a pair of parentheses,
   and returns an ``Instrunction'' representation thereof."
  (declare (type Parser parser))
  (with-parser (parser)
    (let ((function-name (token-value (parser-eat parser :function))))
      (declare (type command function-name))
      (the Instruction
        (make-instruction function-name
          (parser-parse-actual-parameters parser))))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the tokens supplied by the PARSER's internally managed lexer
   and returns a list of ``Instruction'' instances representing the
   detected function invocations."
  (declare (type Parser parser))
  (with-parser (parser)
    (the tpltsspp-program
      (prog1
        (loop
          while   (token-type-p current-token :function)
          collect (parser-parse-function-call parser))
        (parser-eat parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operations.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize the global random number generator.
(setf *random-state* (make-random-state T))

;;; -------------------------------------------------------

(defun print-addition (augend addend)
  "Prints to the standard output the sum of the AUGEND and ADDEND and
   returns no value."
  (declare (type real augend))
  (declare (type real addend))
  (format T "~&~d"
    (+ augend addend))
  (values))

;;; -------------------------------------------------------

(defun print-99-bottles-of-beer ()
  "Prints to the standard output the lyrics of the program \"99 Bottles
   of Beer\" and returns no value."
  (loop for number-of-bottles of-type (integer 0 99) from 99 downto 2 do
    (format T "~&~d bottles of beer on the wall,~@
               ~d bottles of beer.~@
               Take one down, pass it around,~@
               ~d~:* bottle~p of beer on the wall.~2%"
      number-of-bottles
      number-of-bottles
      (1- number-of-bottles)))
  (format T "1 bottle of beer on the wall,~@
             1 bottle of beer.~@
             Take one down, pass it around,~@
             No bottles of beer on the wall.")
  (values))

;;; -------------------------------------------------------

(defun print-factorial (n)
  "Prints to the standard output the factorial N! and returns no value."
  (declare (type non-negative-integer n))
  (format T "~&~d"
    (if (zerop n)
      1
      (loop
        with    factorial of-type natural-number = 1
        for     factor    of-type natural-number from 2 upto n
        do      (setf factorial (* factorial factor))
        finally (return factorial))))
  (values))

;;; -------------------------------------------------------

(defun print-Fibonacci-numbers (n)
  "Prints to the standard output the first N Fibonacci numbers and
   returns no value."
  (declare (type non-negative-integer n))
  (cond
    ((zerop n)
      NIL)
    ((= n 1)
      (format T "~&0"))
    (T
      (loop
        repeat (- n 2)
        for f[n-2] of-type non-negative-integer = 0 then f[n-1]
        for f[n-1] of-type non-negative-integer = 1 then f[n]
        for f[n]   of-type non-negative-integer = (+ f[n-1] f[n-2])
        initially
          (format T "~&0, 1")
        do
          (format T ", ~d" f[n]))))
  (values))

;;; -------------------------------------------------------

(defun print-FizzBuzz (n)
  "Prints to the standard output the first N elements of the FizzBuzz
   sequence and returns no value."
  (declare (type non-negative-integer n))
  (flet ((aliquot-p (dividend divisor)
          "Checks whether the DIVISOR divides the DIVIDEND without rest,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type non-negative-integer dividend))
          (declare (type non-negative-integer divisor))
          (the boolean
            (not (null
              (and (plusp divisor)
                   (zerop (mod dividend divisor))))))))
    (when (plusp n)
      (loop for counter of-type non-negative-integer from 1 to n do
        (format T "~&~a"
          (cond
            ((aliquot-p counter 15) "FizzBuzz")
            ((aliquot-p counter  3) "Fizz")
            ((aliquot-p counter  5) "Buzz")
            (T                      counter))))))
  (values))

;;; -------------------------------------------------------

(defun print-hello-world ()
  "Prints to the standard output the message \"Hello, World!\" and
   returns no value."
  (format T "~&Hello, World!")
  (values))

;;; -------------------------------------------------------

(defun print-sum (n)
  "Prints the sum of the first N natural numbers and returns no value."
  (declare (type natural-number n))
  (format T "~&~a"
    (loop for i of-type natural-number from 1 upto n sum i))
  (values))

;;; -------------------------------------------------------

(defun palindrome-p (candidate)
  "Checks whether the CANDIDATE string represents a palindrome,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string candidate))
  (let ((candidate-length (length candidate)))
    (declare (type fixnum candidate-length))
    (the boolean
      (not (null
        (or
          (<= candidate-length 1)
          (let ((test-end-position (floor candidate-length 2)))
            (declare (type fixnum test-end-position))
            (loop
              for left-index
                of-type fixnum
                from    0
                below   test-end-position
              for right-index
                of-type fixnum
                =       (- candidate-length left-index 1)
              always
                (char= (char candidate left-index)
                       (char candidate right-index))))))))))

;;; -------------------------------------------------------

(defun print-palindrome (candidate)
  "Prints to the standard output the message \"true\" if the CANDIDATE
   string represents a palindrome, otherwise \"false\", and returns no
   value."
  (declare (type string candidate))
  (format T "~&~:[false~;true~]" (palindrome-p candidate))
  (values))

;;; -------------------------------------------------------

(defun get-random-integer (minimum maximum)
  "Returns a random integer in the closed range [MINIMUM, MAXIMUM]."
  (declare (type integer minimum))
  (declare (type integer maximum))
  (the non-negative-integer
    (+ minimum
       (random (- (1+ maximum) minimum)))))

;;; -------------------------------------------------------

(defun print-random-integer (minimum maximum)
  "Prints to the standard output a random integer in the closed range
   [MINIMUM, MAXIMUM] and returns no value."
  (declare (type non-negative-integer minimum))
  (declare (type non-negative-integer maximum))
  (format T "~&~d"
    (get-random-integer minimum maximum))
  (values))

;;; -------------------------------------------------------

(defun get-random-real ()
  "Randomly returns a single-precision floating-point number in the
   closed range [0.0, 1.0]."
  (the single-float
    (random (+ 1.0f0 single-float-epsilon))))

;;; -------------------------------------------------------

(defun print-random-real ()
  "Prints to the standard output a random single-precision
   floating-point number in the closed range [0.0, 1.0] and returns no
   value."
  (format T "~&~a"
    (get-random-real))
  (values))

;;; -------------------------------------------------------

(defun print-sorted-list (elements)
  "Sorts the numeric ELEMENTS in ascending order, prints them to the
   standard output, and returns no value."
  (declare (type (list-of real) elements))
  (format T "~&~{~a~^, ~}"
    (sort (copy-list elements) #'<))
  (values))

;;; -------------------------------------------------------

(defun prime-p (candidate)
  "Checks whether the CANDIDATE integer represents a prime number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type integer candidate))
  (the boolean
    (and
      (>= candidate 2)
      (loop
        for divisor of-type (integer 1 *) from (1- candidate) downto 2
        when (zerop (mod candidate divisor)) do
          (return NIL)
        finally
          (return T)))))

;;; -------------------------------------------------------

(defun make-prime-generator ()
  "Creates and returns a niladic function which, starting with the value
   two (2), returns upon each invocation the next prime number.
   ---
   Example:
     ;; Print the first ten prime numbers to the standard output, that
     ;; is: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29.
     (let ((generator (make-prime-generator)))
       (declare (type prime-generator generator))
       (loop repeat 10 do
         (print (funcall generator))))"
  (let ((next-prime 2))
    (declare (type prime-number next-prime))
    (the function
      #'(lambda ()
          (the prime-number
            (prog1 next-prime
              (loop
                do    (incf next-prime)
                until (prime-p next-prime))))))))

;;; -------------------------------------------------------

(defun get-next-prime (prime-generator)
  "Returns the next prime number from the PRIME-GENERATOR."
  (declare (type prime-generator))
  (the prime-number
    (funcall prime-generator)))

;;; -------------------------------------------------------

(defun get-exponent-for-prime (number prime)
  "Returns the exponent by which the PRIME number was raised in order
   to contribute to the product constructing the NUMBER.
   ---
   A concomitant of its arithmetic notion, the exponent represents the
   tally of occurrences of the PRIME in the NUMBER."
  (declare (type (integer 0 *) number))
  (declare (type prime-number  prime))
  (the (integer 0 *)
    (loop
      with  remainder of-type (integer 0 *) = number
      while (zerop (mod remainder prime))
      do    (setf remainder (/ remainder prime))
      count 1)))

;;; -------------------------------------------------------

(defun get-prime-factors (number)
  "Returns for the NUMBER a list of its prime factors, with each factor
   represented by a cons, the left compartment of which comprehends the
   participating prime number, accompanied in the right moiety by its
   number of occurrences in the NUMBER, thus defining in coefficiency
   the power p^e, with p being the prime number, e being the exponent or
   repetitions.
   ---
   As postulated by the fundamental theorem of arithmetic, any positive
   integer number can be represented as a product of one or more prime
   numbers {p[1], p[2], ..., p[r]}, with r being the tally of the
   constituent primes, contributing one or more times k[i], with k >= 1,
   in the form of an exponent to the prime, to the operation. As a
   colorollary, For any positive integer number x holds:
   
     x = p[1]^k[1] * p[2]^k[2] * ... * p[i]^k[i] * ... * p[r]^k[r]
   
   ---
   This function returns the r prime numbers p[1] to p[r] constituting
   the NUMBER x in conjunction with their respective tallies k[1] to
   k[r] as a (p * k)-size list of prime numbers.
   
     (p[1], p[1], ..., p[2], p[2], ..., p[i], p[i], ... p[r], p[r])"
  (declare (type natural-number number))
  (let ((prime-generator (make-prime-generator)))
    (declare (type prime-generator))
    (the prime-factors
      (loop
        ;; The PRODUCT realizes the iteration's termination condition:
        ;; It is multiplied by all prime numbers contained in the NUMBER
        ;; a tally of times equal to their exponent. The moment the
        ;; PRODUCT equals the NUMBER, we can be ascertained that all
        ;; prime factors of the latter have been discovered, as the
        ;; PRODUCT has been constructed using the same.
        with product
          of-type (integer 1 *)
          =       1
        
        ;; As long as the PRODUCT does not equal the input NUMBER, not
        ;; all prime factors have been discovered yet.
        while (< product number)
        
        ;; Load the next prime number.
        for prime
          of-type prime-number
          =       (get-next-prime prime-generator)
        ;; Compute the exponent (tally) of the current PRIME in the
        ;; NUMBER.
        for prime-exponent
          of-type (integer 0 *)
          =       (get-exponent-for-prime number prime)
        
        ;; If the PRIME contributes to the NUMBER, that is, displays an
        ;; exponent (tally) greater than zero, collect both the PRIME
        ;; itself and its PRIME-EXPONENT in a cons, with the former
        ;; lodged in the left cell location, and the latter in the right
        ;; compartment.
        ;; 
        ;; Subsequently, update the PRODUCT to reflect the contribution
        ;; of the current PRIME by inducing it the tally PRIME-EXPONENT
        ;; number of times.
        when (plusp prime-exponent)
          append (make-list prime-exponent :initial-element prime)
            into summands
          and do
            (setf product
              (* product
                 (expt prime prime-exponent)))
        
        finally
          (return summands)))))

;;; -------------------------------------------------------

(defun print-check-prime (n)
  "Prints to the standard output the message \"true\" if N represents a
   prime number, otherwise \"false\", and returns no value."
  (declare (type natural-number n))
  (format T "~&~:[false~;true~]" (prime-p n))
  (values))

;;; -------------------------------------------------------

(defun print-prime-factorization (n)
  "Prints to the standard output in ascending order the prime factors of
   the natural number N and returns no value."
  (declare (type natural-number n))
  (format T "~&~{~d~^ * ~}"
    (get-prime-factors n))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-instructions (instructions)
  "Evaluates the INSTRUCTIONS and returns no value."
  (declare (type tpltsspp-program instructions))
  (dolist (instruction instructions)
    (declare (type Instruction instruction))
    ;; Check the actual INSTRUCTION parameters for validity.
    (instruction-signature-check instruction)
    
    (case (instruction-type instruction)
      (:add
        (print-addition
          (first  (instruction-parameters instruction))
          (second (instruction-parameters instruction))))
      
      (:bottlesofbeer
        (print-99-bottles-of-beer))
      
      (:checkprime
        (print-check-prime
          (first (instruction-parameters instruction))))
      
      (:factor
        (print-prime-factorization
          (first (instruction-parameters instruction))))
      
      (:factorial
        (print-factorial
          (first (instruction-parameters instruction))))
      
      (:fibonacci
        (print-Fibonacci-numbers
          (first (instruction-parameters instruction))))
      
      (:fizzbuzz
        (print-FizzBuzz
          (first (instruction-parameters instruction))))
      
      (:helloworld
        (print-hello-world))
      
      (:palindrome
        (print-palindrome
          (first (instruction-parameters instruction))))
      
      (:randnum
        (print-random-integer
          (first  (instruction-parameters instruction))
          (second (instruction-parameters instruction))))
      
      (:randreal
        (print-random-real))
      
      (:sort
        (print-sorted-list
          (first (instruction-parameters instruction))))
      
      (:sum
        (print-sum
          (first (instruction-parameters instruction))))
      
      (otherwise
        (error "Invalid instruction: ~s." instruction))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-TPLTSSPP (code)
  "Interprets the piece of TPLTSSPP CODE and returns no value."
  (process-instructions
    (parser-parse
      (make-parser
        (make-lexer code))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Demonstrate all thirteen available functions.
(interpret-TPLTSSPP
  "add(-1.5, 4)
   bottlesofbeer()
   checkprime(7)
   factor(56)
   factorial(6)
   fibonacci(10)
   fizzbuzz(20)
   helloworld()
   palindrome(\"otto\")
   randnum(2, 16)
   randreal()
   sort([8, 3, 1, 2])
   sum(6)")

;;; -------------------------------------------------------

;; Print the first 100 FizzBuzz elements.
(interpret-TPLTSSPP "fizzbuzz(100)")

;;; -------------------------------------------------------

;; Print the prime factors of the number 120:
;;   2 * 2 * 2 * 3 * 5
(interpret-TPLTSSPP "factor(120)")

;;; -------------------------------------------------------

;; Signal an error as the signature of "helloworld" does not homologate
;; any parameters.
(interpret-TPLTSSPP "helloworld(7)")
