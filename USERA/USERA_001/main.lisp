;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "User:A", also known as "USERA", presented by the Esolang
;; user "OsmineYT" in the year 2019, and founded upon the assignment of
;; brief command names containing the significant constituent "a" to
;; its humble capabilities that include merely ASCII character output,
;; screen clearance, and the arithmetic operations addition and
;; subtraction.
;; 
;; 
;; Concept
;; =======
;; The User:A programming language provides as its sole effective
;; expression the display of ASCII characters, either stated by their
;; literal codes or obtained by mediation of addition or subtraction.
;; A second operation, the clearing of the output screen is capacitated.
;; 
;; 
;; Architecture
;; ============
;; User:A's simplistic nature, which does not wist of storages or
;; variables, abstains from imposing any architecture at all.
;; 
;; 
;; Data Types
;; ==========
;; While printing ASCII characters, only numeric constituents partake in
;; the language's operation, either stated as non-negative ASCII code,
;; or supplied as signed integers to the two available arithmetic
;; warklooms.
;; 
;; 
;; Syntax
;; ======
;; In its syntactical bailiwick, a User:A program is limned by a
;; sequence of commands, short in their designators, and depending on
;; zero or more arguments, with whitespaces betwixt tokens deployed as
;; an obgliation, while tolerated as design elements in any other
;; location.
;; 
;; == GRAMMAR ==
;; A formal description of the language shall be furnished by the
;; Extended Backus-Naur Form (ENBF):
;; 
;;   program   := [ spaces ]
;;             ,  [ command ]
;;             ,  { spaces , command }
;;             ,  [ spaces ]
;;             ;
;;   command   := display | clear | add | subtract ;
;;   display   := "a" , { spaces , asciiCode } ;
;;   clear     := "aa" ;
;;   add       := "a+a" , spaces , number , spaces , number ;
;;   subtract  := "a-a" , spaces , number , spaces , number ;
;;   
;;   asciiCode := digit , [ digit , [ digit ] ] ;
;;   number    := [ "+" | "-" ] , digit , { digit } ;
;;   digit     := "0" | "1" | "2" | "3" | "4"
;;             |  "5" | "6" | "7" | "8" | "9"
;;             ;
;;   spaces    := space , { space } ;
;;   space     := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; User:A's instruction set is compact of four members, two of which are
;; engaged in the printing field, while the second twains provides basic
;; arithmetics.
;; 
;; == OVERVIEW ==
;; An apercu shall adhibit a cursory nortelry anenst the language's
;; capabilities:
;; 
;;   ------------------------------------------------------------------
;;   Command                    | Effect
;;   ---------------------------+--------------------------------------
;;   a {code-1} ... {code-N}    | Prints to the standard output the
;;                              | characters answering to the zero or
;;                              | more integer ASCII codes {code-1}
;;                              | through {code-N}. The displayed
;;                              | characters are not separated.
;;   ..................................................................
;;   aa                         | Clears the screen.
;;   ..................................................................
;;   a+a {augend} {addend}      | Adds to the {augend} the {addend} and
;;                              | returns the sum. Both operands are
;;                              | expected to be signed integers.
;;   ..................................................................
;;   a-a {minuend} {subtrahend} | Subtracts from the {minuend} the
;;                              | {subtrahend} and returns the
;;                              | difference. Both operands are
;;                              | expected to be signed integers.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The implementation is presented in the programming language Common
;; Lisp, with its kenspeckle attributes commorant in the preference of
;; the functional paradigm and the abstinence of object-oriented
;; warklooms.
;; 
;; == FUNCTIONAL PROGRAMMING WITH SCANT OBJECT-ORIENTATION ==
;; This implementation pursuits two objectives:
;; 
;;   (1) FUNCTIONAL PROGRAMMING
;;       The adherence to a preponderantly functional programming style
;;       shall be obeyed, which embraces especially the minimization of
;;       side effects shall and their substitution by return values.
;;   (2) DESISTENCE FROM OBJECT-ORIENTED FEATURES:
;;       An abstinence from object-oriented features incorporates that
;;       classes shall be superseded by list structures and type
;;       definitions.
;; 
;; == (1) FUNCTIONAL PROGRAMMING: REDUCING SIDE EFFECTS ==
;; Function programming subscribes to a function-centric view on program
;; design, with the reduction or perfect desistence from side effects.
;; A consectary thereof, any invocation of a function, supplied with a
;; specific set of parameters, ought to produce the exact same result.
;; 
;; Common Lisp's association with the functional paradigm emerges in its
;; admission of functional objects, a compass which includes such code
;; units as arguments as well as return values. A counterdistinguishing
;; mark, the Lisp dialect's construe does not resort to a stringent
;; coercion of the style's pure variant, such as is imposed, among
;; others, by the programming language Haskell. The beau ideal's purity
;; is subjected to a cambistry for the tolerance of hid effects.
;; 
;; Whereas the sensible nature of states and their modifications is
;; accepted and embraced by this implementation's author, the project
;; shall serve by an inferior role's apportionment to implicit
;; relationships inherent to the object-oriented approach.
;; 
;; One paragon of this enterprise, the ``token-list'' type, maintaining
;; a list of zero or more tokens obtained by scanning a piece of User:A
;; source code in its string form, manifests as a plain Common Lisp
;; ``list'' container. Operations applied to the same, in particular the
;; removal of a consumed item, do not modify its content; in lieu of
;; this, a fresh instance is created and returned.
;; 
;; The simplicity commorant in an entity like the ``token-list'' fails
;; to equiparate to more potent applications. A forbisen of this, the
;; numerous parsing routines exhibit an augmentation in elaboration
;; anent the requirement in their confrontation with a non-destructive
;; deportment: Having parsed and consumed a subset of a token list's
;; items, two values ought to be returned, the incipient constituting
;; the thus generated abstract syntax tree (AST) node; the second, as
;; exclusively the unused tokens shall retain availability for
;; subsequent parsing steps, a modified token list, curtailed of the
;; digested elements, that must be included in the output's
;; circumference.
;; 
;; Common Lisp's respondency to this predicament partakes in the
;; provision of multiple return values for a function --- a contingently
;; lightweight alternative to traditional storage objects, such as lists
;; or vectors. In the use case presented aboon, the first return value
;; would embrace the node, whereas the second comprises the reduced
;; token list copy.
;; 
;; == (2) ALTERNATIVES TO OBJECT-ORIENTATION ==
;; Intertwined and compatible, yet not perfectly equivalent with the
;; topic of functional programming, the reduction or banishment of
;; object-oriented features shall impart a kenspeckle haecceity to this
;; project.
;; 
;; In stringent parlance, the object-oriented notion does neither
;; exclude nor presuppose the functional peer's presence; the focus on
;; states and their manipulation, natheless, almost naturally redes the
;; introduction of side effects into the respective programs. Modeling a
;; concrete or abstract entity from the real world or a problem domain,
;; the potentials for its mutation encroach in its eidolon's conception.
;; If we limn, for instance, a veridical person's virtual equivalency in
;; a programming environment, its labor's income, when modified by a
;; promotion or disranking, should lead to the dependent object's
;; adjustment --- the creation of a new person entity based on the
;; extant example, siclike to a "rebirth" solely incited via a change in
;; some monetary attribute, ostends closely to a betise.
;; 
;; Common Lisp, being a multi-paradigm language, extends its amplexation
;; across functional as well as object-oriented components. This
;; realization of the User:A language yet shall, as far as tenable by
;; rationality and conceivable contemplation, eschew the class-based
;; mode of designment. The elucidated endeavor does not encumber the
;; appertaining solutions with peisant hindrance, however: Lisp,
;; preceding the object-oriented paradigm, and by its inherent wont
;; inclined towards lists for their substitution, endows the programmer
;; with a magnitude of warklumes for the representation of structured
;; data.
;; 
;; The most pertinent, and trenchant for our instrumentalization,
;; accounts for the list type. Its eligibility for illustrations leads
;; to the ``node'' object's nomination in this aspects. A composition of
;; a categorizing type and a sequence of zero or more attributes, an
;; abstract syntax tree (AST) node may be represented in a general mode
;; by mediation of a single class. Its surrogate, the list, encapsulates
;; the requisite data in one or more elements, the inicipient datum in
;; the same refers to the mandatory category; all subsequent positions
;; shall be appropriated by the attribute values, identified in the
;; perquiring contexts by their positional spatiality.
;; 
;; In concrete diction, the potential class
;; 
;;   (defclass node ()
;;     ((type
;;       :initarg :type
;;       :type    keyword)
;;      (attributes
;;       :initarg :attributes
;;       :type    list)))
;; 
;; is mapped to the list structure
;; 
;;   (list TYPE
;;         ATTRIBUTE-1 ... ATTRIBUTE-i ... ATTRIBUTE-N)
;; 
;; and registered with the derived type (``deftype'') specification
;; ``node'' for the sake of superimposing an abstraction tier.
;; 
;; == (1) + (2): FUNCTIONAL STYLE WITHOUT CUSTOM CLASSES ==
;; A forbisen of the champarty in which the functional paradigm and the
;; abstinence from object-oriented tenets participate relates to the
;; scanner, or lexer, responsible for the token acquisition, whence a
;; parser obtains and assembles its pieces.
;; 
;; A common solution to the lexical analyzation process encompasses a
;; dedicated class' definition, maintaining the source code, the current
;; location into the same, and the character resident under this cursor.
;; An assumed linguistic element's extraction proceeds by fathoming and
;; garnering the characters belonging to this unit, driving the position
;; cursor and character --- as a consectary, the lexer object's internal
;; state will be modified.
;; 
;; The ensconcement incorporated into the object context eludes our
;; combination of functional implements without reliance on the
;; object-oriented programming (OOP) conveniences. A ramification
;; thereof, all states, being immutable, necessitate their insertion
;; into a function's argument list, with the athwart compernage,
;; defining a substitution for state modifications, realized in
;; contingently multiple return values.
;; 
;; An exemplary circumstance shall be illustrated by an abstractly
;; delineated operation responsible for extracting some object found at
;; the last position in the source. As a consequence of its perception,
;; apart from the subject itself, the lexer or any substitute entity
;; is encumbered with the onus of memorizing the location in the
;; indagated string immediately following the desinent consumed
;; character for a potential iterum invocation.
;; 
;; The lexer class interface would resolve to an approximate simulacrum
;; of this ilk:
;; 
;;   (defun <MY-LEXER-SCANNING-FUNCTION> (lexer)
;;     (declare (type Lexer lexer))
;;     (with-slots (source position character) lexer
;;       (declare (type string              source))
;;       (declare (type fixnum              position))
;;       (declare (type (or null character) character))
;;       ;; Implement the extraction capability, additionally advancing
;;       ;; the POSITION cursor and updating the current CHARACTER.
;;       <EXTRACTED-OBJECT))
;; 
;; Whereas the functional alternative requires the separate and explict
;; statements of all inputs and outputs, such as furnished here:
;; 
;;   (defun <MY-SCANNING-FUNCTION> (source start-position)
;;     (declare (type string source))
;;     (declare (type fixnum start-position))
;;     ;; Implement the extraction capability, additionally tracking the
;;     ;; the end position.
;;     (values <EXTRACT-OBJECT>
;;             <END-POSITION-IN-THE-SOURCE>))
;; 
;; Please notice especially the multiple return values issued by the
;; functional solution.
;; 
;; == THE COSTAGE OF THIS FUNCTIONAL APPROACH ==
;; Disencumbered from the class-oriented cohesive pussiance, the almost
;; dogmatic purity commorant in the coefficiency of parameters and
;; return values requires a defrayal in many aspects:
;; 
;;   (a) COMPATIBLE INTERFACES
;;       The conceived lexer class, in this concrete circumstance, would
;;       not impose any intricacies in the external interface --- merely
;;       the operating lexer itself ought to be supplied by the caller.
;;       In its internal matters, the treble champarty of source string,
;;       position cursor and current character partake of a
;;       consanguinity that almost inherently excludes inconncinity.
;;         The bartery for its functional counterpart, on the other
;;       hand, inquires with a high mete of diligence into the vincula
;;       betwixt the cooperating elements. In particular, the source
;;       string and the start position of the analyzation effort for any
;;       function intent on extracting intelligence from the former
;;       necessitate their conjoined induction into the parameter list.
;;       Similiter, an advenient nimiety intrudes the routines' return
;;       values, forecause, as a concomitant to the naturally expected
;;       extraction subject, the end position of the source's
;;       consumption, acting in a twifold agency as the contingent start
;;       for the subsequent analyzation function, extends the output to
;;       multiple values.
;;         The input-output coefficiency reverberates especially with a
;;       conspicable obtrusiveness in this aspect, coercing that the two
;;       pieces of communicated gnarity, the start position as an input
;;       from one function, and the end position as an output for a
;;       successor, accommodate concinnity. The consequence accounts for
;;       a more complex rumination anenst the functional interface
;;       design.
;;   
;;   (b) DUBIOSITY IN PERFORMANCE
;;       Maugre a lack of an experimental attest, the repeated creation
;;       of fresh objects in lieu of extant ones' manipulations
;;       indicates the tenability of penalties regarding the
;;       interpreter's efficiency. A circumstantial evidence, the
;;       Common Lisp standard library itself is equipped with
;;       destructive variants of several common operations, such as
;;       list modifications --- very likely at least partially with
;;       these contemplations in the mind.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-12-30
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/USERA"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type predicates.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-p (candidate)
  "Checks whether the CANDIDATE represents a token, that is, a cons
   whose sinistral moeity contains the type in the form of a keyword
   symbol, and the dextral compartment an arbitrary value, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (the boolean
    (not (null
      (typep candidate '(cons keyword T))))))

;;; -------------------------------------------------------

(defun node-p (candidate)
  "Checks whether the CANDIDATE represents an abstract syntax tree node,
   that is, a list of one or more elements, the incipient member of
   which represents the node type in the form of a keyword symbol, with
   the zero or more subsequent items constituting the attributes of
   arbitrary types, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type T candidate))
  (the boolean
    (not (null
      (and
        (listp candidate)
        (plusp (length (the list candidate)))
        (destructuring-bind (head &rest tail) candidate
          (declare (type T    head))
          (declare (type list tail))
          (and
            (keywordp head)
            (listp (the list tail)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype association-list-of (&optional (key-type T) (value-type T))
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
                  (or (null element)
                      (typep element `(cons ,key-type ,value-type))))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype property-list-of (&optional (indicator-type T) (value-type T))
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (the (integer 0 *) (length (the list candidate))))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep indicator indicator-type)
                     (typep value     value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
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

(deftype command ()
  "The ``command'' type enumerates the recognized USERA instruction
   types."
  '(member
    :display-text
    :clear-screen
    :add
    :subtract))

;;; -------------------------------------------------------

(deftype token ()
  "The ``token'' type defines a token as a cons, the left compartment of
   which contains the keyword symbol type, accompanied to its right by
   the token value."
  `(satisfies token-p))

;;; -------------------------------------------------------

(deftype token-list ()
  "The ``token-list'' type defines a list of zero or more ``token''
   objects."
  '(list-of token))

;;; -------------------------------------------------------

(deftype node ()
  "The ``node'' type defines an abstract syntax tree node as a list of
   one or more elements, the incipient element of which contains the
   keyword symbol type, followed by zero or more attributes of any
   value."
  `(satisfies node-p))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``node''
   objects."
  '(list-of node))

;;; -------------------------------------------------------

(deftype context ()
  "The ``context'' type defines the properties associated with an
   abstract syntax tree's (AST) interpretation process as a mapping of
   property names to values, designed using a property list whose
   indicators (keys) assume the keyword symbol property identifiers,
   associated with the respective values conforming to any type."
  '(property-list-of keyword T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a new ``token'' encapsulating the TYPE and the
   VALUE."
  (declare (type keyword type))
  (declare (type T       value))
  (the token (cons type value)))

;;; -------------------------------------------------------

(defun token-type (token)
  "Returns the TOKEN type."
  (declare (type token token))
  (the keyword (car token)))

;;; -------------------------------------------------------

(defun token-value (token)
  "Returns the TOKEN value."
  (declare (type token token))
  (the T (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Checks whether the TOKEN conforms to the EXPECTED-TYPE, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean
    (not (null
      (eq (token-type token) expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of identifiers.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (association-list-of string token) +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  '(("a"   . (:display-text . "a"))
    ("aa"  . (:clear-screen . "aa"))
    ("a+a" . (:add          . "a+a"))
    ("a-a" . (:subtract     . "a-a")))
  "An association list which associates the recognized identifier
   strings with the respective token representations.")

;;; -------------------------------------------------------

(defun get-identifier (identifier)
  "Returns the token associated with the IDENTIFIER name, or ``NIL'' in
   the case of a disrespondency."
  (declare (type string identifier))
  (the (or null token)
    (cdr (assoc identifier +IDENTIFIERS+ :test #'string=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token list operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun token-list-current (token-list)
  "Returns without removing the foremost token from the TOKEN-LIST, or a
   new end-of-file (EOF) token upon its exhaustion."
  (declare (type token-list token-list))
  (the token
    (or (first token-list)
        (make-token :eof NIL))))

;;; -------------------------------------------------------

(defun token-list-matches-p (token-list expected-token-type)
  "Checks whether the TOKEN-LIST's current foremost token conforms to
   the EXPECTED-TOKEN-TYPE, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type token-list token-list))
  (declare (type keyword    expected-token-type))
  (the boolean
    (token-type-p
      (token-list-current token-list)
      expected-token-type)))

;;; -------------------------------------------------------

(defun token-list-peek (token-list)
  "Returns without removing the next token from the TOKEN-LIST, or a new
   end-of-file (EOF) token upon its exhaustion."
  (declare (type token-list token-list))
  (the token
    (or (second token-list)
        (make-token :eof NIL))))

;;; -------------------------------------------------------

(defun token-list-rest (token-list)
  "Returns a new token list based upon the specified TOKEN-LIST with the
   foremost element curtailed."
  (declare (type token-list token-list))
  (the token-list
    (rest token-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Checks whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun sign-character-p (candidate)
  "Checks whether the CANDIDATE represents a mathematical sign symbol,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (find candidate "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun read-word (code start)
  "Beginning at the START position in the CODE, reads a word, demarcated
   at the word boundary, and returns two values:
     (1) the word's characters as a string
     (2) the position in the CODE immediately succeeding the last
         character not included in the consumed word."
  (declare (type string code))
  (declare (type fixnum start))
  (with-open-stream (word (make-string-output-stream))
    (declare (type string-stream word))
    (the (values string fixnum)
      (loop
        for position  of-type fixnum    from start below (length code)
        for character of-type character =    (char code position)
        do
          (if (whitespace-character-p character)
            (loop-finish)
            (write-char character word))
        finally
          (return
            (values (get-output-stream-string word) position))))))

;;; -------------------------------------------------------

(defun read-identifier (code start)
  "Beginning at the START position in the CODE, reads an identifier and
   returns two values:
     (1) a token representation of the consumed identifier
     (2) the position in the CODE immediately succeeding the last
         character embraced by the identifier."
  (declare (type string code))
  (declare (type fixnum start))
  (multiple-value-bind (word end) (read-word code start)
    (declare (type string word))
    (declare (type fixnum end))
    (the (values token fixnum)
      (values (get-identifier word) end))))

;;; -------------------------------------------------------

(defun number-follows-p (code start)
  "Checks whether, beginning at the START position in the CODE, a signed
   or unsigned integer number follows, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string code))
  (declare (type fixnum start))
  (flet ((get-character-at (position)
          "Returns the character at the POSITION in the CODE, or ``NIL''
           if the same violates its bourns."
          (declare (type fixnum position))
          (the (or null character)
            (when (array-in-bounds-p code position)
              (char code position)))))
    (let ((current-character (get-character-at start))
          (next-character    (get-character-at (1+ start))))
      (declare (type (or null character) current-character))
      (declare (type (or null character) next-character))
      (the boolean
        (not (null
          (and current-character
               (or (digit-char-p current-character)
                   (and (sign-character-p current-character)
                        next-character
                        (digit-char-p next-character))))))))))

;;; -------------------------------------------------------

(defun read-number (code start)
  "Beginning at the START position in the CODE, reads a potentially
   signed integer number and returns two values:
     (1) a token representation of the consumed integer
     (2) the position in the CODE immediately succeeding the last
         consumed number constituent."
  (declare (type string code))
  (declare (type fixnum start))
  (with-open-stream (digits (make-string-output-stream))
    (declare (type string-stream digits))
    ;; Collect a potentially extant mathematical sign.
    (when (and (array-in-bounds-p code start)
               (sign-character-p (char code start)))
      (write-char (char code start) digits)
      (incf start))
    (the (values token fixnum)
      (loop
        for position  of-type fixnum    from start below (length code)
        for character of-type character =    (char code position)
        do
          (if (digit-char-p character)
            (write-char character digits)
            (loop-finish))
        finally
          (return
            (values
              (make-token :number
                (parse-integer
                  (get-output-stream-string digits)))
              position))))))

;;; -------------------------------------------------------

(defun read-whitespaces (code start)
  "Beginning at the START position in the CODE, collects a sequence of
   zero or more whitespaces and returns two values:
     (1) a ``separator'' token as a representation of the collected
         whitespaces
     (2) the position in the CODE immediately succeeding the last
         collected whitespace."
  (declare (type string code))
  (declare (type fixnum start))
  (with-open-stream (whitespaces (make-string-output-stream))
    (declare (type string-stream whitespaces))
    (the (values token fixnum)
      (loop
        for position  of-type fixnum    from start below (length code)
        for character of-type character =    (char code position)
        do
          (if (whitespace-character-p character)
            (write-char character whitespaces)
            (loop-finish))
        finally
          (return
            (values
              (make-token :separator
                (get-output-stream-string whitespaces))
              position))))))

;;; -------------------------------------------------------

(defun tokenize (code)
  "Splits the piece of USERA code into its tokens on the word boundaries
   and returns a list encompassing these, always concluded with an
   adscititious end-of-file (EOF) token."
  (declare (type string code))
  (let ((tokens   NIL)
        (position 0))
    (declare (type token-list tokens))
    (declare (type fixnum     position))
    
    (flet ((collect-token (token end)
            "Prepends the TOKEN to the TOKENS list, updates the POSITION
             to END, and returns no value."
            (declare (type token token))
            (declare (type fixnum end))
            (push token tokens)
            (setf position end)
            (values)))
      
      (loop
        while (< position (length code))
        for character of-type character = (char code position)
        do
          (cond
            ((whitespace-character-p character)
              (multiple-value-call #'collect-token
                (read-whitespaces code position)))
            
            ((char= character #\a)
              (multiple-value-call #'collect-token
                (read-identifier code position)))
            
            ((number-follows-p code position)
              (multiple-value-call #'collect-token
                (read-number code position)))
            
            (T
              (error "Unexpected character \"~c\" at position ~d."
                character position)))
        finally
          (push (make-token :eof NIL) tokens)))
    
    (the token-list (nreverse tokens))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of node operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-node (type &rest attributes)
  "Creates and returns a new ``node'' of the specified TYPE, optionally
   associated with the ATTRIBUTES."
  (declare (type keyword     type))
  (declare (type (list-of T) attributes))
  (the node (append (list type) attributes)))

;;; -------------------------------------------------------

(defun node-type (node)
  "Returns the NODE type."
  (declare (type node node))
  (the keyword (first node)))

;;; -------------------------------------------------------

(defun node-attributes (node)
  "Returns a list containing the NODE's attributes."
  (declare (type node node))
  (the (list-of T) (second node)))

;;; -------------------------------------------------------

(defun node-attribute (node index)
  "Returns the NODE's attribute located at the zero-based INDEX, or
   signals an error if the same violates the sequence's bournes."
  (declare (type node          node))
  (declare (type (integer 0 *) index))
  (the T (elt node (1+ index))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (token-list) (values node token-list))
                parse-expression))

;;; -------------------------------------------------------

(defun eat-token (tokens expected-type)
  "Checks whether the current token of the TOKENS list conforms to the
   EXPECTED-TYPE, on confirmation returning a new token list based on
   the TOKENS with the probed element removed; otherwise an error of an
   unspecified type is signaled."
  (declare (type token-list tokens))
  (declare (type keyword    expected-type))
  (let ((candidate (token-list-current tokens)))
    (declare (type token candidate))
    (the token-list
      (if (token-type-p candidate expected-type)
        (token-list-rest tokens)
        (error "Expected a token of the type ~s, but encountered ~s."
          expected-type candidate)))))

;;; -------------------------------------------------------

(defun skip-separator (tokens)
  "Starting with the current item in the TOKENS, skips a sequence of
   zero or more adjacent separator tokens and returns a new token list
   comprehending the potentially curtailed input TOKENS."
  (declare (type token-list tokens))
  (let ((remaining-tokens tokens))
    (declare (type token-list remaining-tokens))
    (loop while (token-list-matches-p remaining-tokens :separator) do
      (setf remaining-tokens
        (token-list-rest remaining-tokens)))
    (the token-list remaining-tokens)))

;;; -------------------------------------------------------

(defmacro with-tokens ((tokens token-list-variable token-variable)
                       &body body)
  "Evaluates the token list TOKENS, binds it to the name
   TOKEN-LIST-VARIABLE, establishes a local symbol macro with the
   designated TOKEN-VARIABLE which always maintains the foremost
   TOKENS item, evaluates the BODY, and returns the last evaluated
   form's results.
   ---
   As an additional convenience, a local function ``eat'' is provided
   which upon invocation applies the effect of the ``eat-token''
   function, indagating the current token regarding its conformance to
   a particular type, however, deviating from the foundation in
   automatically updating the TOKEN-LIST-VARIABLE with the returned,
   curtailed token list upon confirmation."
  `(let ((,token-list-variable ,tokens))
     (declare (type token-list ,token-list-variable))
     (declare (ignorable  ,token-list-variable))
     (symbol-macrolet
         ((,token-variable
           (the token
             (token-list-current ,token-list-variable))))
       (declare (type token ,token-variable))
       (declare (ignorable  ,token-variable))
       (flet ((eat (expected-token-type)
               "Checks whether the current token conforms to the
                EXPECTED-TOKEN-TYPE, on confirmation updating the
                TOKEN-LIST-VARIABLE, purged of the matching token, and
                returns no value."
               (declare (type keyword expected-token-type))
               (setf ,token-list-variable
                 (eat-token ,token-list-variable expected-token-type))
               (values)))
         ,@body))))

;;; -------------------------------------------------------

(defun parse-number (tokens)
  "Based upon the TOKENS, parses a literal number and returns two
   values:
     (1) a node representation of the literal number
     (2) a fresh token list containing the items not consumed from the
         TOKENS.
   ---
   This function handles the grammar
   
     number := [ '+' | '-' ] , digit , { digit } ;"
  (declare (type token-list tokens))
  (the (values node token-list)
    (values
      (make-node :number
        (token-value
          (token-list-current tokens)))
      (token-list-rest tokens))))

;;; -------------------------------------------------------

(defun parse-addition (tokens)
  "Based upon the TOKENS, parses an addition expression and returns two
   values:
     (1) a node representation of the addition operation
     (2) a fresh token list containing the items not consumed from the
         TOKENS.
   ---
   This function handles the grammar
   
     addition := 'a+a' , separator , expression , separator ,
                  expression ;"
  (declare (type token-list tokens))
  (let ((remaining-tokens tokens)
        (augend           NIL)
        (addend           NIL))
    (declare (type token-list     remaining-tokens))
    (declare (type (or null node) augend))
    (declare (type (or null node) addend))
    (setf remaining-tokens (eat-token remaining-tokens :add))
    (setf remaining-tokens (eat-token remaining-tokens :separator))
    (multiple-value-setq (augend remaining-tokens)
      (parse-expression remaining-tokens))
    (setf remaining-tokens (eat-token remaining-tokens :separator))
    (multiple-value-setq (addend remaining-tokens)
      (parse-expression remaining-tokens))
    (the (values node token-list)
      (values
        (make-node :addition augend addend)
        remaining-tokens))))

;;; -------------------------------------------------------

(defun parse-subtraction (tokens)
  "Based upon the TOKENS, parses a subtraction expression and returns
   two values:
     (1) a node representation of the subtraction operation
     (2) a fresh token list containing the items not consumed from the
         TOKENS.
   ---
   This function handles the grammar
   
     subtraction := 'a-a' , separator , expression , separator ,
                     expression ;"
  (declare (type token-list tokens))
  (with-tokens (tokens tokens current-token)
    (eat :subtract)
    (eat :separator)
    (let ((minuend    NIL)
          (subtrahend NIL))
      (declare (type (or null node) minuend))
      (declare (type (or null node) subtrahend))
      (multiple-value-setq (minuend tokens)
        (parse-expression tokens))
      (eat :separator)
      (multiple-value-setq (subtrahend tokens)
        (parse-expression tokens))
      (the (values node token-list)
        (values
          (make-node :subtraction minuend subtrahend)
          tokens)))))

;;; -------------------------------------------------------

(defun parse-expression (tokens)
  "Based upon the TOKENS, parses an expression, that is, either a
   literal number or an arithmetic identifier, and returns two values:
     (1) a node representation of the expression and
     (2) a fresh token list containing the items not consumed from the
         TOKENS."
  (declare (type token-list tokens))
  (the (values node token-list)
    (case (token-type (token-list-current tokens))
      (:number
        (parse-number tokens))
      (:add
        (parse-addition tokens))
      (:subtract
        (parse-subtraction tokens))
      (otherwise
        (error "Invalid expression token: ~s."
          (token-list-current tokens))))))

;;; -------------------------------------------------------

(defun parse-display-text (tokens)
  "Based upon the TOKENS, parses a text display instruction (\"a\") and
   returns two values:
     (1) a node representation of the instruction and
     (2) a fresh token list containing the items not consumed from the
         TOKENS."
  (declare (type token-list tokens))
  (with-tokens (tokens tokens current-token)
    (eat :display-text)
    (let ((character-codes NIL))
      (declare (type (list-of node) character-codes))
      (loop while (token-list-matches-p tokens :separator) do
        (case (token-type (token-list-peek tokens))
          ((:number :add :subtract)
            (setf tokens (eat-token tokens :separator))
            (multiple-value-bind (code-node new-tokens)
                (parse-expression tokens)
              (declare (type node       code-node))
              (declare (type token-list new-tokens))
              (push code-node character-codes)
              (setf tokens new-tokens)))
          (otherwise
            (loop-finish))))
      (the (values node token-list)
        (values
          (make-node :display-text (nreverse character-codes))
          tokens)))))

;;; -------------------------------------------------------

(defun parse-clear-screen (tokens)
  "Based upon the TOKENS, parses a screen clearance instruction (\"aa\")
   and returns two values:
     (1) a node representation of the instruction and
     (2) a fresh token list containing the items not consumed from the
         TOKENS."
  (declare (type token-list tokens))
  (with-tokens (tokens tokens current-token)
    (eat :clear-screen)
    (the (values node token-list)
      (values
        (make-node :clear-screen)
        tokens))))

;;; -------------------------------------------------------

(defun parse-statement (tokens)
  "Based upon the TOKENS, parses a single instruction and returns two
   values:
     (1) a node representation of the parsed instruction and
     (2) a fresh token list containing the items not consumed from the
         TOKENS."
  (declare (type token-list tokens))
  (with-tokens (tokens tokens current-token)
    (the (values node token-list)
      (case (token-type current-token)
        (:display-text
          (parse-display-text tokens))
        (:clear-screen
          (parse-clear-screen tokens))
        ((:add :subtract)
          (parse-expression tokens))
        (otherwise
          (error "Invalid statement token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parse-program (tokens)
  "Based upon the TOKENS, parses a sequence of zero or more instructions
   and returns a ``:program'' node representing the root of the abstract
   syntax tree whose children are comprised of the parsed commands, and
   returns two values:
     (1) the program node embracing all parsed instructions and
     (2) a fresh token list containing the items not consumed from the
         TOKENS."
  (declare (type token-list tokens))
  (with-tokens (tokens tokens current-token)
    (let ((statements NIL))
      (declare (type node-list statements))
      (flet ((collect-node (new-node remaining-tokens)
              "Prepends the NEW-NODE to the STATEMENTS, updates the
               TOKENS to the REMAINING-TOKENS, and returns no value."
              (declare (type node new-node))
              (declare (type token-list remaining-tokens))
              (push new-node statements)
              (setf tokens   remaining-tokens)
              (values)))
        
        ;; Skip contingently trailing whitespaces.
        (setf tokens (skip-separator tokens))
        
        (unless (token-type-p current-token :eof)
          (multiple-value-call #'collect-node
            (parse-statement tokens))
          
          ;; As long as whitespaces occur, skip these and collect the
          ;; next statement, if extant.
          (loop while (token-type-p current-token :separator) do
            (setf tokens (skip-separator tokens))
            (case (token-type current-token)
              (:eof
                (loop-finish))
              ((:display-text :clear-screen :add :subtract :number)
                (multiple-value-call #'collect-node
                  (parse-statement tokens)))
              (otherwise
                (error "Invalid program token: ~s." current-token))))))
      
      (setf tokens (skip-separator tokens))
      (eat :eof)
      
      (the (values node token-list)
        (values
          (make-node :program (nreverse statements))
          tokens)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of context.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-context (&rest options)
  "Creates and returns a new interpretation ``context'', optionally
   populated with the initial OPTIONS, supplied as a property list of
   optional name keyword symbols associated with arbitrary option
   values."
  (declare (type (property-list-of keyword T) options))
  (the context options))

;;; -------------------------------------------------------

(defun context-option (context option-name &optional (default NIL))
  "Returns the CONTEXT option value associated with the OPTION-NAME, or
   the DEFAULT if no such identifier could be detected."
  (declare (type context context))
  (declare (type keyword option-name))
  (declare (type T       default))
  (the T (getf context option-name default)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type context +DEFAULT-CONTEXT+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-CONTEXT+
  (make-context :screen-height 20)
  "The default interpretation context.")

;;; -------------------------------------------------------

(defun visit-node (node context)
  "Processes the NODE using the CONTEXT and, depending upon the NODE
   type, either the ``NIL'' value or a signed integer number."
  (declare (type node    node))
  (declare (type context context))
  (declare (ignorable    context))
  
  (the (or null integer)
    (case (node-type node)
      (:program
        (dolist (statement (node-attributes node))
          (declare (type node statement))
          (visit-node statement context))
        (values))
      
      (:number
        (node-attribute node 0))
      
      (:addition
        (+ (visit-node (node-attribute node 0) context)
           (visit-node (node-attribute node 1) context)))
      
      (:subtraction
        (- (visit-node (node-attribute node 0) context)
           (visit-node (node-attribute node 1) context)))
      
      (:display-text
        (dolist (attribute (node-attributes node))
          (declare (type node attribute))
          (write-char (code-char (visit-node attribute context))))
        (values))
      
      (:clear-screen
        (format T "~v%"
          (context-option context :screen-height 20))
        (values))
      
      (otherwise
        (error "Unrecognized node: ~s." node)))))

;;; -------------------------------------------------------

(defun interpret-USERA (code &key (context +DEFAULT-CONTEXT+))
  "Interprets the piece of USERA source CODE, either employing a
   specified CONTEXT for the definition of attributes, or resorting to
   the +DEFAULT-CONTEXT+ upon its omission, and returns the last
   evaluated instruction's result."
  (declare (type string  code))
  (declare (type context context))
  (the (or null integer)
    (visit-node
      (parse-program
        (tokenize code))
      context)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-USERA "a 72 101 108 108 111 44 32 87 111 114 108 100 33")

;;; -------------------------------------------------------

;; Print "Hello, World!" by using several arithmetic operations,
;; including signed numbers.
(interpret-USERA "a a+a 70 2 101 108 108 111 a+a -100 +144 32 87 111 114 108 a-a 150 50 33")

;;; -------------------------------------------------------

;; Print "Hello, ", clear the screen, and output "World!".
(interpret-USERA "a 72 101 108 108 111 44 32 aa a 87 111 114 108 100 33")
