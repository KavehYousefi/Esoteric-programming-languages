;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Quests", invented by the Esolang user "ChuckEsoteric08".
;; 
;; 
;; Concept
;; =======
;; The esoteric programming language Quests establishes a diorism by its
;; own haecceity in providing the first specimen of a Questa-based, and
;; concomitant, Questa-complete, language.
;; 
;; The following sections shall procure a rather comprehensive
;; parasceve, its compass enumerated imprimis with the central Questa
;; data structure, its context and peculiarities, ere the vinculum with
;; the Quests language will be established. If cognizant in terms of the
;; Questa, you may find better reward in a commencement with the section
;; "QUESTS AND THE QUESTA".
;; 
;; == THE QUESTA ESOTERIC DATA STRUCTURE ==
;; The Questa constitutes an esoteric data structure, itself a double
;; cleronomy's scion, produced by confluence of the stack and queue.
;; 
;; The stack data structure is ascribed to the LIFO principle, a
;; last-in first-out arrangement, where elements are inserted, or
;; "pushed", to the storage head and removed, or "popped" from the same
;; location; thus the most recently enlisted member will always vanish
;; first.
;; 
;; The queue, on the other, furnishes a FIFO storage, a first-in
;; first-out ordering, where elements are inserted, or "enqueued", at
;; the rear, but removed, or "dequeued", from the front; in consectary,
;; the incipient member introduced into the collection will be removed
;; first.
;; 
;; As an apostil, a specimen endowed with the fruition of heterosis
;; resides in existence, the deque, a data structure partaking of both
;; the stack and queue nature, with surplus capabilities. The two
;; data structures' coefficiency, hence, does instantiate a veridical
;; concoction of rationality.
;; 
;; The Questa data structure, as a hybrid of the stack and queue,
;; offers a faint simulacrum of the deque principle, maugre its
;; desistence from the supererogation. A mere quadruple of operations
;; already establishes the complete interface:
;; 
;;   ----------------------------------------------------------
;;   Operation | Effect
;;   ----------+-----------------------------------------------
;;   PUSH      | Inserts a new element at the top.
;;   ..........................................................
;;   POP1      | Removes and returns the element at the top.
;;   ..........................................................
;;   POP2      | Removes and returns the element at the bottom.
;;   ..........................................................
;;   SWAP      | Exchanges the top and bottom elements.
;;   ----------------------------------------------------------
;; 
;; No particular maximum capacity inheres in the Questa, nor any type
;; restrictions affiliated to the elements.
;; 
;; == QUESTS AND THE QUESTA ==
;; At the core of its storage concept, Quests employs the Questa data
;; structure, dignified by being this type's incipient representative.
;; Commands exist to satisfy the four basic operations, PUSH, POP1,
;; POP2, and SWAP, as well as custom functionality besides the minimum.
;; Quests programs may store integers and strings, unbounded in their
;; tally.
;; 
;; == QUESTA-COMPLETENESS ==
;; The Quests language's averment of Questa-completeness is attested in
;; its utilization of the same storage type, as much as its exhaustion
;; considering the accompanying operations.
;; 
;; The treble of requirements imposed upon Questa-completeness shall be
;; produced, in juxtaposition to Quests's assessment in the same
;; context:
;;   
;;   (1) The language utilizes the Questa data structure.
;;       Quests, not solely in its agnomination's appropriation, issues
;;       from the Questa data structure, ordaining its employment as the
;;       central and only collection type.
;;   
;;   (2) The data storage is unbounded.
;;       Quests does not inflict a contranatural march upon its Questa
;;       storage.
;;   
;;   (3) All four basic commands are integrated.
;;       Quests implements and furnishes for their application all of
;;       the four mandated Questa operations, as thee following
;;       associations in their establishment stand witness to:
;;       
;;         ---------------------------------
;;         Questa operation | Quests command
;;         -----------------+---------------
;;         PUSH             | p(x)
;;         .................................
;;         POP1             | <(0)
;;         .................................
;;         POP2             | <(1)
;;         .................................
;;         SWAP             | sw()
;;         ---------------------------------
;; 
;; 
;; Architecture
;; ============
;; With the Questa as the language's sole data structure, the
;; architecture's establishment expresses a mere reiteration of the
;; facts.
;; 
;; The Questa instance deployed for this cause may store integer or
;; string objects. Beside the foundational operations --- PUSH, POP1,
;; POP2, and SWAP ---, two additional facilities are embraced in this
;; version of the collection, represented by the commands "inc" and
;; "dec", and manipulating the top or bottom Questa element without its
;; removal.
;; 
;; 
;; Data Types
;; ==========
;; Relating to data processing, Quests, while maintaining as its central
;; repository the namesake Questa, offers a dichotomy of types partaking
;; in the storage, including in this comprehension integers and strings.
;; 
;; == QUESTA ==
;; The esoteric data structure Questa, a conjunction of the stack and
;; queue, constitutes the paravaunt instrument for data management.
;; 
;; == INTEGERS ==
;; Numeric literals may be induced in the form of signed integers of
;; unbounded magnitude, that is, spanning the range
;; [-infinity, +infinity].
;; 
;; == STRINGS ==
;; String literals are introduced in destitution of any demarcating
;; symbols, such as quotes as a commonality in many other programming
;; languages, instead their presence can be discerned by the statement
;; of one or more characters, commencing with a letter.
;; 
;; 
;; Syntax
;; ======
;; Quests programs are comprised of commands only, each such component
;; introduced by its identifier and concluded with a contingently vacant
;; parameter list, and segregated from the next instance by one or more
;; spaces. Parameters may manifest in integer, string, or command form,
;; separated, if tallying more than one item, by a comma each.
;; 
;; == COMMANDS ==
;; The command designation applies to an identifier of one or more
;; non-whitespace characters' length, followed by a parameter list.
;; 
;; This attribute portion always commences with a left parenthesis and
;; terminates via a right companion. Ensconced in this jumelle, zero or
;; more parameters may be present, depending upon the concrete
;; instruction, either being integers, strings, or expressions, the
;; latter of which refers to the very restricted set of commands
;; returning values themselves. A command of an arity higher than one,
;; that is, requiring two or more inputs, segregates each two parameters
;; by a single comma.
;; 
;; Any subsequent commands are separated by one or more whitespace
;; characters, a category that enumerates spaces, tabs, and newlines.
;; 
;; == INTEGERS ==
;; Signed integer values wone in the language's compass, optionally
;; introduced by a single sign --- plus ("+") or minus ("-") ---, the
;; subsequent positions of which are occupied by decimal digits only,
;; unrestrained in their tally.
;; 
;; == STRINGS ==
;; The string definition embraces a sequence of one or more
;; non-whitespace character, not particularly demarcated, as contraposed
;; with many other programming languages.
;; 
;; == WHITESPACES ==
;; Whitespaces impose a requirement as command separators, and an object
;; of intolerance inside of strings. While the latter impediment also
;; applies to the interstice betwixt a command identifier and its
;; parameter list's opening parenthesis, inside of the ensconcement no
;; such stringency exercises its purview. The nomenclature subsumes into
;; whitespaces the space, tab, and newline character.
;; 
;; == COMMENTS ==
;; No provisions for comments are incorporated in the current language
;; iteration.
;; 
;; == GRAMMAR ==
;; An expression of the language in the Extended Backus-Naur Form (EBNF)
;; shall be procured:
;; 
;;   program       := [ command , { whitespaces , command } ] ;
;;   command       := push
;;                 |  pop
;;                 |  output
;;                 |  increment
;;                 |  decrement
;;                 |  swap
;;                 ;
;;   expression    := integer | string | command ;
;;   push          := "p"   , "(" , expression , ")" ;
;;   pop           := "<"   , "(" , expression , ")" ;
;;   output        := ">"   , "(" , expression , ")" ;
;;   increment     := "inc" , "(" , expression , ")" ;
;;   decrement     := "dec" , "(" , expression
;;                 ,          "," , expression , ")"
;;                 ;
;;   swap          := "sw"  , "(" , ")" ;
;;   
;;   string        := textCharacter , { textCharacter } ;
;;   integer       := [ "+" | "-" ] , digit , { digit } ;
;;   whitespaces   := whitespace , { whitespace } ;
;;   whitespace    := " " | "\t" | "\n" ;
;;   textCharacter := letter | digit | "_" | "-" ;
;;   letter        := "a" | ... | "z" | "A" | ... | "Z" ;
;;   digit         := "0" | "1" | "2" | "3" | "4"
;;                 |  "5" | "6" | "7" | "8" | "9"
;;                 ;
;; 
;; 
;; Instructions
;; ============
;; Quests's instruction set is founded upon the Questa data structure as
;; its unalienable core, augmented by a few adscititious members
;; targeted at the programming's facilitation.
;; 
;; == OVERVIEW ==
;; The following apercu shall avail in the provision of an incipient
;; intelligence anenst the language's faculties, as well as counterpose
;; the same with the Questa interface.
;; 
;;   ------------------------------------------------------------------
;;   Command  | Effect
;;   ---------+--------------------------------------------------------
;;   p(x)     | Pushes the value {x} unto the top of the Questa.
;;            | This function represents the Questa PUSH operation.
;;   ..................................................................
;;   <(x)     | If {x} equals zero (0), pops and returns the Questa's
;;            | top element.
;;            | If {x} equals one (1), removes and returns the Questa's
;;            | bottom element.
;;            | This function represents, for the case of {x} = 0, the
;;            | Questa POP1 operation.
;;            | This function represents, for the case of {x} != 0, the
;;            | Questa POP2 operation.
;;   ..................................................................
;;   >(x)     | If {x} equals zero (0), pops the Questa's top element
;;            | and prints the same to the standard output.
;;            | If {x} equals one (1), pops the Questa's bottom
;;            | element and prints the same to the standard output.
;;   ..................................................................
;;   inc(x)   | If {x} equals zero (0), increments the Questa's top
;;            | element by one.
;;            | If {x} equals one (1), increments the Questa's bottom
;;            | element by one.
;;   ..................................................................
;;   dec(x,y) | If {x} equals zero (0), and the Questa's top element
;;            | equals zero (0), moves the instruction pointer to the
;;            | {y}-th command. Please note that the instructions are
;;            | enumerated starting with the index zero (0).
;;            | If {x} equals zero (0), and the Questa's top element
;;            | does not equal zero (0), decrements the Questa's top
;;            | element by one (1).
;;            | If {x} equals one (1), and the Questa's bottom element
;;            | equals zero (0), moves the instruction pointer to the
;;            | {y}-th command. Please note that the instructions are
;;            | enumerated starting with the index zero (0).
;;            | If {x} equals one (1), and the Questa's bottom element
;;            | does not equal zero (0), decrements the Questa's bottom
;;            | element by one (1).
;;   ..................................................................
;;   sw()     | Swaps the Questa's top and bottom elements.
;;            | This function represents the Questa SWAP operation.
;;   ------------------------------------------------------------------
;; 
;; == "DEC": DECREMENT OR GOTO ==
;; The slightly convoluted operations of the "dec" command, which in its
;; nature coalesces both an arithmetic deduction and a navigational
;; moeity, shall be accompanied with the following pseudocode:
;; 
;;   if (x = 0) and (questa.first = 0) then
;;     go to command at index y
;;   else if (x = 0) and (questa.first != 0) then
;;     decrement questa.first
;;   else if (x = 1) and (questa.last = 0) then
;;     go to command at index y
;;   else if (x = 1) and (questa.last != 0) then
;;     decrement questa.bottom
;;   else
;;     signal error "Invalid value for 'x': ", x
;;   end if
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Quests's maturity, measured at the time of this implementation's
;; production in few months only, eventuates a comprehensible lack of
;; details in the specification. A selected subset shall be enumerated
;; in the following sections, in conjunction with an apologia anenst the
;; chosen response.
;; 
;; == WHICH DATA TYPES DO EXIST? ==
;; The data types' delineation as issues by this document describes an
;; extrapolation from the original text's command and example sections.
;; This tacit treatise on the nature of objects does not respond
;; perfectly to certitude concerning the subject. The concrete numeric
;; types, in particular, may be more elaborated than conceived by
;; deduction. Natheless, the following assumptions have been exercised
;; as tenable:
;; 
;;   - Numeric data types are restricted to integers, signed and
;;     unbounded regarding their magnitude.
;;   - Strings may be composed of any non-whitespace character, but must
;;     not be empty.
;; 
;; The incorporation, for instance, of real-valued or complex numbers
;; may become a future language iteration's cynosure.
;; 
;; == HOW SHALL ATTEMPTED ARITHMETICS ON STRINGS PROCEED? ==
;; The two adscititious Questa operations admitted to Quests, "inc" and
;; "dec", perform a rudimentary ilk of arithmetics upon the top or
;; bottom element. As the underlying Questa may include strings, the
;; respondency to such a request suffers from a deficit in its
;; specification. Three options may be extended with tenability:
;; 
;;   (a) An error of unspecified type shall be signaled.
;;   (b) No causatum shall issue.
;;   (c) An increment/decrement based on lexicographical rules shall
;;       apply.
;; 
;; The third possibility (c), while meted with the highest degree of
;; comfort, limns a rather unexpected behavior, and thus may be waived
;; in a preliminary manner. The option (b), similarly inobtrusive,
;; nevertheless inflicts a loss of intelligence concerning the program's
;; state, as invalid operations may elude, and thus corrupt, the user's
;; intentions. A corollary of this treatise, an error of any kind, as
;; proffered by the first option (a), has been admitted into service.
;; 
;; 
;; Implementation
;; ==============
;; The general implementation appropriates a tripartite model for the
;; government of the programming language interpretation, consisting of
;; 
;;   (1) Lexical analyzation, which generates from the source code
;;       an ordered series of tokens.
;;   (2) Parsing, in the course of which an assemblage is exercised to
;;       build from the tokens a vector of instructions, also entailing
;;       the requisite operands.
;;   (3) Interpretation, by whose mediation the instruction vector is
;;       assigned an actual effectivity.
;; 
;; The section below shall avail in the distinct processes' elucidation.
;; 
;; == (1) LEXICAL ANALYZATION: SOURCE CODE BECOMES TOKENS ==
;; The Quests source code, supplied in a string form, experiences in its
;; inchoation a transformation into an ordered series of tokens, the
;; same serve in the representation of significant elements gleaned from
;; the program. Forbisens for such embrace numbers, identifiers, and
;; punctuation marks.
;; 
;; A token in its most simple mold, as is reified in this
;; implementation, does not partake of a more elaborate role than a
;; tuple of type and value, the former acts in the agency of a
;; categorizing criterion, while the latter contains the concrete datum;
;; for instance, the integer number 5 might produce a token compact of
;; (:integer, 5).
;; 
;; The veridical intelligence invested in this process is defined as the
;; onus of a lexical analyzer, or short "lexer"; a component responsible
;; for the discernment of significant elements and their reformulation
;; as tokens.
;; 
;; == (2) PARSING: TOKENS ASSEMBLE INTO INSTRUCTIONS ==
;; The parsing stage constructs from the extant token series a vector
;; of instructions as a representation of the source program, each such
;; containing zero or more operands as its arguments.
;; 
;; Whereas in the tokens the data perspective upon the Quests code
;; resides, a higher-level conspectuity on the language facilities is
;; accommodated by the parsing. Related by an example, the delivery of
;; the token sequence
;;   (:identifier       "p")
;;   (:left-parenthesis "(")
;;   (:number            5)
;;   (:right-parenthesis ")")
;; as a collection of separate objects does not bear any operative
;; value. If, however, assembled into an instruction
;;   (INSTRUCTION :type :push, (OPERAND :type number :value 5))
;; the newly introduced context enlightens the interpreter by stating
;; that the Quests PUSH ("p" or Questa PUSH) operation shall be applied
;; using the numeric argument 5.
;; 
;; == (3) INTERPRETATION: APPLYING EFFECT TO THE INSTRUCTIONS ==
;; A perfectly static construct, the instruction vector contains all
;; requisite information for delineating a Quests program's
;; functionality, without applying the same. The interpreter is ordained
;; to this duty, embuing the statements with an operative character.
;; 
;; In order to realize this accomplishment, the instruction pointer (IP)
;; is located at the first position (index 0), thus designating the
;; incipient instruction. The statement's processing as a concomitant
;; produces a translation of the instruction pointer, either to the next
;; instruction or, if confronted with a goto operation, to any other
;; position in the vector.
;; 
;; Operands may assume one of three forms: numbers, strings, or
;; instructions; the latter case incites a recursive command processing
;; in destitution of a usual instruction pointer advacement --- except
;; for goto operations, which always produce a side effect.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-05-31
;; 
;; Sources:
;;   [esolang2024Quests]
;;   The Esolang contributors, "Quests", January 21st, 2024
;;   URL: "https://esolangs.org/wiki/Quests"
;;   
;;   [esolang2023Questa]
;;   The Esolang contributors, "Questa", September 4th, 2023
;;   URL: "https://esolangs.org/wiki/Questa"
;;   Notes:
;;     - Treatise on the esoteric data structure "Questa".
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, as a forbisen, ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE, associated with
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

(deftype instruction-type ()
  "The ``instruction-type'' type enumerates the recognized instruction
   types."
  '(member
    :push
    :pop
    :output
    :increment
    :decrement
    :swap))

;;; -------------------------------------------------------

(deftype operand-type ()
  "The ``operand-type'' type enumerates the recognized variants of
   instruction operands."
  '(member
    :number
    :string
    :instruction))

;;; -------------------------------------------------------

(deftype tail-pointer ()
  "The ``tail-pointer'' type defines a cons suitable as a tracking
   implement for efficient insertions at the tail of a list."
  '(cons T (or null (cons T null))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Token".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (type value)))
  "The ``Token'' class serves in the encapsulation of a significant
   portion distilled from an analyzed Quests program."
  (type  (error "Missing token type.") :type keyword :read-only T)
  (value NIL                           :type T       :read-only T))

;;; -------------------------------------------------------

(defun token-type-p (token &rest valid-types)
  "Checks whether the TOKEN type matches any among the VALID-TYPES,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token             token))
  (declare (type (list-of keyword) valid-types))
  (the boolean
    (not (null
      (member (token-type token) valid-types :test #'eq)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun identifier-character-p (character)
  "Checks whether the CHARACTER features among the valid constituents
   for an identifier, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (alphanumericp character)
          (find character "-_<>?!" :test #'char=))))))

;;; -------------------------------------------------------

(defun sign-character-p (character)
  "Checks whether the CHARACTER represents a numeric sign, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "+-" :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Lexer".                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Lexer ()
  ((source
    :initarg       :source
    :initform      (error "Missing lexer source.")
    :type          string
    :documentation "The Quests program to analyze.")
   (position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION in the
                    SOURCE."))
  (:documentation
    "The ``Lexer'' provides a class responsible for the discernment of
     significant objects in a piece of Quests source code and their
     reproduction in the form of tokens."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((lexer Lexer) &key)
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a ``Lexer'' which analyzes the SOURCE."
  (declare (type string source))
  (the Lexer (make-instance 'Lexer :source source)))

;;; -------------------------------------------------------

(defun lexer-advance (lexer)
  "Moves the LEXER to the next position in its source, if possible,
   updates the current character, and returns the modified LEXER."
  (declare (type Lexer lexer))
  (with-slots (source position character) lexer
    (declare (type string              source))
    (declare (type fixnum              position))
    (declare (type (or null character) character))
    (setf character
      (when (array-in-bounds-p source (1+ position))
        (char source (incf position)))))
  (the Lexer lexer))

;;; -------------------------------------------------------

(defun lexer-read-identifier (lexer)
  "Starting at the current position into the LEXER's source, reads an
   identifier and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :identifier
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop
            while (and character (identifier-character-p character))
            do
              (write-char character content)
              (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-read-number (lexer)
  "Starting at the current position into the LEXER's source, reads a
   number and returns a token representation thereof."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :number
        (parse-integer
          (with-output-to-string (digits)
            (declare (type string-stream digits))
            ;; Write optional sign.
            (when (and character (sign-character-p character))
              (write-char character digits)
              (lexer-advance lexer))
            (unless (and character (digit-char-p character))
              (error "Expected at least one digit to follow, but ~
                      encountered '~a'."
                character))
            ;; Write digits.
            (loop while (and character (digit-char-p character)) do
              (write-char character digits)
              (lexer-advance lexer))))))))

;;; -------------------------------------------------------

(defun lexer-read-spaces (lexer)
  "Starting at the current position into the LEXER's source, reads one
   or more adjacent whitespaces and returns a token representation
   thereof.
   ---
   Note that abutting whitespaces are coalesced into a single token
   object, whereas their sequence is retained verbatim."
  (declare (type Lexer lexer))
  (with-slots (character) lexer
    (declare (type (or null character) character))
    (the Token
      (make-token :spaces
        (with-output-to-string (content)
          (declare (type string-stream content))
          (loop
            while (and character (whitespace-character-p character))
            do
              (write-char character content)
              (lexer-advance lexer)))))))

;;; -------------------------------------------------------

(defun lexer-get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to each invocation
   uniformly by returning a fresh token of the type ``:eof'', signifying
   an end-of-file (EOF) situation."
  (declare (type Lexer lexer))
  
  (the Token
    (with-slots (character) lexer
      (declare (type (or null character) character))
      
      (cond
        ((null character)
          (make-token :eof NIL))
        
        ((whitespace-character-p character)
          (lexer-read-spaces lexer))
        
        ((alpha-char-p character)
          (lexer-read-identifier lexer))
        
        ((digit-char-p character)
          (lexer-read-number lexer))
        
        ((sign-character-p character)
          (lexer-read-number lexer))
        
        ((char= character #\<)
          (prog1
            (make-token :identifier (string character))
            (lexer-advance lexer)))
        
        ((char= character #\>)
          (prog1
            (make-token :identifier (string character))
            (lexer-advance lexer)))
        
        ((char= character #\()
          (prog1
            (make-token :left-parenthesis character)
            (lexer-advance lexer)))
        
        ((char= character #\))
          (prog1
            (make-token :right-parenthesis character)
            (lexer-advance lexer)))
        
        ((char= character #\,)
          (prog1
            (make-token :comma character)
            (lexer-advance lexer)))
        
        (T
          (error "Invalid character '~c' at position ~d."
            character (slot-value lexer 'position)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Operand".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Operand
  (:constructor make-operand (type &optional (value NIL))))
  "The ``Operand'' class represents an argument to an instruction."
  (type  (error "Missing operand type.")
         :type      operand-type
         :read-only T)
  (value NIL
         :type      T
         :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Instruction
  (:constructor make-instruction (type operands operand-p)))
  "The ``Instruction'' class represents a command and its arguments."
  (type      (error "Missing instruction type.")
             :type      instruction-type
             :read-only T)
  (operands  NIL
             :type      (list-of Operand)
             :read-only T)
  (operand-p NIL
             :type      boolean
             :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction validation.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of instruction-type (list-of T))
               +INSTRUCTION-SIGNATURES+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-SIGNATURES+ (make-hash-table :test #'eq)
  "Associates with each instruction type a list of type specifiers in
   representation of the modeled Quests command's signature, that is,
   the tally and types of expected inputs.")

;;; -------------------------------------------------------

(flet ((add-instruction-signature (instruction-type type-specifiers)
        "Associates the INSTRUCTION-TYPE with the TYPE-SPECIFIERS, each
         member of this list providing the type information for another
         parameter."
        (declare (type instruction-type instruction-type))
        (declare (type (list-of T)      type-specifiers))
        (setf (gethash instruction-type +INSTRUCTION-SIGNATURES+)
              type-specifiers)
        (values)))
  (add-instruction-signature :push      '(T))
  (add-instruction-signature :pop       '(bit))
  (add-instruction-signature :output    '(bit))
  (add-instruction-signature :increment '(bit))
  (add-instruction-signature :decrement '(bit integer))
  (add-instruction-signature :swap      '())
  (values))

;;; -------------------------------------------------------

(defun validate-instruction (instruction-type operands)
  "Checks whether the evaluated OPERANDS conform to the
   INSTRUCTION-TYPE's established signature for the modeled Quests
   command, returning on confirmation the unmodified OPERANDS, otherwise
   signaling an error of an unspecified type."
  (declare (type instruction-type instruction-type))
  (declare (type (list-of T)      operands))
  
  (multiple-value-bind (expected-types contains-instruction-type-p)
      (gethash instruction-type +INSTRUCTION-SIGNATURES+)
    (declare (type (list-of T) expected-types))
    (declare (type T           contains-instruction-type-p))
    
    (flet ((check-instruction-type ()
            "Checks whether the INSTRUCTION-TYPE could be detected in
             the table of instruction signatures, returning on
             confirmation no value, otherwise signaling an unspecified
             error."
            (when (not contains-instruction-type-p)
              (error "No signature for the instruction type ~s found."
                instruction-type))
            (values))
           
           (check-operand-tally ()
            "Checks whether the number of OPERANDS equals the number of
             parameters expected by the instruction, returning on
             confirmation no value, otherwise signaling an unspecified
             error."
            (let ((number-of-expected-types (length expected-types))
                  (number-of-operands       (length operands)))
              (declare (type fixnum number-of-expected-types))
              (declare (type fixnum number-of-operands))
              (unless (= number-of-expected-types number-of-operands)
                (error "The instruction type ~s expected ~d ~
                        parameters, but ~d have been supplied."
                  instruction-type
                  number-of-expected-types
                  number-of-operands)))
            (values))
           
           (check-operand-types ()
            "Checks whether the OPERANDS values match the expected
             instruction parameter types, returning on confirmation no
             value, otherwise signaling an unspecified error."
            (let ((mismatches NIL))
              (declare (type (list-of T) mismatches))
              (setf mismatches
                (let ((operand-index 1))
                  (declare (type fixnum operand-index))
                  (mapcan
                    #'(lambda (expected-type operand)
                        (declare (type T expected-type))
                        (declare (type T operand))
                        (prog1
                          (unless (typep operand expected-type)
                            (list operand-index expected-type operand))
                          (incf operand-index)))
                    expected-types
                    operands)))
              (when mismatches
                (error "The following operands do not match the ~
                        instruction type '~s':~
                        ~{~&The ~d. operand should be of the type ~
                            ~s, but demonstrates a value of ~s.~}"
                  instruction-type mismatches)))
            (values)))
      
      (check-instruction-type)
      (check-operand-tally)
      (check-operand-types)))
  
  (the (list-of T) operands))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Parser".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of string instruction-type) +COMMAND-TYPES+))

;;; -------------------------------------------------------

(defparameter +COMMAND-TYPES+
  (make-hash-table :test #'equal)
  "Associates the valid command names with instruction types in order
   to establish a nexus betwixt the command invocations in a piece of
   Quests source code and the ``Instruction'' representations thereof.")

;;; -------------------------------------------------------

(flet ((add-command-type (identifier type)
        "Associates the command IDENTIFIER with the instruction type
         and returns no value."
        (declare (type string           identifier))
        (declare (type instruction-type type))
        (setf (gethash identifier +COMMAND-TYPES+) type)
        (values)))
  (add-command-type "p"   :push)
  (add-command-type "<"   :pop)
  (add-command-type ">"   :output)
  (add-command-type "inc" :increment)
  (add-command-type "dec" :decrement)
  (add-command-type "sw"  :swap)
  (values))

;;; -------------------------------------------------------

(defun get-instruction-type (identifier)
  "Returns the instruction type associated with the command IDENTIFIER,
   or signals an error if no correlation could be established."
  (declare (type string identifier))
  (the instruction-type
    (or (gethash identifier +COMMAND-TYPES+)
        (error "Unrecognized command name: ~s." identifier))))

;;; -------------------------------------------------------

(defun command-token-p (token)
  "Checks whether the TOKEN represents a command name, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (token-type-p token :identifier)))))

;;; -------------------------------------------------------

(defun parameter-token-p (token)
  "Checks whether the TOKEN represents a command parameter, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token token))
  (the boolean
    (not (null
      (token-type-p token :identifier :number)))))

;;; -------------------------------------------------------

(defclass Parser ()
  ((lexer
    :initarg       :lexer
    :initform      (error "Missing lexer for parser.")
    :type          Lexer
    :documentation "The lexer reponsible for the delivery of tokens.")
   (current-token
    :initarg       :current-token
    :initform      (make-token :eof NIL)
    :type          Token
    :documentation "The most recent token obtained from the LEXER."))
  (:documentation
    "The ``Parser'' class constitutes the responsible unit for
     generating an instruction vector from a sequence of tokens obtained
     from a lexer."))

;;; -------------------------------------------------------

(declaim (ftype (function (Parser) (list-of Operand))
                parser-parse-parameter-list))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((parser Parser) &key)
  (declare (type Parser parser))
  (with-slots (lexer current-token) parser
    (declare (type Parser parser))
    (declare (type Token  current-token))
    (setf current-token (lexer-get-next-token lexer)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun make-parser (lexer)
  "Creates and returns a new ``Parser'' which obtains its tokens from
   the LEXER."
  (declare (type Lexer lexer))
  (the Parser (make-instance 'Parser :lexer lexer)))

;;; -------------------------------------------------------

(defun parser-eat (parser expected-token-type)
  "Checks whether the PARSER's currently stored token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation querying and storing the next
   token from the internally managed lexer and returning the modified
   PARSER, otherwise signaling an error of an unspecified type."
  (declare (type Parser  parser))
  (declare (type keyword expected-token-type))
  (with-slots (lexer current-token) parser
    (declare (type Token current-token))
    (if (eq (token-type current-token) expected-token-type)
      (setf current-token (lexer-get-next-token lexer))
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type current-token)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-skip-spaces (parser)
  "Parses a sequence of zero or more adjacent space tokens processed by
   the PARSER and returns the modified PARSER."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (loop while (token-type-p current-token :spaces) do
      (parser-eat parser :spaces)))
  (the Parser parser))

;;; -------------------------------------------------------

(defun parser-parse-parameter (parser)
  "Parses a command parameter using the PARSER and returns an operand
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (the Operand
      (case (token-type current-token)
        (:identifier
          (let ((identifier current-token))
            (declare (type Token identifier))
            (parser-eat parser :identifier)
            (parser-skip-spaces parser)
            (cond
              ((token-type-p current-token :left-parenthesis)
                (make-operand :instruction
                  (make-instruction
                    (get-instruction-type (token-value identifier))
                    (parser-parse-parameter-list parser)
                    T)))
              (T
                (make-operand :string (token-value identifier))))))
        (:number
          (prog1
            (make-operand :number (token-value current-token))
            (parser-eat parser :number)))
        (otherwise
          (error "Invalid parameter token: ~s." current-token))))))

;;; -------------------------------------------------------

(defun parser-parse-parameter-list (parser)
  "Parses a sequence of zero or more parameters using the PARSER and
   returns a list of operand representations thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((parameters NIL))
      (declare (type (list-of Operand) parameters))
      (parser-eat parser :left-parenthesis)
      (parser-skip-spaces parser)
      (loop while (parameter-token-p current-token) do
        (push (parser-parse-parameter parser) parameters)
        (parser-skip-spaces parser)
        (case (token-type current-token)
          (:comma
            (parser-eat parser :comma)
            (parser-skip-spaces parser))
          (:right-parenthesis
            (loop-finish))))
      (parser-skip-spaces parser)
      (parser-eat parser :right-parenthesis)
      (the (list-of Operand) (nreverse parameters)))))

;;; -------------------------------------------------------

(defun parser-parse-command (parser)
  "Parses a command using the PARSER and returns an instruction
   representation thereof."
  (declare (type Parser parser))
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((operation current-token))
      (declare (type Token operation))
      (if (command-token-p operation)
        (parser-eat parser (token-type current-token))
        (error "Invalid command name token: ~s." current-token))
      (the Instruction
        (make-instruction
          (get-instruction-type (token-value operation))
          (parser-parse-parameter-list parser)
          NIL)))))

;;; -------------------------------------------------------

(defun parser-parse (parser)
  "Parses the Quests program reproduced by the PARSER and returns the
   recognized instructions as a one-dimensional simple array."
  (declare (type Parser parser))
  (parser-skip-spaces parser)
  (with-slots (current-token) parser
    (declare (type Token current-token))
    (let ((commands NIL))
      (declare (type (list-of Instruction) commands))
      (loop while (command-token-p current-token) do
        (push (parser-parse-command parser) commands)
        (parser-skip-spaces parser))
      (parser-eat parser :eof)
      (the (simple-array Instruction (*))
        (coerce (nreverse commands)
          '(simple-array Instruction (*)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Questa".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Questa ()
  ((elements
    :initform      (cons 'head NIL)
    :type          (list-of T)
    :documentation "The list of elements. Necessarily never empty, as
                    the TAIL pointer must reference its desinent cons,
                    the first and initial element constitutes a 'dummy'
                    cell with the value ('head . NIL).")
   (tail
    :initform      NIL
    :type          (or null tail-pointer)
    :documentation "A pointer to the last cons of the ELEMENTS, employed
                    as a warkloom for efficient insertions at the
                    Questa's rear."))
  (:documentation
    "The ``Questa'' class models the esoteric data structure 'Questa'
     (QUEue and STAck), a storage similar to the deque, but amenable to
     insertions only at its front.
     ---
     Please note, if intent on this class' maintenance, that the first
     ELEMENTS list cons always constitutes a 'dummy' node, installed in
     order to ascertain, with its 'cdr' (pointer) moeity, a source for
     the tail-cons (please see the TAIL slot) to possess a reference for
     queries and insertion at the ELEMENTS' tail. The dummy node does
     neither contribute to the size of the Questa, nor does it store
     any element in its 'car' place; in consectary, even clearing the
     ensconcing data structure does not eradicate this particular
     cons."))

;;; -------------------------------------------------------

(define-condition Empty-Questa-Error (error)
  ((questa
    :initarg       :questa
    :initform      NIL
    :reader        empty-questa-error-questa
    :type          (or null Questa)
    :documentation "The offended Questa.")
   (cause
    :initarg       :cause
    :initform      NIL
    :reader        empty-questa-error-cause
    :type          T
    :documentation "An optional message to prepend to the overall
                    error report."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Questa-Error condition))
      (declare (type destination        stream))
      (format stream "~@[~a ~]The Questa is empty."
        (empty-questa-error-cause condition))))
  (:documentation
    "Signals that an operation, which relies on at least one element's
     presence in the Questa, has been attempted on an empty instance."))

;;; -------------------------------------------------------

(defun signal-empty-questa-error (questa &optional (cause NIL))
  "Signals an ``Empty-Questa-Error'' involving the QUESTA in conjunction
   with an optional CAUSE, which defaults to ``NIL''."
  (declare (type Questa questa))
  (declare (type T      cause))
  (error 'Empty-Questa-Error :questa questa :cause cause))

;;; -------------------------------------------------------

;; Standard operations.
(declaim (ftype (function (Questa T) (values))      questa-push))
(declaim (ftype (function (Questa)   T)             questa-pop1))
(declaim (ftype (function (Questa)   T)             questa-pop2))
(declaim (ftype (function (Questa)   boolean)       questa-swap))

;; Extended, non-standard operations.
(declaim (ftype (function (Questa)   T)             questa-peek1))
(declaim (ftype (function (Questa)   T)             questa-peek2))
(declaim (ftype (function (Questa T) T)             questa-set1))
(declaim (ftype (function (Questa T) T)             questa-set2))
(declaim (ftype (function (Questa)   (values))      questa-clear))
(declaim (ftype (function (Questa)   boolean)       questa-is-empty))
(declaim (ftype (function (Questa)   (integer 0 *)) questa-size))

;; Additional operations.
(declaim (ftype (function (Questa)   list)          questa-element-list))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((questa Questa) &key)
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (setf tail elements))
  (the Questa questa))

;;; -------------------------------------------------------

(defmethod print-object ((questa Questa) stream)
  (declare (type Questa      questa))
  (declare (type destination stream))
  (format stream "Questa(~{~a~^, ~})"
    (rest (slot-value questa 'elements))))

;;; -------------------------------------------------------

(defun make-questa ()
  "Creates and returns an empty Questa."
  (the Questa (make-instance 'Questa)))

;;; -------------------------------------------------------

(defun questa-push (questa new-element)
  "Adds the NEW-ELEMENT to the front of the QUESTA and returns no value.
   ---
   This function represents the Questa operation 'PUSH'."
  (declare (type Questa questa))
  (declare (type T      new-element))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (cond
      ((questa-is-empty questa)
        (setf (cdr tail) (cons new-element NIL))
        (setf tail       (cdr tail)))
      (T
        (push new-element elements)
        (rotatef (first elements) (second elements)))))
  (values))

;;; -------------------------------------------------------

(defun questa-pop1 (questa)
  "If the QUESTA is not empty, removes and returns the element at its
   front, otherwise signals an ``Empty-Questa-Error''.
   ---
   This function represents the Questa operation 'POP1'."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa "Cannot pop the top element.")
        (prog1
          ;; As the first item of the ELEMENTS always constitutes a
          ;; "dummy", the true front element is represented by the
          ;; second in its collection.
          (second elements)
          ;; Swap the head and the second element in order to be able to
          ;; use the built-in ``pop'' function on the latter, ultimately
          ;; rendering the head again the first element.
          (rotatef (first elements) (second elements))
          ;; Pop the second element, which has temporarily been moved to
          ;; the front of the ELEMENTS for this exact reason.
          (pop elements)
          ;; Did we just remove the TAIL pointer?
          ;; => Redirect it to the ELEMENTS.
          (when (= (length elements) 1)
            (setf tail elements)))))))

;;; -------------------------------------------------------

(defun questa-pop2 (questa)
  "If the QUESTA is not empty, removes and returns the element at its
   rear, otherwise signals an ``Empty-Questa-Error''."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot pop the bottom element.")
        (prog1
          (car tail)
          (setf elements (nbutlast elements))
          (setf tail     (last elements)))))))

;;; -------------------------------------------------------

(defun questa-swap (questa)
  "Exchanges the position of the first and the last element in the
   QUESTA, returning a ``boolean'' value of ``T'' if the QUESTA contains
   at least one element and thus responds to this operation, otherwise
   ``NIL''."
  (declare (type Questa questa))
  (the boolean
    (when (> (questa-size questa) 1)
      (with-slots (elements tail) questa
        (declare (type list         elements))
        (declare (type tail-pointer tail))
        (rotatef (second elements) (car tail)))
      T)))

;;; -------------------------------------------------------

(defun questa-peek1 (questa)
  "If the QUESTA is not empty, returns without removing the element at
   its front, otherwise signals an ``Empty-Questa-Error''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements) questa
    (declare (type (list-of T) elements))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot peek the top element.")
        (second elements)))))

;;; -------------------------------------------------------

(defun questa-peek2 (questa)
  "If the QUESTA is not empty, returns without removing the element at
   its end, otherwise signals an ``Empty-Questa-Error''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (tail) questa
    (declare (type tail-pointer tail))
    (the T
      (if (questa-is-empty questa)
        (signal-empty-questa-error questa
          "Cannot peek the bottom element.")
        (car tail)))))

;;; -------------------------------------------------------

(defun questa-set1 (questa new-top)
  "Replaces the first QUESTA element with the NEW-TOP and returns the
   replaced element.
   ---
   An ``Empty-Questa-Error'' is signaled if the QUESTA is empty.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (declare (type T      new-top))
  (the T
    (if (questa-is-empty questa)
      (signal-empty-questa-error questa "Cannot set the top element.")
      (with-slots (elements) questa
        (declare (type (list-of T) elements))
        (prog1
          (second elements)
          (setf (car (rest elements)) new-top))))))

;;; -------------------------------------------------------

(defun questa-set2 (questa new-bottom)
  "Replaces the first QUESTA element with the NEW-BOTTOM and returns the
   replaced element.
   ---
   An ``Empty-Questa-Error'' is signaled if the QUESTA is empty.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (declare (type T      new-bottom))
  (the T
    (if (questa-is-empty questa)
      (signal-empty-questa-error questa
        "Cannot set the bottom element.")
      (with-slots (tail) questa
        (declare (type tail-pointer tail))
        (the T
          (prog1
            (car tail)
            (setf (car tail) new-bottom)))))))

;;; -------------------------------------------------------

(defun questa-clear (questa)
  "Removes all elements from the QUESTA and returns no value.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (setf (cdr elements) NIL)
    (setf tail          elements))
  (values))

;;; -------------------------------------------------------

(defun questa-size (questa)
  "Returns the number of elements stored in the QUESTA.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements) questa
    (declare (type (list-of T) elements))
    (the (integer 0 *)
      (1- (length elements)))))

;;; -------------------------------------------------------

(defun questa-is-empty (questa)
  "Checks whether the QUESTA is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (with-slots (elements tail) questa
    (declare (type (list-of T)  elements))
    (declare (type tail-pointer tail))
    (the boolean
      (not (null (eq elements tail))))))

;;; -------------------------------------------------------

(defun questa-element-list (questa)
  "Returns a list containing the elements of the QUESTA.
   ---
   This operation is not part of the standard Questa interface."
  (declare (type Questa questa))
  (the (list-of T)
    (copy-list
      (rest (slot-value questa 'elements)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interface "Visitor".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Visitor ()
  ()
  (:documentation
    "The ``Visitor'' interface establishes a common cleronomy for
     entities designed as Quests instruction processors."))

;;; -------------------------------------------------------

(defgeneric dispatch-operand (visitor operand-type operand)
  (:documentation
    "Processes the OPERAND identified by the OPERAND-TYPE in the context
     of the VISITOR and returns a value appropriate for this OPERAND."))

;;; -------------------------------------------------------

(defun visit-operand (visitor operand)
  "Processes the OPERAND in the context of the VISITOR and returns a
   value appropriate for this OPERAND."
  (declare (type Visitor visitor))
  (declare (type Operand operand))
  (the T (dispatch-operand visitor (operand-type operand) operand)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Interpreter".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter (Visitor)
  ((instructions
    :initarg       :instructions
    :initform      (error "Missing instruction section.")
    :type          (vector Instruction *)
    :documentation "The instructions to process.")
   (ip
    :initarg       :ip
    :initform      0
    :type          fixnum
    :documentation "The instruction pointer, designating the instruction
                    from the INSTRUCTIONS sequence currently operated
                    upon.")
   (questa
    :initarg       :questa
    :initform      (make-questa)
    :type          Questa
    :documentation "The Questa object to operate upon.")
   (result
    :initarg       :result
    :initform      NIL
    :type          T
    :documentation "Stores the result of the last evaluated instruction
                    in order to present an overall output of the Questa
                    program."))
  (:documentation
    "The ``Interpreter'' class embues an instruction sequence with
     actual effect."))

;;; -------------------------------------------------------

(defun make-interpreter (instructions)
  "Creates and returns a new ``Interpreter'' operating on the
   INSTRUCTIONS."
  (declare (type (vector Instruction *) instructions))
  (the Interpreter
    (make-instance 'Interpreter :instructions instructions)))

;;; -------------------------------------------------------

(defun interpreter-advance-ip (interpreter)
  "Moves the INTERPRETER's instruction pointer to the next instruction,
   if possible, and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (with-slots (ip instructions) interpreter
    (declare (type fixnum                 ip))
    (declare (type (vector Instruction *) instructions))
    (when (< ip (length instructions))
      (incf ip)))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-move-ip-to (interpreter new-ip)
  "Moves the INTERPRETER's instruction poitner to the zero-based
   instruction index NEW-IP and returns the modified INTERPRETER."
  (declare (type Interpreter interpreter))
  (declare (type fixnum      new-ip))
  (with-slots (ip) interpreter
    (declare (type fixnum ip))
    (setf ip new-ip))
  (the Interpreter interpreter))

;;; -------------------------------------------------------

(defun interpreter-process-instruction (interpreter instruction)
  "Processes the INSTRUCTION Using the INTERPRETER and returns a result
   appropriate for the INSTRUCTION type.
   ---
   The modification of the INTERPRETER's instruction pointer depends
   upon the INSTRUCTION being an operand or not, with the former case
   abstaining from such progression, while the latter veridically helms
   the pointer."
  (declare (type Interpreter interpreter))
  (declare (type Instruction instruction))
  
  (flet ((evaluate-operands ()
          "Evaluates the INSTRUCTION's operands and returns these in
           their encountered order as elements of a fresh list."
          (the (list-of T)
            (mapcar
              #'(lambda (operand)
                  (declare (type Operand operand))
                  (visit-operand interpreter operand))
              (instruction-operands instruction)))))
    
    (with-slots (questa) interpreter
      (declare (type Questa questa))
      
      (the T
        (case (instruction-type instruction)
          (:push
            (prog1
              (let ((operands (evaluate-operands)))
                (declare (type (list-of T) operands))
                (validate-instruction :push operands)
                (questa-push questa (first operands)))
              (unless (instruction-operand-p instruction)
                (interpreter-advance-ip interpreter))))
          
          (:pop
            (prog1
              (let ((operands (evaluate-operands)))
                (declare (type (list-of T) operands))
                (validate-instruction :pop operands)
                (let ((end (first operands)))
                  (declare (type bit end))
                  (case end
                    (0
                      (questa-pop1 questa))
                    (1
                      (questa-pop2 questa))
                    (otherwise
                      (error "Invalid argument to pop: p(~s)." end)))))
              (unless (instruction-operand-p instruction)
                (interpreter-advance-ip interpreter))))
          
          (:output
            (prog1
              (let ((operands (evaluate-operands)))
                (declare (type (list-of T) operands))
                (validate-instruction :output operands)
                (let ((end (first operands)))
                  (declare (type bit end))
                  (format T "~a"
                    (case end
                      (0
                        (questa-pop1 questa))
                      (1
                        (questa-pop2 questa))
                      (otherwise
                        (error "Invalid argument to output: p(~s)."
                          end))))))
              (unless (instruction-operand-p instruction)
                (interpreter-advance-ip interpreter))))
          
          (:increment
            (prog1
              (let ((operands (evaluate-operands)))
                (declare (type (list-of T) operands))
                (validate-instruction :increment operands)
                (let ((end (first operands)))
                  (declare (type bit end))
                  (case end
                    (0
                      (questa-set1 questa (1+ (questa-peek1 questa)))
                      (questa-peek1 questa))
                    (1
                      (questa-set2 questa (1+ (questa-peek2 questa)))
                      (questa-peek2 questa))
                    (otherwise
                      (error "Invalid argument to increment: inc(~s)."
                        end)))))
              (unless (instruction-operand-p instruction)
                (interpreter-advance-ip interpreter))))
          
          (:decrement
            (destructuring-bind (x y)
                (validate-instruction :decrement (evaluate-operands))
              (declare (type bit     x))
              (declare (type integer y))
              
              (the (or null integer)
                (cond
                  ;; (x = 0) and (questa.first = 0)?
                  ;; => Goto.
                  ((and (zerop x)
                        (zerop (questa-peek1 questa)))
                    (prog1 NIL
                      (interpreter-move-ip-to interpreter y)))
                  
                  ;; (x = 0) and (questa.first != 0)?
                  ;; => Decrement.
                  ((and (zerop x)
                        (not (zerop (questa-peek1 questa))))
                    (questa-set1 questa (1- (questa-peek1 questa)))
                    (prog1
                      (questa-peek1 questa)
                      (unless (instruction-operand-p instruction)
                        (interpreter-advance-ip interpreter))))
                  
                  ;; (x = 1) and (questa.last = 0)?
                  ;; => Goto.
                  ((and (= x 1)
                        (zerop (questa-peek2 questa)))
                    (prog1 NIL
                      (interpreter-move-ip-to interpreter y)))
                  
                  ;; (x = 1) and (questa.last != 0)?
                  ;; => Decrement.
                  ((and (= x 1)
                        (not (zerop (questa-peek2 questa))))
                    (questa-set2 questa (1- (questa-peek2 questa)))
                    (prog1
                      (questa-peek2 questa)
                      (unless (instruction-operand-p instruction)
                        (interpreter-advance-ip interpreter))))
                  
                  ;; Invalid combination of arguments x and y.
                  (T
                    (error "Invalid combination of command arguments: ~
                            dec(x=~s,y=~s)."
                      x y))))))
          
          (:swap
            (prog1 NIL
              (let ((operands (evaluate-operands)))
                (declare (type (list-of T) operands))
                (validate-instruction :swap operands)
                (questa-swap questa))
              (unless (instruction-operand-p instruction)
                (interpreter-advance-ip interpreter))))
          
          (otherwise
            (error "Invalid instruction: ~s." instruction)))))))

;;; -------------------------------------------------------

(defmethod dispatch-operand ((interpreter  Interpreter)
                             (operand-type (eql :string))
                             (operand      Operand))
  (declare (type Interpreter interpreter))
  (declare (type keyword     operand-type))
  (declare (type Operand     operand))
  (declare (ignore           interpreter))
  (declare (ignore           operand-type))
  (the string (operand-value operand)))

;;; -------------------------------------------------------

(defmethod dispatch-operand ((interpreter  Interpreter)
                             (operand-type (eql :number))
                             (operand      Operand))
  (declare (type Interpreter interpreter))
  (declare (type keyword     operand-type))
  (declare (type Operand     operand))
  (declare (ignore           interpreter))
  (declare (ignore           operand-type))
  (the integer (operand-value operand)))

;;; -------------------------------------------------------

(defmethod dispatch-operand ((interpreter  Interpreter)
                             (operand-type (eql :instruction))
                             (operand      Operand))
  (declare (type Interpreter interpreter))
  (declare (type keyword     operand-type))
  (declare (type Operand     operand))
  (declare (ignore           operand-type))
  (interpreter-process-instruction interpreter
    (operand-value operand)))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the instructions stored by the INTERPRETER and returns the
   last evaluated form."
  (declare (type Interpreter interpreter))
  (with-slots (instructions ip result) interpreter
    (declare (type (vector Instruction *) instructions))
    (declare (type fixnum                 ip))
    (declare (type T                      result))
    (loop while (< ip (length instructions)) do
      (setf result
        (interpreter-process-instruction interpreter
          (aref instructions ip))))
    (the T result)))

;;; -------------------------------------------------------

(defun interpret-Quests (code)
  "Interprets the piece of Quests CODE and returns the last evaluated
   form."
  (declare (type string code))
  (the T
    (interpreter-interpret
      (make-interpreter
        (parser-parse
          (make-parser
            (make-lexer code)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HelloWorld".
(interpret-Quests "p(HelloWorld) >(0)")

;;; -------------------------------------------------------

;; Returns "HelloWorld".
(interpret-Quests "p(HelloWorld) <(0)")

;;; -------------------------------------------------------

;; Returns 101.
(interpret-Quests "p(1) p(200) p(100) p(0) inc(<(0))")

;;; -------------------------------------------------------

;; Returns 201.
(interpret-Quests "p(1) p(200) p(100) p(0) inc(<(1))")

;;; -------------------------------------------------------

;; Prints 0.
;; 
;; Uses the goto facility, incorporated as an ancillary into the "dec"
;; command, to bypass the pushing of "World" unto the Questa, thus, when
;; returning the top element, responding with the integer 0, not with
;; "World", which otherwise would have been inserted at the top.
;; 
;; In order to demonstrate the "dec" command's functioning, two elements
;; are pushed, "Hello" and 0, comprising the following Questa content:
;;   0        <- top
;;   "Hello"  <- bottom
;; The invocation
;;   dec(0,4)
;; checks the top element for equality to zero, and jumps to the last
;; line (index = 4), thus not pushing "World" to the top. The
;; instruction
;;   >(0)
;; hence pops and prints the integer 0, not "World".
(interpret-Quests
  "p(Hello)
   p(0)
   dec(0,4)
   p(World)
   >(0)")

;;; -------------------------------------------------------

;; Prints "Hello".
;; 
;; Pushes two elements unto the Questa, 0 and "Hello", comprising the
;; following Questa content:
;;   "Hello"  <- top
;;   0        <- bottom
;; The invocation
;;   dec(1,4)
;; checks the bottom element for equality to zero, and jumps to the last
;; line (index = 4), thus not pushing "World" to the top. The
;; instruction
;;   <(0)
;; hence pops the string "Hello", not "World".
(interpret-Quests
  "p(0)
   p(Hello)
   dec(1,4)
   p(World)
   >(0)")

;;; -------------------------------------------------------

;; Prints "first".
(interpret-Quests
  "p(first)
   p(second)
   sw()
   >(0)")
