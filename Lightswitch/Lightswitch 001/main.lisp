;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Lightswitch", invented by the Esolang user
;; "Infinitehexagon" and presented on September 22nd, 2023, the
;; kenspeckle proprium of which wones in the simulation of light
;; switches as binary state carriers, each such a bit's conditory,
;; amenable to its modulation into the obverse state, and capacitated
;; to engage in an intercourse as a source or dependent of another
;; switch.
;; 
;; 
;; Concept
;; =======
;; The Lightswitch programming language operates on a series of zero or
;; more light bulbs or switches, explicitly defined by the developer and
;; endowed with the capacity for a scalar bit, the activation of which
;; results in the toggling of the dependent switches, as specified by
;; bespoke rules.
;; 
;; == LIGHT SWITCHES ARE BINARY UNITS ==
;; A light switch accommodates a single bit's castaldy, embracing in its
;; diorism at any instant either the "off" (zero) or "on" (one) value,
;; the former among the range serves to provide the incipial status.
;; 
;; == LIGHT SWITCHES ARE IDENTIFIED FOLLOWING A CERTAIN PATTERN ==
;; The lights' agnomination eludes the liberality acquainted with the
;; usual variable notion, as the identifiers ought to concord with a
;; particular forbisen, the prevenience supplied by the minuscular
;; letter "p", and concluded with an unsigned non-negative integer.
;; Siclike, the enumeration's stringency subscribes to the same mete of
;; regulations, as the first switch must be norned "p0", the next "p1",
;; etc.
;; 
;; == LIGHT SWITCHES COLLABORATE VIA TOGGLING ==
;; The contingency for an arbitary light switch generation induces one
;; moiety of the available competences, its patration issuing from the
;; dation of interoperations among the units.
;; 
;; Every light switch might engage in a vinculum with an arbitrary
;; account of dependencies; if acting as such a rule's source, the
;; switch's activation instigates a toggling of every dependent switch.
;; 
;; 
;; Syntax
;; ======
;; A Lightswitch program's conformation is established upon a sequence
;; of zero or more lines, each non-blank specimen among which
;; contributes a single command's invocation, compact of the identifier
;; and one or more argument, their segregation's realization the dever
;; of spaces.
;; 
;; == GRAMMAR ==
;; A treatise on Lightswitch's donet imbued with superior formality
;; shall be the following Extended Backus-Naur Form's (ENBF)
;; contribution:
;; 
;;   program        := { innerLine } , [ lastLine ] ;
;;   innerLine      := lineContent , newlines ;
;;   lastLine       := lineContent ;
;;   lineContent    := [ spacing ] , [ command ] , [ spacing ] ;
;;   
;;   command        := ruleDefinition
;;                  |  toggle
;;                  |  input
;;                  |  output
;;                  |  getState
;;                  ;
;;   
;;   ruleDefinition := lightSwitch , spacing , toggle ;
;;   toggle         := "toggle" , spacing , lightSwitch ;
;;   
;;   input          := "@in"  , spacing , lightSwitch ;
;;   output         := "@out" , spacing , expression ;
;;   
;;   expression     := lightSwitch | getState ;
;;   getState       := "@p" , spacing , lightSwitch ;
;;   lightSwitch    := "p" , number ;
;;   
;;   number         := digit , { digit } ;
;;   digit          := "0" | "1" | "2" | "3" | "4"
;;                  |  "5" | "6" | "7" | "8" | "9"
;;                  ;
;;   newlines       := newline , { newline } ;
;;   newline        := "\n" ;
;;   spacing        := space , { space } ;
;;   space          := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; Lightswitch's instruction set tallies a quintuple cardinality, the
;; circumference imparted to it spanning the definition of light switch
;; activation rules, the actual light state toggling, input, output, and
;; a light switch state query mechanism, the latter of which induces a
;; twifaced stature as both a statement and an expression.
;; 
;; == OVERVIEW ==
;; A cursory mete of gnarity's dation shall be the following apercu's
;; dever.
;; 
;; Please note that succedaneous segments are demarcated by a catena
;; limned via asterisks ("*"), and intended to be superseded by actual
;; Lightswitch code fragments in the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Effect
;;   ------------------------+-----------------------------------------
;;   source toggle dependent | Defines a switch rule: If the light
;;   ******        ********* | switch {source} is activated, the state
;;                           | of the {dependent} switch is toggled.
;;                           |-----------------------------------------
;;                           | {source} must be a light switch name.
;;                           |-----------------------------------------
;;                           | {dependent} must be a light switch name.
;;   ..................................................................
;;   toggle lightSwitch      | Activates the {lightSwitch}, toggling
;;          ***********      | every dependent switch ligated to the
;;                           | same by a prevenient rule definition.
;;                           |-----------------------------------------
;;                           | {lightSwitch} must be a light switch
;;                           | name.
;;   ..................................................................
;;   @in lightSwitch         | Queries a bit from the standard input
;;       ***********         | and sets the {lightSwitch} state to the
;;                           | received value.
;;                           |-----------------------------------------
;;                           | {lightSwitch} must be a light switch
;;                           | name.
;;   ..................................................................
;;   @out lightSwitch        | Prints the bit stored in the
;;        ***********        | {lightSwitch} to the standard output.
;;                           |-----------------------------------------
;;                           | {lightSwitch} must either be either of
;;                           |   - A light switch name
;;                           |   - A light switch state query command
;;                           |     ("@p ...").
;;   ..................................................................
;;   @p lightSwitch          | Returns the bit value stored in the
;;      ***********          | {lightSwitch}.
;;                           |-----------------------------------------
;;                           | This operation acts in the agency of an
;;                           | expression.
;;                           |-----------------------------------------
;;                           | {lightSwitch} must be a light switch
;;                           | name.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This project has been implemented in the programming language Common
;; Lisp, its dioristic proprium the abstinence of any explicit
;; object-oriented facilities --- the concepts of which are molded into
;; the circumambiency of bespoke classes and their instances in this
;; Lisp dialect --- for the sake of alternative encapsulation and
;; modeling techniques, the variety therein appertains to closures over
;; functions, lists, and vectors.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-01-29
;; 
;; Sources:
;;   [esolang2023Lightswitch]
;;   The Esolang contributors, "Lightswitch", October 25th, 2023
;;   URL: "https://esolangs.org/wiki/Lightswitch"
;;   
;;   [stackoverflow2010q2954642]
;;   The Stack Overflow contributors,
;;     "Methods and properties in scheme: is OOP possible in Scheme?",
;;     June 2nd, 20210
;;   URL: "https://stackoverflow.com/questions/2954642/
;;         methods-and-properties-in-scheme-is-oop-possible-in-scheme"
;;   Notes:
;;     - Demonstrates the deployment of closures as an avenue for the
;;       emulation of classes in the Scheme programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype token ()
  "The ``token'' type defines a token as a compound of a categorizing
   type and a detailing value, manifesting in a cons, the sinistral
   department of which installs the former piece of information, while
   the patration issues from the value in the dextral moeity's
   location."
  '(cons keyword *))

;;; -------------------------------------------------------

(deftype lexer ()
  "The ``lexer'' type defines a lexical analyzer, or lexer, as a niladic
   function which upon its inquisition responds with a token; in
   corollary adhering to the signature:
     lambda () => token"
  '(function () token))

;;; -------------------------------------------------------

(deftype lexer-function ()
  "The ``lexer-function'' type defines a lexical analyzer, or lexer, as
   a not further specified function, admitting an alternative to the
   more communicative ``lexer'' type in such contexts that do not
   homologate sophisticated type specifiers, the ilk of which embraces,
   among others, elements in a ``values'' form and iteration variables
   for declared in the ``loop'' macro."
  'function)

;;; -------------------------------------------------------

(deftype token-stream-request ()
  "The ``token-stream-request'' enumerates the valid species of requests
   amplected by a ``token-stream'''s apprehension."
  '(member :consume :peek))

;;; -------------------------------------------------------

(deftype token-stream ()
  "The ``token-stream'' type defines a token stream as a monadic
   function which, in response to a ``token-stream-request'', returns
   the current or next token from an underlying ``lexer''.
   ---
   The delegated function's signature, as a consectary, conforms to:
     lambda (token-stream-request) => token"
  '(function (token-stream-request) token))

;;; -------------------------------------------------------

(deftype token-stream-function ()
  "The ``token-stream-function'' type defines a token stream as a not
   further specified function, acting in the agency of a succedaneum for
   the more invested ``token-stream'' type in contexts which interdict
   a sophisticated type specifier, siccan encompasses the ``values''
   form, as well as the iteration variable declarations of the ``loop''
   macro."
  'function)

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' type defines an attribute map compatible with a
   node's requisites as a mapping from attribute names to values,
   realized as a property list, or plist, composed of one or two
   entries, each such a duality of a keyword indicator, or key, and its
   arbitrary datum."
  '(cons keyword
     (cons *
       (or null
         (cons keyword
           (cons * null))))))

;;; -------------------------------------------------------

(deftype node (&optional (node-type * node-type-supplied-p))
  "The ``node'' type defines an abstract syntax tree (AST) node as a
   a property list, or plist, composed of two or more elements, the
   first entry's value is imposed an equality to the keyword symbol
   ``:type'', while the remaining portion constitutes an
   ``attribute-map''."
  `(cons (eql :type)
     (cons
       ;; If a NODE-TYPE is supplied, ascertain its ``eql''-equality;
       ;; otherwise impose a general keyword symbol conformance.
       ,(if node-type-supplied-p
          `(eql ,node-type)
          'keyword)
       attribute-map)))

;;; -------------------------------------------------------

(deftype parser-request ()
  "The ``parser-request'' type enumerates the recognized modes of
   behests admitted by a ``parser'' function.
   ---
   The available options bifurcate into a twain of cognates:
     ------------------------------------------------------------------
     Parser request member  | Causatum
     -----------------------+------------------------------------------
     :get-current-token     | Returns the current token without its
                            | ejection from the parser.
     ..................................................................
     :consume-current-token | Returns the current token, while
                            | concomitantly the next one loads from the
                            | underlying lexer and substitutes the
                            | returned instance in the parser.
     ------------------------------------------------------------------"
  '(member :get-current-token :consume-current-token))

;;; -------------------------------------------------------

(deftype parser ()
  "The ``parser'' type defines a parser as a monadic function, the sole
   argument of which admits a ``parser-request'', whence ensues a token
   instance.
   ---
   The function's signature, as a corollary, conforms to:
     lambda (parser-request) => token"
  '(function (parser-request) token))

;;; -------------------------------------------------------

(deftype parser-function ()
  "The ``parser-function'' type defines a parser as a not further
   specified function, congruous with context that prohibit the more
   sophisticated type specifier delineation, the ilk of which amplects,
   for instance, the ``values'' form and the iteration variables
   declared in the ``loop'' macro."
  'function)

;;; -------------------------------------------------------

(deftype unsigned-integer ()
  "The ``unsigned-integer'' type defines a non-negative integer greater
   than or equal to zero, the range of which thus amounts to
   [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype switch-rule ()
  "The ``switch-rule'' type defines a relationship betwixt one light
   switch state's modulation, the source, to that of another switch, the
   sink, where the toggling of the first inverts the second's state,
   realized as a cons, to whom the switch source is imparted in the
   sinistral comparment, while the dextral maintains the sink, both
   assuming ``unsigned-integer'' light switch identifiers."
  '(cons unsigned-integer unsigned-integer))

;;; -------------------------------------------------------

(deftype switch-rule-set ()
  "The ``switch-rule-set'' type defines an ordered sequence of light
   switch toggle rules, realized as an adjustable vector of triggering
   and dependent switches, each such a ``switch-rule'' object."
  '(vector switch-rule *))

;;; -------------------------------------------------------

(deftype switch-state-entry ()
  "The ``switch-state-entry'' type defines an encapsulation of a light
   switch identifier and its concomitant current state, realized as a
   cons whose sinistral moiety maintains the ``unsigned-integer'' light
   number, affiliated with the ``bit''-valued state in the dextral
   section."
  '(cons unsigned-integer bit))

;;; -------------------------------------------------------

(deftype switch-state-set ()
  "The ``switch-state-set'' type defines an affiliation of the
   recognized light switches to their states, realized as an association
   list, or alist, each entry of which represents one light switch as a
   ``switch-state-entry'', that is, a cons of identifier and state, the
   former being an ``unsigned-integer'', the latter a ``bit''."
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
                  (typep element 'switch-state-entry))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype interpreter ()
  "The ``interpreter'' type defines a Lightswitch interpreter in terms
   of a quadruple vector, the first constituent of which provides
   a representation of the parsed Lightswitch program in the form of an
   abstract syntax tree (AST), the subsequent member realized by an
   association list comprehending the binary light switch states, the
   third specifying the toggle rules as a vector of light switch
   source-dependent twains, and the desinent fourth incorporating a
   light switch identifier counter that traces the sanity of the name
   assignments in regard to the start value of \"p0\" and a strictly
   monotonous, incremental allotment of the numeric components."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            ;; The CANDIDATE constitutes a four-element vector, ...
            (typep candidate           '(simple-vector 4))
            ;; ... its first item being the program root node, ...
            (typep (svref candidate 0) '(node :program))
            ;; ... the second a set of light switch states, ...
            (typep (svref candidate 1) 'switch-state-set)
            ;; ... its third the light switch rules, ...
            (typep (svref candidate 2) 'switch-rule-set)
            ;; ... and its fourth an identifier sequence counter.
            (typep (svref candidate 3) 'unsigned-integer))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   that comply to the ELEMENT-TYPE, the same defaults to the generic
   sentinel ``*''."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of node.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-node (type &rest attributes)
  "Creates and returns a fresh node, delineated by its categorizing
   TYPE, and detailed by its ATTRIBUTES."
  (declare (type keyword       type))
  (declare (type attribute-map attributes))
  (the node
    (append (list :type type) attributes)))

;;; -------------------------------------------------------

(defun get-node-type (node)
  "Returns the NODE type."
  (declare (type node node))
  (the keyword (second node)))

;;; -------------------------------------------------------

(defun get-node-attributes (node)
  "Returns the NODE's attribute map."
  (declare (type node node))
  (the attribute-map (cddr node)))

;;; -------------------------------------------------------

(defun get-node-attribute (node attribute-name)
  "Returns the NODE attribute designated by the ATTRIBUTE-NAME, or
   responds with ``NIL'' if none such exists."
  (declare (type node    node))
  (declare (type keyword attribute-name))
  (the T (getf (get-node-attributes node) attribute-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-to-boolean-truth-value (object)
  "Converts the OBJECT to a ``boolean'' truth value, returning for a
   non-``NIL'' input ``T'', otherwise responding with ``NIL''.
   ---
   Expressed in a more formal and compendious diction, this operation
   translates a generalized boolean into the more restricted species of
   ``boolean'' value."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a space or horizontal tab
   character, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-to-boolean-truth-value
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))

;;; -------------------------------------------------------

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-to-boolean-truth-value
      (or (space-character-p candidate)
          (char= candidate #\Newline)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-spaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent spaces and returns the position of the first
   non-space character in the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'space-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun get-word-end-position (source start)
  "Proceeding from the START position into the SOURCE, returns the
   position in the SOURCE immediately succeeding the sector occupied by
   the word which is expected to be anchored at the START location."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word,
   delimited by optional spaces along its both lateralities, and returns
   two values:
     (1) A string representation of the retrieved word.
     (2) The position into the SOURCE immediately succeeding the word's
         occupied section."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((word-end-position (get-word-end-position source start)))
    (declare (type fixnum word-end-position))
    (the (values string fixnum)
      (values
        (subseq source start word-end-position)
        word-end-position))))

;;; -------------------------------------------------------

(defun light-switch-name-p (candidate)
  "Determines whether the CANDIDATE represents a valid light switch
   identifier, composed of a the minuscular letter \"p\" at its inchoate
   location, and succeeded by a series of one or more decimal digits,
   returning on confirmation the numerical segment as an unsigned
   integer datum, otherwise ``NIL''."
  (declare (type string candidate))
  (the (or null unsigned-integer)
    (and
      (plusp (length candidate))
      (char= (char candidate 0) #\p)
      (ignore-errors
        (parse-integer candidate :start 1)))))

;;; -------------------------------------------------------

(defmacro string-case (key-form &rest clauses)
  "Exercies a conditional execution of at most one of the CLAUSES based
   upon the first eligible clause key's ``string='' equivalency to the
   evaluated KEY-FORM, upon a convenable clause's detection, its forms
   are evaluated, and its desinent form's results returned; otherwise
   the ``NIL'' value is delivered.
   ---
   The KEY-FORM must resolve to a string, intended for the matching
   against the CLAUSES' keys.
   ---
   Each member of the CLAUSES must be a list compact of one or more
   elements, the first constituent of which imposes the clause key and
   ought to produce a string to juxtapose the evaluated KEY-FORM with,
   while the remaining elements furnish the clause forms, executed only
   upon the clause key's equivalency with the KEY-FORM.
   ---
   A particular accommodation, one of the CLAUSES, ideally that empight
   at the desinence, might contribute the default case, designated by
   any of the sentinels ``otherwise'' or ``T'', and, if present,
   instigates this fallback clause forms' execution.
   ---
   An exemplary invocation constitutes the following:
     (let ((input (read-line)))
       (declare (type string input))
       (string-case input
         (\"hello\"   :greeting)
         (\"goodbye\" :valediction)
         (otherwise   :unknown-phrase)))"
  (let ((evaluated-key-form (gensym)))
    (declare (type symbol evaluated-key-form))
    `(let ((,evaluated-key-form ,key-form))
       (declare (type string ,evaluated-key-form))
       (cond
         ,@(loop for clause of-type list in clauses collect
             (destructuring-bind (clause-key &rest clause-body) clause
               (declare (type T    clause-key))
               (declare (type list clause-body))
               (if (member clause-key '(otherwise T) :test #'eq)
                 `(T
                    ,@clause-body)
                 `((string= ,clause-key ,evaluated-key-form)
                    ,@clause-body))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token (type value)
  "Creates and returns a fresh token categorized by its TYPE and
   detailed via the VALUE."
  (the token (cons type value)))

;;; -------------------------------------------------------

(defun get-token-type (token)
  "Returns the TOKEN's categorizing type."
  (the keyword (car token)))

;;; -------------------------------------------------------

(defun get-token-value (token)
  "Returns the TOKEN's detailing value."
  (the T (cdr token)))

;;; -------------------------------------------------------

(defun token-type-p (token expected-type)
  "Determines whether the TOKEN conforms to the EXPECTED-TYPE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type token   token))
  (declare (type keyword expected-type))
  (the boolean
    (convert-to-boolean-truth-value
      (eq (get-token-type token) expected-type))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-word (word)
  "Analyzes the WORD and attempts to return a token representation
   thereof; upon its failure signaling an error of an unspecified type."
  (declare (type string word))
  (the token
    (string-case word
      ("@in"
        (make-token :in word))
      ("@out"
        (make-token :out word))
      ("@p"
        (make-token :state word))
      ("toggle"
        (make-token :toggle word))
      (otherwise
        (let ((light-switch-number (light-switch-name-p word)))
          (declare (type (or null unsigned-integer)
                         light-switch-number))
          (if light-switch-number
            (make-token :light-switch light-switch-number)
            (error "The word ~s does not define a valid token."
              word)))))))

;;; -------------------------------------------------------

(defun make-lexer (source)
  "Creates and returns a fresh lexer dedicated to the SOURCE line's
   lexical analysis."
  (declare (type string source))
  (let ((position          0)
        (desinent-position (length source)))
    (declare (type fixnum position))
    (declare (type fixnum desinent-position))
    (the lexer-function
      #'(lambda ()
          (setf position (skip-spaces source position))
          (the token
            (cond
              ((>= position desinent-position)
                (make-token :eof NIL))
              ((char= (char source position) #\Newline)
                (make-token :newline
                  (prog1 #\Newline
                    (incf position))))
              (T
                (let ((current-word ""))
                  (declare (type string current-word))
                  (setf (values current-word position)
                    (read-word source position))
                  (analyze-word current-word)))))))))

;;; -------------------------------------------------------

(defun get-next-token (lexer)
  "Returns the next token from the LEXER.
   ---
   Upon its source's exhaustion, the LEXER responds to any request with
   a fresh end-of-file (EOF) token."
  (declare (type (function () token) lexer))
  (the token (funcall lexer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token stream.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-token-stream (lexer)
  "Creates and returns a fresh token stream dedicated to the LEXER
   tokens' provision in an eath manner.
   ---
   The thus resulting token stream always responds with a token to its
   possible request modes; however, while the phenotype for the choices
   might concur, the epiphenomena diverge:
     ------------------------------------------------------------------
     Request  | Causatum
     ---------+--------------------------------------------------------
     :peek    | Returns the next token from the lexer without its
              | consumption.
     ..................................................................
     :consume | Returns the next token from the lexer and concomitantly
              | consume it.
     ------------------------------------------------------------------"
  (declare (type lexer lexer))
  (let ((current-token (get-next-token lexer)))
    (declare (type token current-token))
    (the token-stream-function
      #'(lambda (request)
          (declare (type token-stream-request request))
          (the token
            (case request
              (:consume
                (prog1 current-token
                  (setf current-token
                    (get-next-token lexer))))
              (:peek
                current-token)
              (otherwise
                (error "Invalid token stream request: ~s."
                  request))))))))

;;; -------------------------------------------------------

(defun request-token (tokens request)
  "Depending upon the REQUEST mode, either peeks without removing or
   consumes the next token from the TOKENS token stream's underlying
   lexer."
  (declare (type token-stream         tokens))
  (declare (type token-stream-request request))
  (the token (funcall tokens request)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-parser (tokens)
  "Creates and returns a new ``parser'' whose dever to assemble an
   abstract syntax tree (AST) node representation of a Lightswitch
   program is realized by the token stream TOKENS' services."
  (declare (type token-stream tokens))
  (the parser-function
    #'(lambda (request)
        (declare (type parser-request request))
        (the token
          (case request
            (:get-current-token
              (request-token tokens :peek))
            (:consume-current-token
              (request-token tokens :consume))
            (otherwise
              (error "Invalid parser request: ~s." request)))))))

;;; -------------------------------------------------------

(defun get-current-token (parser)
  "Returns the PARSER's current token."
  (declare (type parser parser))
  (the token
    (funcall parser :get-current-token)))

;;; -------------------------------------------------------

(defun consume-current-token (parser)
  "Returns the PARSER's current token and substitutes it by loading the
   next one from the underlying token stream."
  (declare (type parser parser))
  (the token
    (funcall parser :consume-current-token)))

;;; -------------------------------------------------------

(defun current-token-matches-p (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type parser  parser))
  (declare (type keyword expected-token-type))
  (the boolean
    (token-type-p (get-current-token parser) expected-token-type)))

;;; -------------------------------------------------------

(defun eat-token (parser expected-token-type)
  "Determines whether the PARSER's current token conforms to the
   EXPECTED-TOKEN-TYPE, on confirmation returning the probed token,
   while concomitantly substituting it by the next one from the
   underlying token stream, otherwise signaling an error of an
   unspecified type."
  (declare (type parser  parser))
  (declare (type keyword expected-token-type))
  (the token
    (if (current-token-matches-p parser expected-token-type)
      (consume-current-token parser)
      (error "Expected a token of the type ~s, but encountered ~s."
        expected-token-type
        (get-current-token parser)))))

;;; -------------------------------------------------------

(defun skip-newlines (parser)
  "Commencing with the PARSER's current token, skips a sequence of zero
   or more accolent newlines, and returns no value."
  (declare (type parser parser))
  (loop while (current-token-matches-p parser :newline) do
    (consume-current-token parser))
  (values))

;;; -------------------------------------------------------

(defun parse-light-switch-name (parser)
  "Parses a light switch identifier in the PARSER's context and returns
   a ``:light-switch'' node representation thereof."
  (declare (type parser parser))
  (the (node :light-switch)
    (make-node :light-switch :number
      (get-token-value
        (eat-token parser :light-switch)))))

;;; -------------------------------------------------------

(defun parse-expression (parser)
  "Parses an expression in the PARSER's context and returns a ``node''
   representation thereof."
  (declare (type parser parser))
  (the node
    (case (get-token-type (get-current-token parser))
      (:light-switch
        (parse-light-switch-name parser))
      (:state
        (consume-current-token parser)
        (make-node :get-state :light-switch
          (parse-light-switch-name parser)))
      (otherwise
        (error "No expression token: ~s."
          (get-current-token parser))))))

;;; -------------------------------------------------------

(defun parse-command (parser)
  "Parses a command in the PARSER's context and returns a ``node''
   representation thereof."
  (declare (type parser parser))
  (the node
    (case (get-token-type (get-current-token parser))
      ;; {lightSwitch} toggle {lightSwitch}
      (:light-switch
        (make-node :define-switch-rule
          :antecedent
            (parse-light-switch-name parser)
          :consequent
            (progn
              (eat-token parser :toggle)
              (parse-light-switch-name parser))))
      
      ;; toggle {lightSwitch}
      (:toggle
        (eat-token parser :toggle)
        (make-node :toggle
          :light-switch (parse-light-switch-name parser)))
      
      ;; @in {lightSwitch}
      (:in
        (consume-current-token parser)
        (make-node :in
          :light-switch (parse-light-switch-name parser)))
      
      ;; @out {lightSwitch}
      ;; @out @p {lightSwitch}
      (:out
        (consume-current-token parser)
        (make-node :out
          :argument (parse-expression parser)))
      
      ;; @p {lightSwitch}
      (:state
        (consume-current-token parser)
        (make-node :get-state
          :light-switch (parse-light-switch-name parser)))
      
      (otherwise
        (error "No command token: ~s."
          (get-current-token parser))))))

;;; -------------------------------------------------------

(defun expect-command-coda (parser)
  "Determines whether the, proceeding from the PARSER's current token,
   no further content except for the line's termination occurs,
   skipping consecutive newlines if such are encountered, and, on
   confirmation, returning no value; otherwise an error of an
   unspecified type is signaled."
  (declare (type parser parser))
  (case (get-token-type (get-current-token parser))
    (:eof
      NIL)
    (:newline
      (skip-newlines parser))
    (otherwise
      (error "Expected end of command, but encountered ~s."
        (get-current-token parser))))
  (values))

;;; -------------------------------------------------------

(defun parse-program (parser)
  "Assembles a Lightswitch program utilizing the token provided to the
   PARSER by its token stream and returns a ``:program'' node
   representation of the parsed program."
  (declare (type parser parser))
  (the (node :program)
    (make-node :program :commands
      (loop
        initially
          ;; Skip optional newlines at the program's incipiency.
          (skip-newlines parser)
        
        until
          (current-token-matches-p parser :eof)
        
        collect
          (prog1
            (parse-command       parser)
            (expect-command-coda parser))
        
        finally
          ;; Skip optional newlines at the program's desinence.
          (skip-newlines parser)
          ;; Ascertain the absence of any other content.
          (eat-token parser :eof)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of array operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-dynamic-array (element-type initial-element)
  "Creates and returns an initially vacant, adjustable vector
   accommodating a variable tally of elements complying to the
   ELEMENT-TYPE, its cells' default value resolving to the
   INITIAL-ELEMENT."
  (declare (type T element-type))
  (declare (type T initial-element))
  (the vector
    (make-array 0
      :element-type    element-type
      :initial-element initial-element
      :adjustable      T
      :fill-pointer    0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic and numeric operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-bit (source)
  "Parses the SOURCE string as a bit, upon success returning its numeric
   value, otherwise signaling an error of an unspecified type."
  (declare (type string source))
  (let ((numeric-value (parse-integer source)))
    (declare (type integer numeric-value))
    (the bit
      (or (and (typep numeric-value 'bit) numeric-value)
          (error "Cannot parse the string ~s as a bit." source)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of switch rule operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-switch-rule-source (rule)
  "Returns the instigating light switch's identifier from the switch
   RULE."
  (declare (type switch-rule rule))
  (the unsigned-integer
    (car rule)))

;;; -------------------------------------------------------

(defun get-switch-rule-dependent (rule)
  "Returns the dependent light switch's identifier from the switch
   RULE."
  (declare (type switch-rule rule))
  (the unsigned-integer
    (cdr rule)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-interpreter (tree)
  "Creates and returns a new ``interpreter'' dedicated to the abstract
   syntax TREE (AST) representation of a Lightswitch program's
   evaluation."
  (declare (type (node :program) tree))
  (the interpreter
    (vector
      tree
      NIL
      (make-dynamic-array 'switch-rule (cons 0 0))
      0)))

;;; -------------------------------------------------------

(defun get-syntax-tree (interpreter)
  "Returns the abstract syntax tree (AST) representation of the
   Lightswitch program stored in the INTERPRETER."
  (declare (type interpreter interpreter))
  (the (node :program)
    (svref interpreter 0)))

;;; -------------------------------------------------------

(defun get-switch-states (interpreter)
  "Returns a mapping of the INTERPRETER's light switch identifiers to
   their current states."
  (declare (type interpreter interpreter))
  (the switch-state-set
    (svref interpreter 1)))

;;; -------------------------------------------------------

(defun get-switch-state-entry (interpreter identifier)
  "Returns the entry for the light switch state amenable to the
   IDENTIFIER in the INTERPRETER's registry, or ``NIL'' upon its
   disrespondency."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (the (or null switch-state-entry)
    (assoc identifier
      (get-switch-states interpreter)
      :test #'=)))

;;; -------------------------------------------------------

(defun verify-switch-state-entry (interpreter identifier)
  "Determines whether the INTERPRETER contains a light switch amenable
   to the IDENTIFIER, upon confirmation returning its identifier-state
   entry, otherwise signaling an error of an unspecified type."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (the switch-state-entry
    (or (get-switch-state-entry interpreter identifier)
        (error "No light switch with the name p~d defined."
          identifier))))

;;; -------------------------------------------------------

(defun ensure-switch-state-entry (interpreter identifier)
  "Determines whether the INTERPRETER contains a light switch amenable
   to the IDENTIFIER, upon confirmation returning its identifier-state
   entry, otherwise producing and registering such an affiliation, ere
   the new produce is returned."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (let ((switch-entry (get-switch-state-entry interpreter identifier)))
    (declare (type (or null switch-state-entry)))
    (unless switch-entry
      (setf switch-entry
        (cons identifier 0))
      (push switch-entry
        (svref interpreter 1)))
    (the switch-state-entry switch-entry)))

;;; -------------------------------------------------------

(defun get-switch-state (interpreter identifier)
  "Returns the state of the light switch amenable to the IDENTIFIER in
   the INTERPRETER, contingently, upon its absence, generating a
   zero-valued default entry for the same, ere responding with this
   state."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (the bit
    (cdr (verify-switch-state-entry interpreter identifier))))

;;; -------------------------------------------------------

(defun set-switch-state (interpreter identifier new-state)
  "Sets the state of the light switch registered with the IDENTIFIER at
   the INTERPRETER to the NEW-STATE and returns no value."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (declare (type bit              new-state))
  (let ((switch-entry
          (ensure-switch-state-entry interpreter identifier)))
    (declare (type switch-state-entry switch-entry))
    (setf (cdr switch-entry) new-state))
  (values))

;;; -------------------------------------------------------

(defun toggle-switch-state (interpreter identifier)
  "Toggles the state of the light switch registered with the IDENTIFIER
   at the INTERPRETER and returns no value."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer identifier))
  (set-switch-state interpreter identifier
    (- 1 (get-switch-state interpreter identifier)))
  (values))

;;; -------------------------------------------------------

(defun get-switch-rules (interpreter)
  "Returns the switch rules maintained by the INTERPRETER."
  (declare (type interpreter interpreter))
  (the (vector (or null switch-rule) *)
    (svref interpreter 2)))

;;; -------------------------------------------------------

(defun contains-switch-rule-p (interpreter rule)
  "Determines whether the INTERPRETER comprehends a light switch toggle
   rule entry equivalent or identical to the specified RULE, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type interpreter interpreter))
  (declare (type switch-rule rule))
  (the boolean
    (not (null
      (find rule
        (get-switch-rules interpreter)
        :test #'equal)))))

;;; -------------------------------------------------------

(defun get-next-valid-switch-number (interpreter)
  "Returns the largest number homologated to be assigned to a new light
   switch in the INTERPRETER."
  (declare (type interpreter interpreter))
  (the unsigned-integer
    (svref interpreter 3)))

;;; -------------------------------------------------------

(defun assign-next-valid-switch-number (interpreter)
  "Increments the largest number homologated to be assigned to a new
   light switch in the INTERPRETER and returns no value."
  (declare (type interpreter interpreter))
  (incf (svref interpreter 3))
  (values))

;;; -------------------------------------------------------

(defun verify-switch-number (interpreter desired-switch-number)
  "Determines whether the DESIRED-SWITCH-NUMBER, intended to define a
   new light switch, complies to the INTERPRETER's enumeration sequence,
   returning on confirmation no value; otherwise signaling an error of
   an unspecified type."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer desired-switch-number))
  (let ((next-valid-switch-number
          (get-next-valid-switch-number interpreter)))
    (declare (type unsigned-integer next-valid-switch-number))
    (cond
      ((< desired-switch-number next-valid-switch-number)
        NIL)
      ((= desired-switch-number next-valid-switch-number)
        (assign-next-valid-switch-number interpreter))
      (T
        (error "Cannot assign the light switch name p~d, as the ~
                next free identifier constitutes p~d."
          desired-switch-number
          next-valid-switch-number))))
  (values))

;;; -------------------------------------------------------

(defun add-switch-rule (interpreter antecedent consequent)
  "Inserts into the INTERPRETER a new light switch toggle rule, if none
   such exists, which triggers the CONSEQUENT switch's state inversion
   upon the ANTECEDENT's modification, and returns no value."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer antecedent))
  (declare (type unsigned-integer consequent))
  (verify-switch-number      interpreter antecedent)
  (ensure-switch-state-entry interpreter antecedent)
  (ensure-switch-state-entry interpreter consequent)
  (let ((new-rule (cons antecedent consequent)))
    (declare (type switch-rule new-rule))
    (unless (contains-switch-rule-p interpreter new-rule)
      (vector-push-extend new-rule
        (get-switch-rules interpreter))))
  (values))

;;; -------------------------------------------------------

(defun get-dependent-switches (interpreter antecedent)
  "Returns an ordered list comprehending the light switches toggled by
   the ANTECEDENT's instigation."
  (declare (type interpreter      interpreter))
  (declare (type unsigned-integer antecedent))
  (the (list-of unsigned-integer)
    (loop
      for rule
        of-type switch-rule
        across  (get-switch-rules interpreter)
      when
        (= (get-switch-rule-source rule) antecedent)
      collect
        (get-switch-rule-dependent rule))))

;;; -------------------------------------------------------

(defgeneric dispatch-node (interpreter node-type node)
  (:documentation
    "Evaluates the NODE, dispatched on its NODE-TYPE, in the
     INTERPRETER's context and returns a value connable for this
     combination."))

;;; -------------------------------------------------------

(defun visit-node (interpreter node)
  "Evaluates the NODE in the INTERPRETER's context and returns a value
   connable for this combination."
  (declare (type interpreter interpreter))
  (declare (type node        node))
  (the T
    (dispatch-node interpreter
      (get-node-type node)
      node)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :program))
                          (node        list))
  (declare (type interpreter     interpreter))
  (declare (type keyword         node-type))
  (declare (ignore               node-type))
  (declare (type (node :program) node))
  (dolist (command (get-node-attribute node :commands))
    (declare (type node command))
    (visit-node interpreter command))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :light-switch))
                          (node        list))
  (declare (type interpreter          interpreter))
  (declare (type keyword              node-type))
  (declare (ignore                    node-type))
  (declare (type (node :light-switch) node))
  (the unsigned-integer
    (get-node-attribute node :number)))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :get-state))
                          (node        list))
  (declare (type interpreter       interpreter))
  (declare (type keyword           node-type))
  (declare (ignore                 node-type))
  (declare (type (node :get-state) node))
  (the unsigned-integer
    (visit-node interpreter
      (get-node-attribute node :light-switch))))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :define-switch-rule))
                          (node        list))
  (declare (type interpreter                interpreter))
  (declare (type keyword                    node-type))
  (declare (ignore                          node-type))
  (declare (type (node :define-switch-rule) node))
  (add-switch-rule interpreter
    (visit-node interpreter
      (get-node-attribute node :antecedent))
    (visit-node interpreter
      (get-node-attribute node :consequent)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :toggle))
                          (node        list))
  (declare (type interpreter    interpreter))
  (declare (type keyword        node-type))
  (declare (ignore              node-type))
  (declare (type (node :toggle) node))
  (let ((dependent-switches
          (get-dependent-switches interpreter
            (visit-node interpreter
              (get-node-attribute node :light-switch)))))
    (declare (type (list-of unsigned-integer) dependent-switches))
    (dolist (dependent-switch dependent-switches)
      (declare (type unsigned-integer dependent-switch))
      (toggle-switch-state interpreter dependent-switch)))
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :in))
                          (node        list))
  (declare (type interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignore           node-type))
  (declare (type (node :in)  node))
  (format T "~&>> ")
  (finish-output)
  (set-switch-state interpreter
    (visit-node interpreter
      (get-node-attribute node :light-switch))
    (parse-bit
      (read-line)))
  (clear-input)
  (values))

;;; -------------------------------------------------------

(defmethod dispatch-node ((interpreter vector)
                          (node-type   (eql :out))
                          (node        list))
  (declare (type interpreter interpreter))
  (declare (type keyword     node-type))
  (declare (ignorable        node-type))
  (declare (type (node :out) node))
  (let ((argument (get-node-attribute node :argument)))
    (declare (type node argument))
    (format T "~d"
      (case (get-node-type argument)
        (:light-switch
          (get-switch-state interpreter
            (visit-node interpreter argument)))
        (:get-state
          (get-switch-state interpreter
            (visit-node interpreter
              (get-node-attribute argument :light-switch))))
        (otherwise
          (error "Invalid argument to \"@out\": ~s." argument)))))
  (values))

;;; -------------------------------------------------------

(defun interpret-Lightswitch (code)
  "Interprets the piece of Lightswitch source CODE and returns no
   value."
  (declare (type string code))
  (let ((interpreter
          (make-interpreter
            (parse-program
              (make-parser
                (make-token-stream
                  (make-lexer code)))))))
    (declare (type Interpreter interpreter))
    (visit-node interpreter
      (get-syntax-tree interpreter)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program which queries and prints one bit.
(interpret-Lightswitch
  "p0 toggle p0
   toggle p0
   @out @p p0")

;;; -------------------------------------------------------

;; One time cat program which prints the input twice.
(interpret-Lightswitch
  "p0 toggle p0
   @in p0
   @out p0
   @out @p p0")

;;; -------------------------------------------------------

;; One-time cat program.
;; 
;; Queries the user for four bits and prints the same in their given
;; order to the standard output.
(interpret-Lightswitch
  "
  p0 toggle p0
  @in p0
  p1 toggle p1
  @in p1
  p2 toggle p2
  @in p2
  p3 toggle p3
  @in p3
  @out p0
  @out p1
  @out p2
  @out p3
  ")

;;; -------------------------------------------------------

;; Join two states.
;; 
;; The user is queried for two bits, the same are concatenated in the
;; order of specification, and printed to the standard output.
(interpret-Lightswitch
  "p0 toggle p0 
   p1 toggle p1
   @in p0 
   @in p1 
   @out @p p0
   @out @p p1")

;;; -------------------------------------------------------

;; NOT logic gate.
;; 
;; This program queries the user for a bit, negates the same, and
;; outputs this new state.
(interpret-Lightswitch
  "p0 toggle p0
   @in p0
   toggle p0
   @out @p p0")
