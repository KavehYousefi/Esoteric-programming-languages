;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tueue", invented by the Esolang user "ChuckEsoteric08" and
;; presented on April 20th, 2023, its kenspeckle proprium the
;; application of a queue twain and an accumulator in order to perquire
;; and manipulate signed integers or strings.
;; 
;; 
;; Concept
;; =======
;; The Tueue programming language is founded upon the coefficiency of
;; two queues and one accumulator, both lending their homologation to
;; signed integer numbers and strings of one or more characters' length.
;; 
;; 
;; Syntax
;; ======
;; From a syntactical application of one's conspectuity, a Tueue program
;; enumerates a catena of zero or more lines, everichon's accommodation
;; airted at the provision of at most one instruction, these
;; compositions combining assembly-like operation identifiers with
;; zero or more operands.
;; 
;; == THE PROGRAM: A SEQUENCE OF LINES ==
;; A Tueue program's syntactical structure derives from a sequence of
;; zero or more lines, everichon among these either blank or a
;; commorancy's accommodation regarding an aefauld instruction.
;; 
;; == INSTRUCTIONS: OPERATORS FOLLOWED BY OPERANDS ==
;; An instruction's diorism embraces a parasceuastic operation name,
;; succeeded by zero or more operands, all constituents being segregated
;; by one or more spaces.
;; 
;; == OPERANDS: INTEGERS, STRINGS, AND MEMORY LOCATIONS ==
;; The operand department intrines the contingencies for signed and
;; unsigned integer literals, strings of one or more characters'
;; dispansion, and memory locations for referencing the queues and the
;; accumulator.
;; 
;; == INTEGERS: SIGNED DECIMAL LITERALS ==
;; Signed and unsigned integer literals, their manifestation chosen as
;; decimal digit catenas, signifies the basic operand entity.
;; 
;; == STRINGS: CHARACTER SEQUENCES DESTITUTE OF DEMARCATIONS ==
;; The sufficiency of literal character sequences vanquishes a
;; stipulation concerning the string type's demarcating warklumes, the
;; ilk of which in many programming languages bears the incarnation of
;; double quotation marks ('"'). A non-empty arrangement of symbols,
;; expressed in a variety that deviates from the exclusive statement of
;; decimal digits, already serves in a string's specification.
;; 
;; == MEMORY LOCATIONS: KEYWORDS WHICH ADDRESS QUEUES AND ACCUMULATOR ==
;; Equinumerant in its exposition, a triplet of sentinels provides
;; access to the Tueue memory components, scilicet, the queues twain and
;; the accumulator.
;; 
;; == WHITESPACES ==
;; The participation of whitespaces, in its diorism limned by spaces,
;; horizontal tabs, and newline entities, bifurcates into such of
;; requisite adhibition, and the athwart encheson nuncupated to the
;; allowances of one's deliberation.
;; 
;; Spaces and horizontal tabs as sepiments impose a requisitum betwixt
;; the language tokens; counterdistinguished in this agency from a
;; tolerance meted in an equipollent degree to their insignificance at
;; any further location on a line.
;; 
;; Linebreak characters, siclike to the token segregation function of
;; spaces, apply themselves to the discrimination betwixt code lines,
;; their presence necessitated in the instructions' distribution, and
;; as such subordinated to stringency in their ceremonials. Blank lines,
;; being of no epiphenomenal value, enjoy a homologation across a
;; program's entirety.
;; 
;; == COMMENTS ==
;; The Tueue programming language offers a dedicated instruction to
;; the commentary causes, its introduction ensuing from the "COM"
;; keyword, whence any content until the ensconcing line's desinence
;; enjoys its homologation.
;; 
;; == GRAMMAR ==
;; The Tueue language's donet shall be the recipient of an augmented ilk
;; of formality's adhibition expressed in the guise of the Extended
;; Backus-Naur Form (ENBF) standard:
;; 
;;   program         := { innerLine } , [ lastLine ] ;
;;   innerLine       := [ command ] , newlines ;
;;   lastLine        := [ command ] ;
;;   
;;   command         := swpCommand
;;                   |  deqCommand
;;                   |  enqCommand
;;                   |  decCommand
;;                   |  incCommand
;;                   |  divCommand
;;                   |  mulCommand
;;                   |  addCommand
;;                   |  subCommand
;;                   |  movCommand
;;                   |  labCommand
;;                   |  jmpCommand
;;                   |  iqeCommand
;;                   |  iqjCommand
;;                   |  iajCommand
;;                   |  ascCommand
;;                   |  outCommand
;;                   |  comCommand
;;                   ;
;;   
;;   swpCommand      := "SWP" ;
;;   
;;   deqCommand      := "DEQ" ;
;;   enqCommand      := "ENQ" , expression ;
;;   
;;   decCommand      := "DEC" , expression ;
;;   incCommand      := "INC" , expression ;
;;   divCommand      := "DIV" , expression ;
;;   mulCommand      := "MUL" , expression ;
;;   
;;   addCommand      := "ADD" ;
;;   subCommand      := "SUB" ;
;;   
;;   movCommand      := "MOV" , expression , memoryReference ;
;;   
;;   labCommand      := "LAB" , labelName ;
;;   jmpCommand      := "JMP" , labelName ;
;;   iqeCommand      := "IQE" , labelName ;
;;   iqjCommand      := "IQJ" , expression , labelName ;
;;   iajCommand      := "IAJ" , expression , labelName ;
;;   
;;   ascCommand      := "ASC" , expression ;
;;   outCommand      := "OUT" , expression ;
;;   
;;   comCommand      := "COM" , { wordCharacter } ;
;;   
;;   expression      := stringLiteral
;;                   |  integerLiteral
;;                   |  memoryReference
;;                   ;
;;   
;;   memoryReference := "1QU" | "2QU" | "ACC" ;
;;   
;;   stringLiteral   := word ;
;;   
;;   word            := wordCharacter , { wordCharacter } ;
;;   wordCharacter   := character - whitespace ;
;;   
;;   integerLiteral  := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   
;;   whitespace      := space | newline ;
;;   newlines        := newline , { newline } ;
;;   newline         := "\n" ;
;;   space           := " " | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The Tueue programming language specifies in its instruction set a
;; cardinality of 18 members, this multifarious potential entreparted
;; betwixt siccan devoted to arithmetics, the manipulation of the
;; accumulator and the queues, supplemented by output facilities, and
;; several conditional and as well as one unconditional control flow
;; governance construct.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be measured in the adhibition of
;; a cursory ilk of nortelry concerning the language's operative
;; competences.
;; 
;; Please heed the underlining of succedaneous by a catena of asterisk
;; symbols ("*"), intended for their supersession in the ultimate Tueue
;; program by valid code.
;; 
;;   ------------------------------------------------------------------
;;   Command          | Effect
;;   -----------------+------------------------------------------------
;;   ENQ newElement   | Inserts the {newElement} at the current
;;       **********   | queue's rear.
;;                    |------------------------------------------------
;;                    | {newElement} must be an expression.
;;   ..................................................................
;;   DEQ              | Removes the element at the current queue's
;;                    | front and discards the same.
;;                    |------------------------------------------------
;;                    | If the current queue is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyQueueError" is signaled.
;;   ..................................................................
;;   SWP              | Changes the current queue to the hitherto
;;                    | inactive one.
;;   ==================================================================
;;   ADD              | Dequeues the current queue's first element,
;;                    | "a", following by a removal of the new front,
;;                    | "b", supputates the sum c = b + a, and enqueues
;;                    | thilk in the current queue.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction:
;;                    |   let a <- currentQueue.dequeue()
;;                    |   let b <- currentQueue.dequeue()
;;                    |   let c <- b + a
;;                    |   currentQueue.enqueue(c)
;;                    |------------------------------------------------
;;                    | If the current queue cannot furnish at least
;;                    | two elements, an error of the type
;;                    | "EmptyQueueError" is signaled.
;;   ..................................................................
;;   SUB              | Dequeues the current queue's first element,
;;                    | "a", following by a removal of the new front,
;;                    | "b", supputates the difference c = b - a, and
;;                    | enqueues thilk in the current queue.
;;                    |------------------------------------------------
;;                    | In a pseudocode diction:
;;                    |   let a <- currentQueue.dequeue()
;;                    |   let b <- currentQueue.dequeue()
;;                    |   let c <- b - a
;;                    |   currentQueue.enqueue(c)
;;                    |------------------------------------------------
;;                    | If the current queue cannot furnish at least
;;                    | two elements, an error of the type
;;                    | "EmptyQueueError" is signaled.
;;   ==================================================================
;;   INC amount       | Increments the accumulator's value by the
;;       ******       | {amount}.
;;                    |------------------------------------------------
;;                    | {amount} must be an expression.
;;   ..................................................................
;;   DEC amount       | Decrements the accumulator's value by the
;;       ******       | {amount}.
;;                    |------------------------------------------------
;;                    | {amount} must be an expression.
;;   ..................................................................
;;   MUL multiplier   | Multiplies the accumulator's value by the
;;       **********   | {multiplier}.
;;   ..................................................................
;;   DIV divisor      | Divides the accumulator's value by the
;;       *******      | {divisor} and rounds the quotient to the
;;                    | nearest integer.
;;   ==================================================================
;;   MOV source dest  | Copies the {source}'s value to the destination
;;       ****** ****  | {dest}.
;;                    |------------------------------------------------
;;                    | {source} must be an expression.
;;                    |------------------------------------------------
;;                    | {dest} must refer to a memory component,
;;                    | scilicet, either of:
;;                    |   ---------------------------------------------
;;                    |   Dest. | Causatum
;;                    |   ------+--------------------------------------
;;                    |   1QU   | Replaces the element at the first
;;                    |         | queue's front by the {source}.
;;                    |   .............................................
;;                    |   2QU   | Replaces the element at the second
;;                    |         | queue's front by the {source}.
;;                    |   .............................................
;;                    |   ACC   | Stores the {source} value in the
;;                    |         | accumulator.
;;                    |   ---------------------------------------------
;;   ==================================================================
;;   ASC argument     | Prints the {argument} as an ASCII character or
;;       ********     | string of ASCII characters to the standard
;;                    | output.
;;                    |------------------------------------------------
;;                    | {argument} must be an expression.
;;                    |------------------------------------------------
;;                    | The following stipulations hold for the
;;                    | {argument}'s content:
;;                    |   (1) If the {argument} resolves to an integral
;;                    |       number, the character whose ASCII code
;;                    |       concurs with this value is printed.
;;                    |   (2) If the {argument} resolves to a string,
;;                    |       its characters are printed verbatim.
;;   ..................................................................
;;   OUT argument     | Prints the {argument} as a numeric object to
;;       ********     | the standard output.
;;                    |------------------------------------------------
;;                    | {argument} must be an expression.
;;                    |------------------------------------------------
;;                    | The following stipulations hold for the
;;                    | {argument}'s content:
;;                    |   (1) If the {argument} resolves to an integral
;;                    |       number, this value is printed verbatim.
;;                    |   (2) If the {argument} resolves to a string,
;;                    |       its characters' ASCII codes are printed
;;                    |       in an order replicating their occurrency,
;;                    |       each twissel being segregated by the
;;                    |       standard numeric separator.
;;   ==================================================================
;;   LAB labelName    | Defines a new label which associates the
;;       *********    | {labelName} with the line number of its
;;                    | definition for latter referral.
;;                    |------------------------------------------------
;;                    | {labelName} must be a valid label identifier.
;;                    |------------------------------------------------
;;                    | If a label with the same {labelName} has
;;                    | already been defined in the program, an error
;;                    | of the type "DuplicateLabelError" will be
;;                    | signaled.
;;   ..................................................................
;;   JMP target       | Relocates the instruction pointer (IP) to the
;;       ******       | line comprehending the label definition for the
;;                    | {target} label identifier.
;;                    |------------------------------------------------
;;                    | {target} must be a valid label identifier of an
;;                    | extant label.
;;                    |------------------------------------------------
;;                    | If no label amenable to the {target} name can
;;                    | be detected, an error of the type
;;                    | "UnknownLabelError" will be signaled.
;;   ..................................................................
;;   IAJ guard target | If the accumulator value equals the {guard},
;;       ***** ****** | relocates the instruction pointer (IP) to the
;;                    | line comprehending the label definition for the
;;                    | {target} label identifier.
;;                    |------------------------------------------------
;;                    | If no label amenable to the {target} name can
;;                    | be detected, an error of the type
;;                    | "UnknownLabelError" will be signaled.
;;   ..................................................................
;;   IQJ guard target | If the current queue's front element equals the
;;       ***** ****** | {guard}, relocates the instruction pointer (IP)
;;                    | to the line comprehending the label definition
;;                    | for the {target} label identifier.
;;                    |------------------------------------------------
;;                    | If the current queue is empty at the instant of
;;                    | this operation's invocation, an error of the
;;                    | type "EmptyQueueError" is signaled.
;;                    |------------------------------------------------
;;                    | If no label amenable to the {target} name can
;;                    | be detected, an error of the type
;;                    | "UnknownLabelError" will be signaled.
;;   ..................................................................
;;   IQE target       | If the current queue is empty, relocates the
;;       ******       | instruction pointer (IP) to the line
;;                    | comprehending the label definition for the
;;                    | {target} label identifier.
;;                    |------------------------------------------------
;;                    | If no label amenable to the {target} name can
;;                    | be detected, an error of the type
;;                    | "UnknownLabelError" will be signaled.
;;   ==================================================================
;;   COM text         | Defines a comment composed of the {text},
;;       ****         | extending to the current line's desinence.
;;   ------------------------------------------------------------------
;; 
;; == OPERATIONS SPECIALIZED ON THE ACCUMULATOR ==
;; This tabulation shall administer a purlicue's worth concerning those
;; instructions accommodated exclusively to the accumulator:
;; 
;;   ------------------------------------------------------------------
;;   Accumulator | Causatum
;;   operation   | 
;;   ------------+-----------------------------------------------------
;;   DEC x       | Decrement accumulator by {x}.
;;   ..................................................................
;;   DIV x       | Divide accumulator by {x}.
;;   ..................................................................
;;   INC x       | Increment accumulator by {x}.
;;   ..................................................................
;;   MUL x       | Multiply accumulator by {x}.
;;   ==================================================================
;;   IAJ x y     | Jump to {y} if accumulator equals {x}.
;;   ------------------------------------------------------------------
;; 
;; == OPERATIONS SPECIALIZED ON THE QUEUES ==
;; The following set of operations subsumes into the devotion to the
;; current queue or the collaboration among the available queue twain:
;; 
;;   ------------------------------------------------------------------
;;   Queue       | Causatum
;;   operation   | 
;;   ------------+-----------------------------------------------------
;;   ADD         | Add current queue bottom elements.
;;   ..................................................................
;;   SUB         | Subtract current queue bottom elements.
;;   ==================================================================
;;   ENQ x       | Enqueue {x} in current queue.
;;   ..................................................................
;;   DEQ         | Dequeue from current queue.
;;   ..................................................................
;;   SWP         | Swap current queue.
;;   ==================================================================
;;   IQE x y     | Jump to {y} if current queue is empty.
;;   ..................................................................
;;   IQJ x y     | Jump to {y} if current queue bottom equals {x}.
;;   ------------------------------------------------------------------
;; 
;; == GENERAL OR INDEPENDENT OPERATIONS ==
;; The following subset of orra instructions does either homologate
;; both the accumulator, a queue, or any expression to participate, or
;; remains disencumbered from neither datum:
;; 
;;   ------------------------------------------------------------------
;;   General     | Causatum
;;   operation   | 
;;   ------------+-----------------------------------------------------
;;   MOV x y     | Copies {y} to the accumulator or queue {x}.
;;   ==================================================================
;;   OUT x       | Print {x} as an integer.
;;   ..................................................................
;;   ASC x       | Print {y} as an ASCII character.
;;   ==================================================================
;;   LAB x       | Declares the label {x}.
;;   ..................................................................
;;   JMP x       | Jumps to the label {x}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's reification has been realized in the programming
;; language Common Lisp, the entirety of this endeavours' conformation
;; a catena of several tiers:
;; 
;;   (1) EXTRACTION OF INSTRUCTIONS PER LINE:
;;       The Tueue source code induced into the system experiences a
;;       segregation into its lines, these becoming recipients of a
;;       seriatim analyzation stage, producing through this investment
;;       at most one instruction per line, each such embodied in a
;;       dedicated object, and collated into a composite program.
;;   
;;   (2) REGISTRATION OF LABELS:
;;       Ensuing from the instructions' gathering, those entrusted with
;;       the dever of a label's definition are registered via their
;;       zero-based position into the program's underlying instruction
;;       vector in a label table for the contingency of future
;;       inquisition and navigation.
;;   
;;   (3) EXECUTION OF INSTRUCTIONS:
;;       Ultimately, the program, as an ordered catena of instructions,
;;       is processed by an interpreter entity, accompassing actual
;;       efficacy to the operator and its arguments.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-12-06
;; 
;; Sources:
;;   [esolang2023Tueue]
;;   The Esolang contributors, "Tueue", April 28th, 2023
;;   URL: "https://esolangs.org/wiki/Tueue"
;;   
;;   [goodrich2006datastructure4th]
;;   Michael T. Goodrich, Roberto Tamassia
;;     "Data Structures & Algorithms in Java", Fourth Edition, 2006
;;   Notes:
;;     - The pages 115--119 describe the singly linked list.
;;       o The page 116 presents a partial implementation in the Java
;;         programming language.
;;     - The pages 204--212 describe the queue abstract data type (ADT).
;;       o The pages 205--206 present the "Queue" interface.
;;       o The pages 206--209 furnish a fixed-capacity array-based queue
;;         implementation in the Java programming language.
;;       o The page 210 provides a fragmentary implementation of the
;;         queue via a generic singly linked list.
;;   
;;   [goodrich2014datastructure6th]
;;   Michael T. Goodrich, Roberto Tamassia, Michael H. Goldwasser,
;;     "Data Structures & Algorithms in Java", Sixth Edition, 2014
;;   Notes:
;;     - The pages 122--127 describe the concept and an
;;       implementation of the singly linked list in the Java
;;       programming language.
;;       o This establishes the most pertinent subject for our project.
;;       o The pages 126--127 furnish an implementation.
;;     - The pages 238--247 describe the queue abstract data type (ADT).
;;       o The pages 241-244 produce an array-based implementation.
;;       o The page 245 demonstrates an implementation via a singly
;;         linked list.
;;     - The pages 276--280 describe the concept and an
;;       implementation of the doubly linked list in the Java
;;       programming language, significant for the introduction and
;;       deployment of the positional list principles.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-bespoke-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type the agnomination of which provided by its
   TYPE-NAME, its formal parameters being desumed from the LAMBDA-LIST,
   while applying to the subject of its perquisition the CANDIDATE-NAME,
   evaluates the BODY forms, construing the desinent form's primary
   value as a \"generalized boolean\" assessment of the docimasy
   subject's covenableness, with a non-``NIL'' output representing an
   affirmative response, the ``NIL'' value furnishing a signification
   of its refutation."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or mor elements,
   each member of which complies with the ELEMENT-TYPE, thilk defaults
   to the comprehensive ``T''."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-bespoke-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a tuple whose diorism ensues from the
   ELEMENT-TYPES, the tuple size deriving from the ELEMENT-TYPE's tally,
   whereas each tuple element at the i-th index complies with the
   ELEMENT-TYPES item at the same position i."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length element-types))
    (every #'typep
      (the list candidate)
      element-types)))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable Tueue program as a
   one-dimensional simple array comprehending zero or more
   ``Instruction'' objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(define-bespoke-type hash-table-of (candidate
                                    &optional (key-type   T)
                                              (value-type T))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates zero or more entries, each member among which producing
   a twissel of a key compliant with the KEY-TYPE and its value whose
   compatibility appertains to the VALUE-TYPE, for both governs the
   default of the comprehensive ``T'' species."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines a mapping betixt label names and
   their zero-based line numbers inside of a Tueue program as a hash
   table, the keys of which serve in the modeling of the label
   identifier strings, and respond with fixnums as line specifiers."
  '(hash-table-of string fixnum))

;;; -------------------------------------------------------

(deftype selected-queue-id ()
  "The ``selected-queue-id'' type enumerates the valid identifications
   for a Tueue program's contemporaneously activated queue, which per
   the standard's diorism ostends a twissel componency."
  '(member :first :second))

;;; -------------------------------------------------------

(deftype tueue-object ()
  "The ``tueue-object'' type defines an object admissible as a datum
   participating in a Tueue program."
  '(or integer string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which amplects, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\"
   value and produces a veridical Boolean tantamount thereof, returning
   for a non-``NIL'' input the ``T'' value, otherwise, for a ``NIL''
   OBJECT, responds with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun space-character-p (candidate)
  "Determines whether the CANDIDATE represents a spacing character,
   into whose diorism are subsumed the actual space and the horizontal
   tab, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun empty-string-p (source)
  "Determines whether the SOURCE represents an empty string or
   \"null string\", thilk exhibits a character tally of exactly zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (zerop (length source)))))

;;; -------------------------------------------------------

(defun get-character-at (source position)
  "Returns the character at the POSITION into the SOURCE, or, if thilk
   transcends the SOURCE's admissible bournes, responds with the
   \"null\" character."
  (declare (type string source))
  (declare (type fixnum position))
  (the character
    (if (array-in-bounds-p source position)
      (char source position)
      #\Null)))

;;; -------------------------------------------------------

(defun default-clause-key-p (clause-key)
  "Determines whether the CANDIDATE represents a sentinel employed in
   the communication of a default case, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T clause-key))
  (the boolean
    (get-boolean-value-of
      (and (symbolp clause-key)
           (or (eq clause-key 'T)
               (eq clause-key 'otherwise))))))

;;; -------------------------------------------------------

(defmacro string-case (keyform &rest clauses)
  "Evaluates the KEYFORM to a string, the so called \"test key\",
   probes everichon among the CLAUSES, these being lists of one or more
   elements, the incipient of which produces the clause key, via a
   case-insensitive equiparation of the clause key with the test key,
   selecting the first eligible clause, executing its body forms and
   returning its desinent form's results; or, upon a failure to locate
   even a default case, responds with the ``NIL'' value.
   ---
   Any member of the CLAUSES ought to represent a list of zero or more
   elements' componency, where the first item represents the clause key
   to juxtapose with the evaluated KEYFORM, while the subsequent clause
   constituents define the forms to execute, the desinent as an
   epiphenomenon also imposed the dever of producing the results. For
   a clause definition it holds:
     (clause-key clause-form-1 ... clause-form-N)
   where the CLAUSE-KEY must resolve to a string.
   ---
   A default case may be specified by statement of its clause key as the
   symbol ``T'' or, alternatively, ``otherwise'', natheless, ligated to
   the same stipulations as the specific clauses; that is, it holds:
     (T         default-clause-form-1 ... default-clause-form-N)
   or
     (otherwise default-clause-form-1 ... default-clause-form-N)"
  (let ((test-key (gensym)))
    (declare (type symbol test-key))
    `(let ((,test-key ,keyform))
       (declare (type string ,test-key))
       (cond
         ,@(loop for clause of-type list in clauses collect
             (destructuring-bind (clause-key &rest clause-forms)
                 clause
               (declare (type T    clause-key))
               (declare (type list clause-forms))
               (if (default-clause-key-p clause-key)
                 `(T
                    ,@clause-forms)
                 `((string= ,clause-key ,test-key)
                    ,@clause-forms))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of comparison operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric are-equal-p (first-object second-object)
  (:documentation
    "Determines whether the FIRST-OBJECT and the SECOND-OBJECT may be
     considered as tantamounts, returning on confirmation a ``boolean''
     value of ``T'', otherwise ``NIL''.")
  
  (:method ((first-object integer) (second-object integer))
    (declare (type integer first-object))
    (declare (type integer second-object))
    (the boolean
      (get-boolean-value-of
        (= first-object second-object))))
  
  (:method ((first-object integer) (second-object string))
    "Determines whether the FIRST-OBJECT, this being an integer number,
     and the SECOND-OBJECT, a string, may be considered as equals, which
     may be stipulated by a lexicographic conformity of the former's
     string representation with the latter's verbatim design, returning
     on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
    (declare (type integer first-object))
    (declare (type string  second-object))
    (the boolean
      (get-boolean-value-of
        (string=
          (write-to-string first-object)
          second-object))))
  
  (:method ((first-object string) (second-object string))
    (declare (type string first-object))
    (declare (type string second-object))
    (the boolean
      (get-boolean-value-of
        (string= first-object second-object))))
  
  (:method ((first-object string) (second-object integer))
    (declare (type string  first-object))
    (declare (type integer second-object))
    (the boolean
      (are-equal-p second-object first-object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition type generation operations.      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-symbol-into-keyword (symbol)
  "Generates, interns, and returns a keyword symbol as a tantamount to
   the SYMBOL's name."
  (declare (type symbol symbol))
  (the keyword
    (nth-value 0
      (intern
        (format NIL "~:@(~a~)"
          (symbol-name symbol))
        'keyword))))

;;; -------------------------------------------------------

(defun assemble-condition-slot-reader-name (condition-type-name
                                            slot-name)
  "Returns the reader function name for the SLOT-NAME, adhering to the
   norm of the CONDITION-TYPE-NAME's prefixion."
  (declare (type symbol condition-type-name))
  (declare (type symbol slot-name))
  (the symbol
    (intern
      (format NIL "~:@(~a-~a~)" condition-type-name slot-name))))

;;; -------------------------------------------------------

(defun assemble-condition-slot (condition-type-name slot-specification)
  "Generates and returns for the condition type's SLOT-SPECIFICATION,
   this being a triple of symbolic name, slot type specifier, and
   documentation string, with the condition's agnomination resolving to
   the CONDITION-TYPE-NAME, a ``define-condition''-compatible slot
   definition."
  (declare (type symbol                     condition-type-name))
  (declare (type (tuple-of symbol T string) slot-specification))
  (destructuring-bind (slot-name slot-type slot-documentation)
      slot-specification
    (declare (type symbol slot-name))
    (declare (type T      slot-type))
    (declare (type string slot-documentation))
    `(,slot-name
       :initarg       ,(convert-symbol-into-keyword slot-name)
       :initform      (error "Missing value for slot ~s." ',slot-name)
       :reader        ,(assemble-condition-slot-reader-name
                         condition-type-name
                         slot-name)
       :type          ,slot-type
       :documentation ,slot-documentation)))

;;; -------------------------------------------------------

(defmacro define-bespoke-condition
    (condition-type-name
     super-classes
     documentation-string
     (&rest slot-specifications)
     ((report-condition-name report-stream-name) &body report-body))
  "Defines a new condition type, its agnomination desumed from the
   CONDITION-TYPE-NAME, its cleronomy accommodated via the list of
   SUPER-CLASSES, and ordaining the DOCUMENTATION-STRING to the
   latreutical agency of its purpose elucidation, the condition slots
   being specified by adminiculum of the SLOT-SPECIFICATIONS, everichon
   among these a triple of symbolic name, slot type, and a slot
   documentation string, finally deriving its reporting warklume from
   an implicitly generated lambda function, this condition type as the
   premier formal parameter being nevened by the REPORT-CONDITION-NAME,
   the destination by the REPORT-STREAM-NAME, and the actual reporting
   causata yielded via the REPORT-BODY."
  `(define-condition ,condition-type-name ,super-classes
     (,@(loop
          for slot-specification
            of-type (tuple-of symbol T string)
            in      slot-specifications
          collect
            (assemble-condition-slot
              condition-type-name
              slot-specification)))
     
     (:report
       (lambda (,report-condition-name ,report-stream-name)
         (declare (type ,condition-type-name ,report-condition-name))
         (declare (ignorable                 ,report-condition-name))
         (declare (type destination          ,report-stream-name))
         (declare (ignorable                 ,report-stream-name))
         ,@report-body))
     
     (:documentation ,documentation-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-bespoke-condition Tueue-Error (error)
  "The ``Tueue-Error'' condition type serves as the firmament to all
   condition species nuncupated to the communication of anomalous
   situations during a Tueue program's reception, lexical analyzation,
   parsing, or execution."
  ()
  ((condition stream)
    (format stream "A general Tueue-Error has been instigated.")))

;;; -------------------------------------------------------

(define-bespoke-condition Label-Error (Tueue-Error)
  "The ``Label-Error'' condition type establishes a common basis for all
   condition types pursuing the apprizal about anomalies in conjunction
   with labels."
  ((name string "The offending name"))
  ((condition stream)
    (format stream "An amolous situation concerning the label ~a arose."
      (label-error-name condition))))

;;; -------------------------------------------------------

(define-bespoke-condition Duplicate-Label-Error (Label-Error)
  "The ``Duplicate-Label-Error'' condition type applies itself to the
   communication of an anomalous situation whose etiology emerges from
   the attempt to define a label by a name already accommodated to this
   encheson."
  ()
  ((condition stream)
    (format stream "The label name ~s has already been defined."
      (label-error-name condition))))

;;; -------------------------------------------------------

(define-bespoke-condition Unknown-Label-Error (Label-Error)
  "The ``Unknown-Label-Error'' condition type applies itself to the
   communication of an anomalous situation whose etiology emerges from
   the attempt to refer to a label by a name not accommodated to this
   encheson."
  ()
  ((condition stream)
    (format stream "No label with the name ~s could be detected."
      (label-error-name condition))))

;;; -------------------------------------------------------

(define-bespoke-condition Empty-Queue-Error (Tueue-Error)
  "The ``Empty-Queue-Error'' condition type serves in the apprizal about
   the attempt to inquire into or remove from an empty queue."
  ()
  ((condition stream)
    (format stream "Cannot query or remove from an empty queue.")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Token
  (:constructor make-token (content start-position end-position)))
  "The ``Token'' class serves in the furnishment of a token's
   encapsulation, thilk edifies its diorism upon the prial of a
   sequence of characters desumed from a source and the demarcation
   imposed by the twissel enumerating the inclusive start and the
   exclusive end position."
  (content        (error "Missing content.")
                  :type      string
                  :read-only T)
  (start-position (error "Missing start position.")
                  :type      fixnum
                  :read-only T)
  (end-position   (error "Missing end position.")
                  :type      fixnum
                  :read-only T))

;;; -------------------------------------------------------

(defun token-is-empty-p (token)
  "Determines whether the TOKEN's content represents an empty string,
   returning on confirmation  a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Token token))
  (the boolean
    (get-boolean-value-of
      (empty-string-p
        (token-content token)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction operands.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface serves in the establishment of a common
   fundament to a classes whose telos appertains to the representation
   of Tueue instruction operands.")

;;; -------------------------------------------------------

(defstruct (Integer-Literal
  (:include     Operand)
  (:constructor make-integer-literal (value)))
  "The ``Integer-Literal'' class serves in the ensconcement of a signed
   or unsigned integer number in an instruction operand's guise."
  (value (error "Missing integer literal value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (String-Literal
  (:include     Operand)
  (:constructor make-string-literal (value)))
  "The ``String-Literal'' class serves in the ensconcement of a
   non-empty character sequence in an instruction operand's guise."
  (value (error "Missing string literal value.")
         :type      string
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Identifier
  (:include     Operand)
  (:constructor make-label-identifier (name)))
  "The ``Label-Identifier'' class serves in the ensconcement of label
   name reference in an instruction operand's guise."
  (name (error "Missing label identifier value.")
        :type      string
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Reference
  (:include Operand))
  "The ``Reference'' interface furnishes a fundament for all classes
   serving in the modeling of a Tueue memory component reference, that
   is, either one among the queues' twissel or the accumulator.")

;;; -------------------------------------------------------

(defstruct (First-Queue-Reference
  (:include     Reference)
  (:constructor make-first-queue-reference ()))
  "The ``First-Queue-Reference'' class serves in the communication of a
   behest directed at the first queue bottom element's perquisition or
   modification.")

;;; -------------------------------------------------------

(defstruct (Second-Queue-Reference
  (:include     Reference)
  (:constructor make-second-queue-reference ()))
  "The ``Second-Queue-Reference'' class serves in the communication of a
   behest directed at the second queue bottom element's perquisition or
   modification.")

;;; -------------------------------------------------------

(defstruct (Accumulator-Reference
  (:include     Reference)
  (:constructor make-accumulator-reference ()))
  "The ``Accumulator-Reference'' class serves in the communication of a
   behest directed at the accumulator state's perquisition or
   modification.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instruction class definition operations.   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun assemble-structure-slot (slot-specification)
  "Generates and returns for the SLOT-SPECIFICATION, this being a tuple
   comprehending a structure name and its type, a
   ``defstruct''-compatible slot definition."
  (declare (type (tuple-of symbol symbol) slot-specification))
  (destructuring-bind (slot-name slot-type) slot-specification
    (declare (type symbol slot-name))
    (declare (type symbol slot-type))
    (the (list-of T)
      `(,slot-name (error "Missing value for slot ~s." ',slot-name)
                   :type      ,slot-type
                   :read-only T))))

;;; -------------------------------------------------------

(defun complete-slot-definitions (slot-specifications)
  "Generates and returns from the SLOT-SPECIFICATIONS, thilk comprehend
   a list compact of zero or more symbolic name-type twains, a fresh
   list of ``defstruct''-covenable slot definitions."
  (declare (type (list-of (tuple-of symbol symbol))
                 slot-specifications))
  (the (list-of T)
    (mapcar #'assemble-structure-slot slot-specifications)))

;;; -------------------------------------------------------

(defmacro define-instruction-class (class-name
                                    documentation-string
                                    &rest slot-specifications)
  "Defines a new structure class, its agnomination desumed in an
   ipsissima verba fashion from the CLASS-NAME, automatically being
   subsumed into a direct descendency from the ``Instruction'' class,
   with the slots' derivation emerging from the SLOT-SPECIFICATIONS,
   each such a twissel of a symbolic name and a symbolic type, and
   expecting a documentation string's supplementation for commentary
   purposes."
  `(defstruct (,class-name
     (:include Instruction))
     ,documentation-string
     ,@(complete-slot-definitions slot-specifications)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of instructions.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface establishes a firmament shared by all
   classes in a pursuit of a Tueue operation's modeling.")

;;; -------------------------------------------------------

(define-instruction-class SWP-Instruction
  "The ``SWP-Instruction'' class serves in the encapsulation of the
   Tueue \"SWP\" instruction, dedicated to the transition from one
   stack to the other.")

;;; -------------------------------------------------------

(define-instruction-class ENQ-Instruction
  "The ``ENQ-Instruction'' class serves in the encapsulation of the
   Tueue \"ENQ\" instruction, dedicated to the enqueing of an element
   into the current queue."
  (new-element Operand))

;;; -------------------------------------------------------

(define-instruction-class DEQ-Instruction
  "The ``DEQ-Instruction'' class serves in the encapsulation of the
   Tueue \"DEQ\" instruction, dedicated to the dequeing of an element
   from the current queue.")

;;; -------------------------------------------------------

(define-instruction-class DEC-Instruction
  "The ``DEC-Instruction'' class serves in the encapsulation of the
   Tueue \"DEC\" instruction, dedicated to the decrementation of the
   accumulator by a certain amount."
  (amount Operand))

;;; -------------------------------------------------------

(define-instruction-class INC-Instruction
  "The ``INC-Instruction'' class serves in the encapsulation of the
   Tueue \"INC\" instruction, dedicated to the incrementation of the
   accumulator by a certain amount."
  (amount Operand))

;;; -------------------------------------------------------

(define-instruction-class DIV-Instruction
  "The ``DIV-Instruction'' class serves in the encapsulation of the
   Tueue \"DIV\" instruction, dedicated to the division of the
   accumulator by a certain divisor."
  (divisor Operand))

;;; -------------------------------------------------------

(define-instruction-class MUL-Instruction
  "The ``MUL-Instruction'' class serves in the encapsulation of the
   Tueue \"MUL\" instruction, dedicated to the multiplication of the
   accumulator by a certain factor."
  (factor Operand))

;;; -------------------------------------------------------

(define-instruction-class ADD-Instruction
  "The ``ADD-Instruction'' class serves in the encapsulation of the
   Tueue \"ADD\" instruction, dedicated to addition of the current
   queue's two bottom elements and the resulting sum's enqueing.")

;;; -------------------------------------------------------

(define-instruction-class SUB-Instruction
  "The ``SUB-Instruction'' class serves in the encapsulation of the
   Tueue \"SUB\" instruction, dedicated to addition of the current
   queue's two bottom elements and the resulting sum's enqueing.")

;;; -------------------------------------------------------

(define-instruction-class MOV-Instruction
  "The ``MOV-Instruction'' class serves in the encapsulation of the
   Tueue \"MOV\" instruction, dedicated to assignment of a value to a
   memory component, that is, either one of the queues or the
   accumulator."
  (source      Operand)
  (destination Reference))

;;; -------------------------------------------------------

(define-instruction-class LAB-Instruction
  "The ``LAB-Instruction'' class serves in the encapsulation of the
   Tueue \"LAB\" instruction, dedicated to the declaration of a label."
  (name Label-Identifier))

;;; -------------------------------------------------------

(define-instruction-class JMP-Instruction
  "The ``JMP-Instruction'' class serves in the encapsulation of the
   Tueue \"JMP\" instruction, dedicated to the unconditional relocation
   to a label."
  (target Label-Identifier))

;;; -------------------------------------------------------

(define-instruction-class IQE-Instruction
  "The ``IQE-Instruction'' class serves in the encapsulation of the
   Tueue \"IQE\" instruction, dedicated to the conditional relocation
   to a label based upon the current queue's vacancy state."
  (target Label-Identifier))

;;; -------------------------------------------------------

(define-instruction-class IQJ-Instruction
  "The ``IQJ-Instruction'' class serves in the encapsulation of the
   Tueue \"IQJ\" instruction, dedicated to the conditional relocation
   to a label based upon the current queue's bottom element."
  (guard  Operand)
  (target Label-Identifier))

;;; -------------------------------------------------------

(define-instruction-class IAJ-Instruction
  "The ``IAJ-Instruction'' class serves in the encapsulation of the
   Tueue \"IAJ\" instruction, dedicated to the conditional relocation
   to a label based upon the accumulator's content."
  (guard  Operand)
  (target Label-Identifier))

;;; -------------------------------------------------------

(define-instruction-class ASC-Instruction
  "The ``ASC-Instruction'' class serves in the encapsulation of the
   Tueue \"ASC\" instruction, dedicated to the printing of an argument
   in the form of its associated ASCII character."
  (argument Operand))

;;; -------------------------------------------------------

(define-instruction-class OUT-Instruction
  "The ``OUT-Instruction'' class serves in the encapsulation of the
   Tueue \"OUT\" instruction, dedicated to the printing of an argument
   in its integer form."
  (argument Operand))

;;; -------------------------------------------------------

(define-instruction-class COM-Instruction
  "The ``COM-Instruction'' class serves in the encapsulation of the
   Tueue \"COM\" instruction, dedicated to the inclusing of comments in
   a program.")

;;; -------------------------------------------------------

(define-instruction-class NOP-Instruction
  "The ``NOP-Instruction'' class, constituting an advenient partipant,
   applies itself to the representation of a no-operation (NOP, no-op),
   most commonly affiliated with the notion of a blank line.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token parsing operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-token-as-integer-literal (token)
  "Attempts to parse the TOKEN's content as a signed or unsigned integer
   literal, returning on success an ``Integer-Literal'' encapsulation of
   the probed TOKEN; otherwise produces the ``NIL'' sentinel."
  (declare (type Token token))
  (the (or null Integer-Literal)
    (ignore-errors
      (make-integer-literal
        (nth-value 0
          (parse-integer
            (token-content token)))))))

;;; -------------------------------------------------------

(defun parse-token-as-string-literal (token)
  "Attempts to parse the TOKEN's content as a string literal,
   disencumbered from any damarcating symbol's ensconcement, afflicted,
   however, with the requisite of at least one character's
   participation, returning on success a ``String-Literal''
   encapsulation of the probed TOKEN; otherwise produces the ``NIL''
   sentinel."
  (declare (type Token token))
  (the (or null String-Literal)
    (unless (token-is-empty-p token)
      (make-string-literal
        (token-content token)))))

;;; -------------------------------------------------------

(defun parse-token-as-label-identifier (token)
  "Attempts to parse the TOKEN's content as a label identifier,
   returning on success a ``Label-Identifier'' encapsulation of the
   probed TOKEN; otherwise produces the ``NIL'' sentinel."
  (declare (type Token token))
  (the (or null Label-Identifier)
    (unless (token-is-empty-p token)
      (make-label-identifier
        (token-content token)))))

;;; -------------------------------------------------------

(defun parse-token-as-storage-name (token)
  "Attempts to parse the TOKEN's content as a reference to a mmory
   component, that is, either one of the queues or the accumulator,
   returning on success a ``Reference'' encapsulation of the probed
   TOKEN; otherwise produces the ``NIL'' sentinel."
  (declare (type Token token))
  (the (or null Reference)
    (string-case (token-content token)
      ("ACC"     (make-accumulator-reference))
      ("1QU"     (make-first-queue-reference))
      ("2QU"     (make-second-queue-reference))
      (otherwise NIL))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Tueue program operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-program (instructions)
  "Creates and returns a fresh Tueue ``program'' emerging from the list
   of INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array Instruction (*)))))

;;; -------------------------------------------------------

(defun remove-no-operation-instances (program)
  "Removes, contingently destructively, the ``NOP-Instruction''
   instances from the PROGRAM and returns the resulting ``program''.
   ---
   Please heed that the PROGRAM may or may not be destructively modified
   by this operations; natheless, a program purged of all no-operations
   will be generated."
  (declare (type program program))
  (the program
    (delete-if #'nop-instruction-p program)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of line scanner.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Line-Scanner
  (:constructor make-empty-line-scanner
    (&aux (source   "")
          (position 0)))
  (:constructor make-line-scanner
    (initial-source
     &aux (source   initial-source)
          (position 0))))
  "The ``Line-Scanner'' class applies itself to the dever of a sere
   line's lexical analyzation into its significant components."
  (source   ""  :type string  :read-only NIL)
  (position 0   :type fixnum  :read-only NIL))

;;; -------------------------------------------------------

(defmacro with-line-scanner ((scanner) &body body)
  "Evaluates the line SCANNER, binds its slot ``source'' to the local
   symbol macro ``$source'' and ``position'' to ``$position, evaluates
   the BODY forms, and returns the desinent form's results.
   ---
   Concretely, the following quadruple of symbol macros, one moeity
   furnished by the foundational slot-derived instances, the second
   twissel adscititious and adminicular supererogations, experience
   their diorism:
     ------------------------------------------------------------------
     Symbol macro | Agency
     -------------+----------------------------------------------------
     $source      | The processed line.
     ..................................................................
     $position    | The current position into the $SOURCE.
     ..................................................................
     $character   | The character at the $POSITION into the $SOURCE.
                  | Resolves to the \"null character\" upon the
                  | latter's exhaustion.
     ..................................................................
     $exhausted-p | Determines whether the $POSITION cursor has
                  | transgressed beyond the $SOURCE's marches.
     ------------------------------------------------------------------"
  (let ((evaluated-scanner (gensym)))
    (declare (type symbol evaluated-scanner))
    `(let ((,evaluated-scanner ,scanner))
       (declare (type Line-Scanner ,evaluated-scanner))
       (declare (ignorable         ,evaluated-scanner))
       (symbol-macrolet
           (($source
              (the string
                (line-scanner-source ,evaluated-scanner)))
            ($position
              (the fixnum
                (line-scanner-position ,evaluated-scanner)))
            ($character
              (the character
                (get-character-at $source $position)))
            ($exhausted-p
              (the boolean
                (not (array-in-bounds-p $source $position)))))
         (declare (type string    $source))
         (declare (ignorable      $source))
         (declare (type fixnum    $position))
         (declare (ignorable      $position))
         (declare (type character $character))
         (declare (ignorable      $character))
         ,@body))))

;;; -------------------------------------------------------

(defun set-line-to-process (scanner new-line)
  "Sets the line SCANNER's source to the NEW-LINE, resets its state as
   a parasceve to the analyzation process subjected to its antipication,
   and returns no value."
  (declare (type Line-Scanner scanner))
  (declare (type string       new-line))
  (with-line-scanner (scanner)
    (psetf $source   new-line
           $position 0))
  (values))

;;; -------------------------------------------------------

(defun skip-spaces (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   skips a catena of zero or more accolent spaces and returns no value."
  (declare (type Line-Scanner scanner))
  (with-line-scanner (scanner)
    (setf $position
      (or (position-if-not #'space-character-p $source :start $position)
          (length $source))))
  (values))

;;; -------------------------------------------------------

(defun locate-end-of-token (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   locates the end of the abutting token and returns the position into
   the SOURCE immediately succeeding its desinence."
  (declare (type Line-Scanner scanner))
  (with-line-scanner (scanner)
    (the fixnum
      (or (position-if #'space-character-p $source :start $position)
          (length $source)))))

;;; -------------------------------------------------------

(defun move-to-end-of-current-line (scanner)
  "Relocates the line SCANNER to its current line's desinence and
   returns no value."
  (declare (type Line-Scanner scanner))
  (with-line-scanner (scanner)
    (setf $position
      (length $source)))
  (values))

;;; -------------------------------------------------------

(defun expect-end-of-line (scanner)
  "Determines whether, proceeding from the current position into the
   line SCANNER's source, the remaining line segment comprehends no
   effective content, exempting from this imposition the occurrencies
   of spaces, on confirmation returning no value; otherwise an error of
   an unspecified type is signaled."
  (declare (type Line-Scanner scanner))
  (skip-spaces scanner)
  (with-line-scanner (scanner)
    (unless $exhausted-p
      (error "Expected the line to conclude, but instead encountered ~
              the character \"~c\" at position ~d."
        $character $position)))
  (values))

;;; -------------------------------------------------------

(defun read-next-token (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   consumes the next token and returns a ``Token'' representation
   thereof."
  (declare (type Line-Scanner scanner))
  (with-line-scanner (scanner)
    (skip-spaces scanner)
    (let ((start $position)
          (end   (locate-end-of-token scanner)))
      (declare (type fixnum start))
      (declare (type fixnum end))
      (setf $position end)
      (the Token
        (make-token
          (subseq $source start end)
          start end)))))

;;; -------------------------------------------------------

(defun read-expression (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   consumes an expression token and returns an ``Operand'' encapsulation
   of its haecceity."
  (declare (type Line-Scanner scanner))
  (let ((next-token (read-next-token scanner)))
    (declare (type Token next-token))
    (the Operand
      (or (parse-token-as-storage-name    next-token)
          (parse-token-as-integer-literal next-token)
          (parse-token-as-string-literal  next-token)
          (error "The token ~s does not represent an expression."
            next-token)))))

;;; -------------------------------------------------------

(defun read-memory-reference (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   reads a Tueue storage component name and returns a ``Reference''
   representation thereof."
  (declare (type Line-Scanner scanner))
  (let ((next-token (read-next-token scanner)))
    (declare (type Token next-token))
    (the Reference
      (or (parse-token-as-storage-name next-token)
          (error "The token ~s does not represent a storage name."
            next-token)))))

;;; -------------------------------------------------------

(defun read-label-identifier (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   reads a label name and returns a ``Label-Identifier'' representation
   thereof."
  (declare (type Line-Scanner scanner))
  (let ((next-token (read-next-token scanner)))
    (declare (type Token next-token))
    (the Label-Identifier
      (or (parse-token-as-label-identifier next-token)
          (error "The token ~s does not represent a label name."
            next-token)))))

;;; -------------------------------------------------------

(defun read-instruction (scanner)
  "Proceeding from the current position into the line SCANNER's source,
   consumes a Tueue instruction and returns an ``Instruction''
   tantamount thereof."
  (declare (type Line-Scanner scanner))
  (with-line-scanner (scanner)
    (let ((next-token (read-next-token scanner)))
      (declare (type Token next-token))
      (the Instruction
        (prog1
          (string-case (token-content next-token)
            (""
              (make-nop-instruction))
            
            ("SWP"
              (make-swp-instruction))
            ("ENQ"
              (make-enq-instruction :new-element
                (read-expression scanner)))
            ("DEQ"
              (make-deq-instruction))
            
            ("DEC"
              (make-dec-instruction :amount
                (read-expression scanner)))
            ("INC"
              (make-inc-instruction :amount
                (read-expression scanner)))
            ("DIV"
              (make-div-instruction :divisor
                (read-expression scanner)))
            ("MUL"
              (make-mul-instruction :factor
                (read-expression scanner)))
            
            ("ADD"
              (make-add-instruction))
            ("SUB"
              (make-sub-instruction))
            
            ("MOV"
              (make-mov-instruction
                :source      (read-expression       scanner)
                :destination (read-memory-reference scanner)))
            
            ("LAB"
              (make-lab-instruction :name
                (read-label-identifier scanner)))
            ("JMP"
              (make-jmp-instruction :target
                (read-label-identifier scanner)))
            ("IQE"
              (make-iqe-instruction :target
                (read-label-identifier scanner)))
            ("IQJ"
              (make-iqj-instruction
                :guard  (read-expression       scanner)
                :target (read-label-identifier scanner)))
            ("IAJ"
              (make-iaj-instruction
                :guard  (read-expression       scanner)
                :target (read-label-identifier scanner)))
            
            ("ASC"
              (make-asc-instruction :argument
                (read-expression scanner)))
            ("OUT"
              (make-out-instruction :argument
                (read-expression scanner)))
            
            ("COM"
              (move-to-end-of-current-line scanner)
              (make-com-instruction))
            
            (otherwise
              (error "The token ~s does not introduce an instruction."
                next-token)))
          
          (expect-end-of-line scanner))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program parser.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-instructions (code)
  "Extracts from the piece of Tueue source CODE the incorporated
   instructions and returns a fresh ``program'' representation thereof."
  (declare (type string code))
  (let ((scanner (make-empty-line-scanner)))
    (declare (type Line-Scanner scanner))
    (with-input-from-string (code-stream code)
      (declare (type string-stream code-stream))
      (the program
        (remove-no-operation-instances
          (make-program
            (loop
              for code-line
                of-type (or null string)
                =       (read-line code-stream NIL NIL)
              while code-line collect
                (progn
                  (set-line-to-process scanner code-line)
                  (read-instruction    scanner)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of label table operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-empty-label-table ()
  "Creates and returns a fresh and initially vacant ``label-table''."
  (the label-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun define-label (labels name line-number)
  "Registers a new label specified by its name and associated with the
   LINE-NUMBER at the LABELS table and returns no value.
   ---
   If a label amenable to the NAME already exists among the LABELS, an
   error of the type ``Duplicate-Label-Error'' is signaled."
  (declare (type label-table labels))
  (declare (type string      name))
  (declare (type fixnum      line-number))
  (if (nth-value 1 (gethash name labels))
    (error 'Duplicate-Label-Error :name name)
    (setf (gethash name labels) line-number))
  (values))

;;; -------------------------------------------------------

(defun locate-label (labels name)
  "Returns the zero-based line number associated with the definition of
   the label NAME in the LABELS table, or signals an error of the type
   ``Unknown-Label-Error'' upon its disrespondency."
  (declare (type label-table labels))
  (declare (type string      name))
  (the fixnum
    (or (gethash name labels)
        (error 'Unknown-Label-Error :name name))))

;;; -------------------------------------------------------

(defun collect-labels (program)
  "Collects the label definitions of present participation in the Tueue
   PROGRAM, affiliating the label names with their zero-based line
   numbers, and returns the resulting ``label-table''."
  (declare (type program program))
  (let ((labels (make-empty-label-table)))
    (declare (type label-table labels))
    (loop
      for current-instruction of-type Instruction across program
      and line-number         of-type fixnum      from   0 by 1
      when (lab-instruction-p current-instruction) do
        (define-label labels
          (label-identifier-name
            (lab-instruction-name current-instruction))
          line-number))
    (the label-table labels)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of singly linked node.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor make-node (element &optional (next NIL)))
  (:print-function
    (lambda (node stream depth)
      (declare (type Node        node))
      (declare (type destination stream))
      (declare (type integer     depth)
               (ignore           depth))
      (format stream "(Node :element ~s :has-next ~:[no~;yes~])"
        (node-element node)
        (node-next    node)))))
  "The ``Node'' class serves in the encapsulation of a singly linked
   node's notion, capacitated in its castaldy of an element and a
   reference to a contingent successor to partake of a singly linked
   list or, by extension, a queue."
  (element (error "Missing node element.")
           :type      tueue-object
           :read-only NIL)
  (next    (error "Missing node successor.")
           :type      (or null Node)
           :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of queue.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Queue ()
  ((head
    :initform      NIL
    :accessor      queue-head
    :type          (or null Node)
    :documentation "The first element's node.")
   (tail
    :initform      NIL
    :accessor      queue-tail
    :type          (or null Node)
    :documentation "The desinent element's node.")
   (size
    :initform      0
    :accessor      queue-size
    :type          (integer 0 *)
    :documentation "The tally of elements in this queue.
                    ---
                    This account also includes the HEAD and TAIL
                    nodes, if defined."))
  (:documentation
    "The ``Queue'' class furnishes an implementation of the queue
     abstract data structure (ADT) edified upon the firmament of a
     singly linked list."))

;;; -------------------------------------------------------

(defun make-empty-queue ()
  "Creates and returns an initially vacant ``Queue''."
  (the Queue
    (make-instance 'Queue)))

;;; -------------------------------------------------------

(defun queue-is-empty-p (queue)
  "Determines whether the QUEUE is empty, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Queue queue))
  (the boolean
    (get-boolean-value-of
      (zerop
        (queue-size queue)))))

;;; -------------------------------------------------------

(defun enqueue (queue new-element)
  "Inserts the NEW-ELEMENT at the QUEUE's rear and returns no value."
  (declare (type Queue        queue))
  (declare (type tueue-object new-element))
  (let ((new-node (make-node new-element NIL)))
    (declare (type Node new-node))
    (if (queue-is-empty-p queue)
      (setf (queue-head queue)             new-node)
      (setf (node-next (queue-tail queue)) new-node))
    (setf (queue-tail queue) new-node)
    (incf (queue-size queue)))
  (values))

;;; -------------------------------------------------------

(defun check-for-non-empty-queue (queue)
  "Determines whether the QUEUE contains at least one element, on
   confirmation returning no value, otherwise signals an error of the
   type ``Empty-Queue-Error''."
  (declare (type Queue queue))
  (when (queue-is-empty-p queue)
    (error 'Empty-Queue-Error))
  (values))

;;; -------------------------------------------------------

(defun dequeue (queue)
  "Removes and returns the element at the QUEUE's front.
   ---
   If the QUEUE resided in a vacant state at the instant of the deletion
   behest's issuance, an error of the type ``Empty-Queue-Error'' is
   signaled."
  (declare (type Queue queue))
  (check-for-non-empty-queue queue)
  (the tueue-object
    (prog1
      (node-element
        (queue-head queue))
      (setf (queue-head queue)
        (node-next
          (queue-head queue)))
      (decf (queue-size queue))
      (when (queue-is-empty-p queue)
        (setf (queue-tail queue) NIL)))))

;;; -------------------------------------------------------

(defun peek-front (queue)
  "Returns without removing the element at the QUEUE's front.
   ---
   If the QUEUE resided in a vacant state at the instant of the deletion
   behest's issuance, an error of the type ``Empty-Queue-Error'' is
   signaled."
  (declare (type Queue queue))
  (check-for-non-empty-queue queue)
  (the tueue-object
    (node-element
      (queue-head queue))))

;;; -------------------------------------------------------

(defun replace-front (queue new-element)
  "Replaces the element at the QUEUE's front by the NEW-ELEMENT and
   returns no value.
   ---
   If the QUEUE resided in a vacant state at the instant of the deletion
   behest's issuance, an error of the type ``Empty-Queue-Error'' is
   signaled."
  (declare (type Queue        queue))
  (declare (type tueue-object new-element))
  (check-for-non-empty-queue queue)
  (setf
    (node-element
      (queue-head queue))
    new-element)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((queue Queue) (stream T))
  (declare (type Queue       queue))
  (declare (type destination stream))
  (loop
    initially
      (format stream "(Queue")
    for current-node
      of-type (or null Node)
      =       (queue-head queue)
      then    (node-next  current-node)
    while current-node do
      (format stream " ~s"
        (node-element current-node))
    finally
      (format stream ")")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of printing operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric print-as-number (argument)
  (:documentation
    "Prints the ARGUMENT in a numeric to the standard output, succeeded
     by an unconditional newline character, and returns no value.")
  
  (:method ((argument integer))
    (declare (type integer argument))
    (format T "~&~d~%" argument)
    (values))
  
  (:method ((argument string))
    (declare (type string argument))
    (loop for character of-type character across argument do
      (print-as-number
        (char-code character)))
    (values)))

;;; -------------------------------------------------------

(defgeneric print-as-character (argument)
  (:documentation
    "Prints the ARGUMENT in a character form to the standard output,
     neither preceded nor succeeded by any further content, and returns
     no value.")
  
  (:method ((argument integer))
    (declare (type integer argument))
    (format T "~c"
      (code-char argument))
    (values))
  
  (:method ((argument string))
    (declare (type string argument))
    (format T "~a" argument)
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((program
    :initarg       :program
    :initform      (error "Missing program for interpreter.")
    :reader        program-instructions
    :type          program
    :documentation "The Tueue instructions to process.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          integer
    :documentation "The instruction pointer's (IP) or program counter's
                    (PC) zero-based current location in the PROGRAM.")
   (labels
    :reader        program-labels
    :type          label-table
    :documentation "Association betwixt the defined label names and
                    their zero-based line numbers in the PROGRAM.")
   (first-queue
    :initform      (make-empty-queue)
    :reader        first-program-queue
    :type          Queue
    :documentation "The first memory queue.")
   (second-queue
    :initform      (make-empty-queue)
    :reader        second-program-queue
    :type          Queue
    :documentation "The second memory queue.")
   (current-queue-id
    :initform      :first
    :accessor      current-program-queue-id
    :type          selected-queue-id
    :documentation "A flag which determines which member of the queue's
                    twissel is currently active.")
   (accumulator
    :initform      0
    :type          tueue-object
    :accessor      program-accumulator
    :documentation "The adminicular memory accumulator."))
  (:documentation
    "The ``Interpreter'' class is apportioned that dever to accompass
     veridical efficacy to a Tueue program purveyed in an instruction
     listing's guise."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter Interpreter) &key)
  "Gathers the INTERPRETER program's labels, stores the same in the
   INTERPRETER, and returns no value."
  (declare (type Interpreter interpreter))
  (setf (slot-value interpreter 'labels)
    (collect-labels
      (slot-value interpreter 'program)))
  (values))

;;; -------------------------------------------------------

(defun make-interpreter (program)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Tueue
   PROGRAM's execution."
  (declare (type program program))
  (the Interpreter
    (make-instance 'Interpreter :program program)))

;;; -------------------------------------------------------

(defun current-program-queue (interpreter)
  "Returns a reference to the INTERPRETER's currently active queue."
  (declare (type Interpreter interpreter))
  (with-slots (current-queue-id) interpreter
    (declare (type selected-queue-id current-queue-id))
    (the Queue
      (case current-queue-id
        (:first
          (slot-value interpreter 'first-queue))
        (:second
          (slot-value interpreter 'second-queue))
        (otherwise
          (error "Invalid current queue ID: ~s."
            current-queue-id))))))

;;; -------------------------------------------------------

(defmacro with-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots to representative local
   symbol macros, evaluates the BODY forms, and returns the desinent
   form's results.
   ---
   The following equiparation applies to the INTERPRETER's slot names
   and this context's local symbol macro diorisms:
     --------------------------------------
     Slot name        | Local symbol macro
     -----------------+--------------------
     program          | $program
     ......................................
     ip               | $ip
     ......................................
     labels           | $labels
     ......................................
     first-queue      | $first-queue
     ......................................
     second-queue     | $second-queue
     ......................................
     current-queue-id | $current-queue-id
     ......................................
     current-queue    | $current-queue
     ......................................
     accumulator      | $accumulator
     --------------------------------------
   As an ultimity of supererogation, the following adscititious symbol
   macros partake of an adminicular agency, their provenance certain
   utible interpreter functions:
     -------------------------------------------
     Operation name        | Local symbol macro
     ----------------------+--------------------
     current-program-queue | $current-queue
     -------------------------------------------"
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type Interpreter ,evaluated-interpreter))
       (declare (ignorable        ,evaluated-interpreter))
       (symbol-macrolet
           (($program
             (the program
               (slot-value ,evaluated-interpreter 'program)))
            ($ip
             (the integer
               (slot-value ,evaluated-interpreter 'ip)))
            ($labels
             (the label-table
               (slot-value ,evaluated-interpreter 'labels)))
            ($first-queue
             (the Queue
               (slot-value ,evaluated-interpreter 'first-queue)))
            ($second-queue
             (the Queue
               (slot-value ,evaluated-interpreter 'second-queue)))
            ($current-queue-id
             (the selected-queue-id
               (slot-value ,evaluated-interpreter 'current-queue-id)))
            ($current-queue
             (the Queue
               (current-program-queue ,evaluated-interpreter)))
            ($accumulator
             (the tueue-object
               (slot-value ,evaluated-interpreter 'accumulator))))
         (declare (type program           $program))
         (declare (ignorable              $program))
         (declare (type integer           $ip))
         (declare (ignorable              $ip))
         (declare (type label-table       $labels))
         (declare (ignorable              $labels))
         (declare (type Queue             $first-queue))
         (declare (ignorable              $first-queue))
         (declare (type Queue             $second-queue))
         (declare (ignorable              $second-queue))
         (declare (type selected-queue-id $current-queue-id))
         (declare (ignorable              $current-queue-id))
         (declare (type Queue             $current-queue))
         (declare (ignorable              $current-queue))
         (declare (type tueue-object      $accumulator))
         (declare (ignorable              $accumulator))
         ,@body))))

;;; -------------------------------------------------------

(defgeneric resolve-operand (interpreter operand)
  (:documentation
    "Returns the OPERAND's value in the INTERPRETER's context.")
  
  (:method ((interpreter Interpreter)
            (operand     Integer-Literal))
    (declare (type Interpreter     interpreter))
    (declare (ignore               interpreter))
    (declare (type Integer-Literal operand))
    (the integer
      (integer-literal-value operand)))
  
  (:method ((interpreter Interpreter)
            (operand     String-Literal))
    (declare (type Interpreter    interpreter))
    (declare (ignore              interpreter))
    (declare (type String-Literal operand))
    (the string
      (string-literal-value operand)))
  
  (:method ((interpreter Interpreter)
            (operand     Label-Identifier))
    (declare (type Interpreter      interpreter))
    (declare (type Label-Identifier operand))
    (the string
      (label-identifier-name operand)))
  
  (:method ((interpreter Interpreter)
            (operand     First-Queue-Reference))
    (declare (type Interpreter           interpreter))
    (declare (type First-Queue-Reference operand))
    (the tueue-object
      (with-interpreter (interpreter)
        (peek-front $first-queue))))
  
  (:method ((interpreter Interpreter)
            (operand     Second-Queue-Reference))
    (declare (type Interpreter            interpreter))
    (declare (type Second-Queue-Reference operand))
    (the tueue-object
      (with-interpreter (interpreter)
        (peek-front $second-queue))))
  
  (:method ((interpreter Interpreter)
            (operand     Accumulator-Reference))
    (declare (type Interpreter           interpreter))
    (declare (type Accumulator-Reference operand))
    (the tueue-object
      (program-accumulator interpreter))))

;;; -------------------------------------------------------

(defun program-completed-p (interpreter)
  "Determines whether the Tueue program consigned to the INTERPRETER's
   castaldy has been executed in its entirety, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (with-interpreter (interpreter)
      (get-boolean-value-of
        (not (array-in-bounds-p $program $ip))))))

;;; -------------------------------------------------------

(defun advance-to-next-instruction (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   instruction in its maintained Tueue program and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (incf $ip))
  (values))

;;; -------------------------------------------------------

(defun jump-to-label (interpreter target-name)
  "Relocates the INTERPRETER's instruction pointer (IP) to the line
   comprehending the label definition of the TARGET-NAME and returns no
   value."
  (declare (type Interpreter interpreter))
  (declare (type string      target-name))
  (with-interpreter (interpreter)
    (setf $ip
      (locate-label $labels target-name)))
  (values))

;;; -------------------------------------------------------

(defun current-instruction (interpreter)
  "Returns the Tueue instruction currently processed by the
   INTERPRETER."
  (declare (type Interpreter interpreter))
  (the Instruction
    (with-interpreter (interpreter)
      (aref $program $ip))))

;;; -------------------------------------------------------

(defun swap-current-queue (interpreter)
  "Sets the INTERPRETER's current queue to the contemporaneously
   inactive member and returns no value."
  (declare (type Interpreter interpreter))
  (with-interpreter (interpreter)
    (setf $current-queue-id
      (case $current-queue-id
        (:first  :second)
        (:second :first)
        (otherwise
          (error "Invalid current queue ID: ~s." $current-queue-id)))))
  (values))

;;; -------------------------------------------------------

(defgeneric copy-data-to (interpreter destination source)
  (:documentation
    "Stores the SOURCE datum in the DESTINATION, mediated by the
     INTERPRETER's memory component, and returns no value.
     ---
     This operation's interface maintains a stark mete of concinnity
     with the Tueue instruction \"MOV\", where the DESTINATION refers
     to the second argument, whereas the SOURCE conflates with the
     first \"MOVE\" input.")
  
  (:method ((interpreter Interpreter)
            (destination First-Queue-Reference)
            (source      T))
    (declare (type Interpreter           interpreter))
    (declare (type First-Queue-Reference destination))
    (declare (type tueue-object          source))
    (with-interpreter (interpreter)
      (replace-front $first-queue source))
    (values))
  
  (:method ((interpreter Interpreter)
            (destination Second-Queue-Reference)
            (source      T))
    (declare (type Interpreter            interpreter))
    (declare (type Second-Queue-Reference destination))
    (declare (type tueue-object           source))
    (with-interpreter (interpreter)
      (replace-front $second-queue source))
    (values))
  
  (:method ((interpreter Interpreter)
            (destination Accumulator-Reference)
            (source      T))
    (declare (type Interpreter           interpreter))
    (declare (type Accumulator-Reference destination))
    (declare (type tueue-object          source))
    (with-interpreter (interpreter)
      (setf $accumulator source))
    (values)))

;;; -------------------------------------------------------

(defgeneric process-instruction (interpreter instruction)
  (:documentation
    "Evaluates the Tueue INSTRUCTION in the INTERPRETER's context and
     returns no value."))

;;; -------------------------------------------------------

(defmacro define-instruction-processor (instruction-class &body body)
  "Defines an implementation of the generic function
   ``process-instruction'', stevening its first formal parameter
   ``$interpreter'' and its second ``$instruction'', the latter being
   employed to dispatch on the INSTRUCTION class, evaluates the BODY
   forms, and returns no value."
  `(defmethod process-instruction (($interpreter Interpreter)
                                   ($instruction ,instruction-class))
     (declare (type Interpreter        $interpreter))
     (declare (ignorable               $interpreter))
     (declare (type ,instruction-class $instruction))
     (declare (ignorable               $instruction))
     ,@body
     (values)))

;;; -------------------------------------------------------

(define-instruction-processor ADD-Instruction
  (with-interpreter ($interpreter)
    (let ((a (dequeue $current-queue))
          (b (dequeue $current-queue)))
      (declare (type tueue-object a))
      (declare (type tueue-object b))
      (enqueue $current-queue
        (+ b a)))))

;;; -------------------------------------------------------

(define-instruction-processor ASC-Instruction
  (print-as-character
    (resolve-operand $interpreter
      (asc-instruction-argument $instruction))))

;;; -------------------------------------------------------

(define-instruction-processor COM-Instruction)

;;; -------------------------------------------------------

(define-instruction-processor DEC-Instruction
  (with-interpreter ($interpreter)
    (decf $accumulator
      (resolve-operand $interpreter
        (dec-instruction-amount $instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor DEQ-Instruction
  (with-interpreter ($interpreter)
    (dequeue $current-queue)))

;;; -------------------------------------------------------

(define-instruction-processor DIV-Instruction
  (with-interpreter ($interpreter)
    (setf $accumulator
      (ceiling $accumulator
        (resolve-operand $interpreter
          (div-instruction-divisor $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor ENQ-Instruction
  (with-interpreter ($interpreter)
    (enqueue $current-queue
      (resolve-operand $interpreter
        (enq-instruction-new-element $instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor IAJ-Instruction
  (with-interpreter ($interpreter)
    (when (are-equal-p
            $accumulator
            (resolve-operand $interpreter
              (iaj-instruction-guard $instruction)))
      (jump-to-label $interpreter
        (resolve-operand $interpreter
          (iaj-instruction-target $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor INC-Instruction
  (with-interpreter ($interpreter)
    (incf $accumulator
      (resolve-operand $interpreter
        (inc-instruction-amount $instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor IQE-Instruction
  (with-interpreter ($interpreter)
    (when (queue-is-empty-p $current-queue)
      (jump-to-label $interpreter
        (resolve-operand $interpreter
          (iqe-instruction-target $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor IQJ-Instruction
  (with-interpreter ($interpreter)
    (when (are-equal-p
            (peek-front $current-queue)
            (resolve-operand $interpreter
              (iqj-instruction-guard $instruction)))
      (jump-to-label $interpreter
        (resolve-operand $interpreter
          (iqj-instruction-target $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor JMP-Instruction
  (with-interpreter ($interpreter)
    (jump-to-label $interpreter
      (resolve-operand $interpreter
        (jmp-instruction-target $instruction)))))

;;; -------------------------------------------------------

(define-instruction-processor LAB-Instruction)

;;; -------------------------------------------------------

(define-instruction-processor MOV-Instruction
  (copy-data-to $interpreter
    (mov-instruction-destination $instruction)
    (resolve-operand $interpreter
      (mov-instruction-source $instruction))))

;;; -------------------------------------------------------

(define-instruction-processor MUL-Instruction
  (with-interpreter ($interpreter)
    (setf $accumulator
      (* $accumulator
         (resolve-operand $interpreter
           (mul-instruction-factor $instruction))))))

;;; -------------------------------------------------------

(define-instruction-processor OUT-Instruction
  (print-as-number
    (resolve-operand $interpreter
      (out-instruction-argument $instruction))))

;;; -------------------------------------------------------

(define-instruction-processor SUB-Instruction
  (with-interpreter ($interpreter)
    (let ((a (dequeue $current-queue))
          (b (dequeue $current-queue)))
      (declare (type tueue-object a))
      (declare (type tueue-object b))
      (enqueue $current-queue
        (- b a)))))

;;; -------------------------------------------------------

(define-instruction-processor SWP-Instruction
  (swap-current-queue $interpreter))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the Tueue program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-completed-p interpreter) do
    (process-instruction interpreter
      (current-instruction interpreter))
    (advance-to-next-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Tueue (code)
  "Interprets the piece of Tueue source CODE and returns no value."
  (declare (type string code))
  (execute-program
    (make-interpreter
      (extract-instructions code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-Tueue
  "
  ASC 72
  ASC 101
  ASC 108
  ASC 108
  ASC 111
  ASC 44
  ASC 32
  ASC 87
  ASC 111
  ASC 114
  ASC 108
  ASC 100
  ASC 33
  ")

;;; -------------------------------------------------------

;; Count up from inclusive one (1) to inclusive (10).
(interpret-Tueue
  "
  LAB countUp
  INC 1
  OUT ACC
  IAJ 10 endProgram
  JMP countUp
  LAB endProgram
  ")

;;; -------------------------------------------------------

;; Interpreter for the esoteric programming language
;; "Bitwise Cyclic Tag" (BCT).
(interpret-Tueue
  "
  COM == INITIALIZATION OF PROGRAM STRING (00111) ==
  
  ENQ 0
  ENQ 0
  ENQ 1
  ENQ 1
  ENQ 1
  ENQ @
  SWP
  
  
  COM == INITIALIZATION OF DATA STRING (101) ==
  
  ENQ 1
  ENQ 0
  ENQ 1
  SWP
  
  
  COM == IMPLEMENTATION OF BCT INTERPRETER ==
  
  LAB SRT
  
  COM Halt if data string is empty.
  SWP
  IQE END
  SWP
  
  MOV 1QU ACC
  DEQ
  ENQ ACC
  IAJ 0 EX0
  IAJ 1 EX1
  IAJ @ IEM
  JMP SRT
  
  LAB EX0
  SWP
  DEQ
  SWP
  JMP SRT
  
  LAB EX1
  SWP
  IQJ 0 SJM
  SWP
  IQJ @ IF@
  SWP
  ENQ 1QU
  SWP
  ENQ 1QU
  DEQ
  JMP SRT
  
  LAB SJM
  SWP
  ENQ 1QU
  DEQ
  JMP SRT
  
  LAB IF@
  DEQ
  ENQ @
  SWP
  ENQ 1QU
  SWP
  ENQ 1QU
  DEQ
  JMP SRT
  
  LAB IEM
  DEQ
  ENQ @
  IQE END
  JMP SRT
  
  LAB END
  ")

;;; -------------------------------------------------------

;; Looping counter which prints ten lines.
(interpret-Tueue
  "COM The accumulator maintains the current line number.
   MOV 1 ACC
   
   COM The first queue maintains the line's number of printed asterisks.
   ENQ 0
   
   LAB printLine
   IAJ 11 haltProgram
   
   LAB printAsterisk
   ASC 42
   ENQ 1
   ADD
   IQJ ACC concludeCurrentLine
   JMP printAsterisk
   
   LAB concludeCurrentLine
   ASC 10
   INC 1
   DEQ
   ENQ 0
   JMP printLine
   
   LAB haltProgram")
