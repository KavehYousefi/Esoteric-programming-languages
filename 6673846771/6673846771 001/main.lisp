;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "6673846771", invented by the Esolang user "Xi-816" and
;; presented on September 21st, 2023, its proprium's provenance woning
;; in the instructions' designment in accordance with an assembler
;; language, thilk ensues the capacitation for the perquisition and
;; modification of a random-access memory comprehending signed
;; integer-valued cells, amenable to non-negative integral subscripts.
;; 
;; 
;; Concept
;; =======
;; The 6673846771 programming languages ostends a lealty to the
;; syntactical conformations that popular acquaintance ligates with
;; assembler languages, both in the agnomination of its instructions
;; identifiers, the operand additament's mode, as well as the
;; statements' distribution across seriatim lines, each a dedication to
;; at most one specimen's commorancy, their telos the modulation and
;; perquisition of an infinite vector entailing signed integer numbers.
;; 
;; == THE PROGRAM: AN ASSEMBLER-LIKE CATENA OF STATEMENTS ==
;; Ordained to the wike of the spiritus rectus in its program diorism,
;; the 6673846771 language's operative componency ensues from an ordered
;; sequence of lines, thilk tolerate each at most one instruction's
;; tenancy.
;; 
;; == THE MEMORY: A UNILATERALLY INFINITE DISPANSION OF INTEGERS ==
;; The substratum of 6673846771's data castaldy defines itself in terms
;; of a one-dimensional catena encompassing signed integer numbers, to
;; whom no imposition of the mickleness' stature bewrays a stipulation.
;; 
;; Operating on a random-access foundry, its bournless accompt of
;; positions inchoates with the minimum index of zero (0), but retains
;; a disencumberance along the upper laterality's ascent.
;; 
;; == INPUT AND OUTPUT ARE REALIZED BY "STRUCTURES" ==
;; The intercourse's governail, inwith whose diorism the input and
;; output actuations transpire, accompts for the bailiwick of the so
;; called "structures", connable representations to whom, in the
;; phenotypical aspect, a set of three operations inclavates the
;; interface, and whose genotype's amplection lays its compass along
;; an signed integer-valued buffer, as well as a pointer into this
;; salvatory's next amenable position.
;; 
;; == STRUCTURES RESPOND TO THREE OPERATIONS ==
;; The boustrophedon commerce's distinguishment does not enumerate a
;; mere directional conspection; a fortiori, a rather convolute
;; nomothesia's pertinence serves as a sepiment to the bifurcating
;; cases in their deportment and expectactions.
;; 
;; A consangunity of gremial qualification establishes the vincula
;; alligating the structures internal buffer and pointer states on one
;; hand, and the operative triad on the other. A consectary obtained
;; from this intricacy's participation, the operative trisulk shall
;; be the following treatise of compendiousness dation:
;; 
;;   ------------------------------------------------------------------
;;   Operation | Causatum
;;   ----------+-------------------------------------------------------
;;   snd       | Sends a signed integer number to the structure.
;;             |-------------------------------------------------------
;;             | Only the "out" facility reacts to this behest.
;;   ..................................................................
;;   mvp       | Moves the structure pointer relative to its current
;;             | position.
;;             |-------------------------------------------------------
;;             | The "out" facility advances its pointer automatically
;;             | upon each "snd" request in order to fill its buffer.
;;   ..................................................................
;;   cll       | Activates the structure's actual services.
;;             |-------------------------------------------------------
;;             | For the "out" facility, the buffer content is copied
;;             | the standard output conduit, ere the buffer's
;;             | clearance.
;;             |-------------------------------------------------------
;;             | For the "in" conduit, a request for a signed integer
;;             | number is issued to the standard input channel.
;;   ------------------------------------------------------------------
;; 
;; == OUTPUT STRUCTURE "OUT": A CHARACTER PRINTER ==
;; The ostention of information across the standard output channel
;; is assigned to the "out" output structure's bailiwick, thilk, upon
;; each integeral number's reception, stores the obtained datum in its
;; buffer's contemporaneously selected position, designated by internal
;; structure pointer, ere advacing this cursor to the subsequent
;; position for contingent further behests.
;; 
;; A replication of this structure's working scheme shall be the
;; alow produced pseudocode tmema's cynosure:
;; 
;;   procedure snd (buffer, command)
;;     buffer[pointer] <- command.message
;;     pointer         <- pointer + 1
;;   end procedure
;;   
;;   procedure mvp (buffer, command)
;;     pointer <- pointer + command.offset
;;   end
;;   
;;   procedure cll (buffer)
;;     for bufferIndex from 0 to (buffer.size - 1) do
;;       let character        <- buffer[bufferIndex]
;;       let characterForCode <- getCharacterForCode(characterCode)
;;       
;;       print characterForCode
;;     end for
;;     
;;     buffer.clear()
;;     pointer <- 0
;;   end procedure
;; 
;; == INPUT STRUCTURE "IN": A INTEGRAL INPUT RECEIVER ==
;; The overthwart communication's afferent procession, from the standard
;; input into the program, experiences its manifestation via the "in"
;; structure's arbitration, a warklume whose data species' expectancy
;; accompts for a single signed or unsigned integer number's
;; appropriation.
;; 
;; A sequela to its input's reception, the datum transfer's destination
;; cell in the memory constitutes a kith's dation from the special
;; cell with the index 9999, inwith whom shall be as the incolant the
;; index of the cell to write the input response to.
;; 
;; A further proprium's incorporation vouchsafes to this conduit an
;; overthwart conception juxtaposed to the "out" structure: the
;; obligation anent its pointer's motation as an explicit affair. The
;; "out" counterpart applies itself in an automatic fashion to the
;; next buffer position's appropriation, mediated by this indicial
;; cursor's forward advancement; its tendance airted at the athwart
;; notion, the "in" structure's wike incorporates a manual procession's
;; realization. A consequence of its incipient input reception, the
;; input conduit memorizes the obtained datum and stores the same in
;; its buffer at the subscript imposed by the pointer location. Upon a
;; forward perambulation's lapsus, the next behest directed at this
;; structure will yield, without any interactive implications, the thus
;; memorized value. In the pursuit of the next intercourse's actuation,
;; the pointer ought to be avaunted to a not yet occupied buffer cell
;; via the "mvp" instruction.
;; 
;; This conduit's deportment, with respect to the trisulc operations'
;; establishment, limns the following principles' tantamount in a
;; pseudocode designment:
;; 
;;   procedure snd (buffer)
;;   end
;;   
;;   procedure mvp (buffer)
;;     pointer <- pointer + mvp.offset
;;   end procedure
;;   
;;   procedure cll (buffer)
;;     if buffer[pointer] is not set then
;;       buffer[pointer] <- queryForIntegerNumber()
;;     end if
;;     
;;     memory[memory[9999]] <- buffer[pointer]
;;   end procedure
;; 
;; 
;; Syntax
;; ======
;; 6673846771's syntactical conformation consigns its lealty to an
;; assembler language' simulacrum, accommodating to each statement a
;; line of its personal paraphernalia, the instruction part of which
;; ostends a mnemonic agnomination, separated from the operand moeity
;; by one or more spaces, the interstices betwixt these adminicular
;; pieces of data themselves wisting of a single comma's agency as the
;; merist.
;; 
;; == PROGRAMS: LINES OF STATEMENTS ==
;; Anent the syntactical administration of conspectuity, a 6673846771
;; program's conformation is entreparted into a catena of lines, each
;; such assigning a commorancy to at most one instruction.
;; 
;; == INSTRUCTIONS: TOKENS SPECIFYING OPERATION NAMES AND OPERANDS ==
;; An instruction's parasceuastic warklume limns an incolant in the
;; identifying name, consanguinous in the chosen agnomination to an
;; assembly language's mnemonic, and a sequence enumerating zero or
;; more operands, or arguments, each token's merist realized in one or
;; more space characters.
;; 
;; == COMMENTS ==
;; The provision for descants in the language is edified upon the
;; parasceuastic dollar sign, "$", whence ensues, until the end of the
;; respective line, a neglect's parcery to all subsequent characters.
;; 
;; == GRAMMAR ==
;; The language's donat shall experience a valorized mete of
;; formalization's dation in the following Extended Backus-Naur Form
;; (EBNF) explication:
;; 
;;   program         := { innerLine } , finalLine ;
;;   innerLine       := lineContent , newline   ;
;;   finalLine       := lineContent ;
;;   
;;   lineContent     := [ command ] , [ comment ] ;
;;   command         := pushCommand
;;                   |  addCommand
;;                   |  subCommand
;;                   |  mulCommand
;;                   |  divCommand
;;                   |  swpCommand
;;                   |  cpyCommand
;;                   |  sndCommand
;;                   |  mvpCommand
;;                   |  cllCommand
;;                   |  labelDefCommand
;;                   |  jmpCommand
;;                   ;
;;   
;;   comment         := "$" , commentText ;
;;   commentText     := { character - newline } ;
;;   
;;   pushCommand     := "push" , address , "," , operand ;
;;   addCommand      := "add" , binaryOpSuffix
;;   subCommand      := "sub" , binaryOpSuffix
;;   mulCommand      := "mul" , binaryOpSuffix
;;   divCommand      := "div" , binaryOpSuffix ;
;;   binaryOpSuffix  := operand , "," , operand , "," , address ;
;;   
;;   swpCommand      := "swp" , addressTwain ;
;;   cpyCommand      := "cpy" , addressTwain ;
;;   addressTwain    := address , "," , address ;
;;   
;;   sndCommand      := "snd" , structure , "," , address ;
;;   mvpCommand      := "mvp" , structure , "1" ;
;;   cllCommand      := "cll" , structure ;
;;   
;;   labelDefCommand := ":" , labelName , ":" ;
;;   jmpCommand      := "jmp" , address , "," , labelName ;
;;   labelName       := labelChar , { labelChar } ;
;;   labelChar       := character - ( whitespace | "," | ":" ) ;
;;   
;;   structure       := "in" | "out" ;
;;   
;;   operand         := integerLiteral | address;
;;   integerLiteral  := optionalSign , digits ;
;;   optionalSign    := [ "+" | "-" ] ;
;;   address         := digits , "c"
;;   
;;   digits          := digit , { digit } ;
;;   digit           := "0" | "1" | "2" | "3" | "4"
;;                   |  "5" | "6" | "7" | "8" | "9"
;;                   ;
;;   
;;   whitespace      := space | newline ;
;;   newline         := "\f" | "\n" | "\r" | "\v" ;
;;   space           := " "  | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The 6673846771 programming language's instruction set enumerates a
;; cardinality amplecting a duodecimal accompt of members, the
;; facilities participating in this diorism appertaining to the
;; memory cells' assignment and management, basic arithmetics, input
;; and output communication, as well as a conditional goto construct.
;; 
;; == OVERVIEW ==
;; Please heed the succedanea's demarcation by a catena of asterisks
;; ("*"), intended to be replaced by actual 6673846771 code tmemata in
;; the ultimate program.
;; 
;;   ------------------------------------------------------------------
;;   Command                 | Description
;;   ------------------------+-----------------------------------------
;;   push target, source     | Stores the {source} value in the cell
;;        ******  ******     | amenable to the {target} address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;                           |-----------------------------------------
;;                           | {source} must either be an integer
;;                           | literal or an address.
;;   ..................................................................
;;   cpy source, target      | Copies the value from the cell amenable
;;       ******  ******      | to the {source} address into the cell
;;                           | identified by the {target} address.
;;                           |-----------------------------------------
;;                           | {source} must be an address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;   ..................................................................
;;   swp first, second       | Exchanges the values stored in the
;;       *****  ******       | cell amenable to the {first} address and
;;                           | the cell identified by the {second}
;;                           | address.
;;                           |-----------------------------------------
;;                           | {first} must be an address.
;;                           |-----------------------------------------
;;                           | {second} must be an address.
;;   ==================================================================
;;   add left, right, target | Supputates the sum of the {left} operand
;;       ****  *****  ****** | incremented by the {right} one and
;;                           | stores the result in the {target} cell.
;;                           |-----------------------------------------
;;                           | {left} must either be an integer literal
;;                           | or an address.
;;                           |-----------------------------------------
;;                           | {right} must either be an integer
;;                           | literal or an address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;   ..................................................................
;;   sub left, right, target | Supputates the different of the {left}
;;       ****  *****  ****** | operand decremented by the {right} one
;;                           | and stores the result in the {target}
;;                           | cell.
;;                           |-----------------------------------------
;;                           | {left} must either be an integer literal
;;                           | or an address.
;;                           |-----------------------------------------
;;                           | {right} must either be an integer
;;                           | literal or an address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;   ..................................................................
;;   mul left, right, target | Supputates the quotient of the {left}
;;       ****  *****  ****** | operand multiplied by the {right} one
;;                           | and stores the result in the {target}
;;                           | cell.
;;                           |-----------------------------------------
;;                           | {left} must either be an integer literal
;;                           | or an address.
;;                           |-----------------------------------------
;;                           | {right} must either be an integer
;;                           | literal or an address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;   ..................................................................
;;   div left, right, target | Supputates the integer quotient of the
;;       ****  *****  ****** | {left} operand divided by the {right}
;;                           | one and rounded down to the next smaller
;;                           | integral value, and stores the result in
;;                           | the {target} cell.
;;                           |-----------------------------------------
;;                           | {left} must either be an integer literal
;;                           | or an address.
;;                           |-----------------------------------------
;;                           | {right} must either be an integer
;;                           | literal or an address.
;;                           |-----------------------------------------
;;                           | {target} must be an address.
;;   ==================================================================
;;   :label:                 | Defines a new label identified by the
;;    *****                  | name {label}.
;;                           |-----------------------------------------
;;                           | {label} must be a label name.
;;                           |-----------------------------------------
;;                           | If a label amenable to the {label} name
;;                           | has already been defined at a prevenient
;;                           | position, an error of the type
;;                           | "DuplicateLabelError" is signaled.
;;   ..................................................................
;;   jmp guard, label        | If the value of the cell amenable to the
;;       *****  *****        | {guard} address does not equal zero (0),
;;                           | relocates the instruction pointer (IP)
;;                           | to the definition position of the label
;;                           | identified by the {label} name.
;;                           |-----------------------------------------
;;                           | {guard} must be an address.
;;                           |-----------------------------------------
;;                           | {label} must be a label name.
;;                           |-----------------------------------------
;;                           | If no label amenable to the {label} name
;;                           | can be detected, an error of the type
;;                           | "MissingLabelError" is signaled.
;;   ==================================================================
;;   snd conduit, source     | Writes the {source} value to the output
;;       *******  ******     | {conduit}'s buffer, without a
;;                           | concomitant printing of its content.
;;                           |-----------------------------------------
;;                           | {conduit} must be the output channel, or
;;                           | structure, "out"; in the case of the
;;                           | input {conduit}, "in", no epiphenomenon
;;                           | is elicited.
;;                           |-----------------------------------------
;;                           | {source} must be an address.
;;   ..................................................................
;;   mvp conduit, offset     | Advances the {conduit}'s pointer by the
;;       *******  ******     | {offset} relative to its current state.
;;                           |-----------------------------------------
;;                           | The output structure "out" advances its
;;                           | cursor automatically by one position as
;;                           | an epiphenomenal causatum accompassed by
;;                           | a "snd" request.
;;   ..................................................................
;;   cll conduit             | Invokes the conduit, or structure,
;;       *******             | {conduit}.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation ostends an effort in the
;; programming language Common Lisp, its reification that from the
;; string provenance, holding the 6673846771 source code intended for
;; its execution, into a vector of representative instruction objects
;; as a parasceve to the evaluation's ultimity.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-11-12
;; 
;; Sources:
;;   [esolang2023:6673846771]
;;   The Esolang contributors, "6673846771", September 21st, 2023
;;   URL: "https://esolangs.org/wiki/6673846771"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the types.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a singly linked list composed of zero
   or more elements, each member of which complies with the
   ELEMENT-TYPE, for thilk is stipulated the generic sentinel ``*'' as
   the default configuration."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency's
   firmament is edified upon zero or more entries, for everichon among
   these a key of the KEY-TYPE participates, allied with a value
   subsuming into the VALUE-TYPE, both assigned the generic sentinel
   ``*'' as the default configuration."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type '*) (value-type '*))
  "The ``association-list-of'' type defines an association list, or
   alist, whose conformation admits an arbitary accompt for entries,
   each such a cons whose sinistral compartment lends a commorancy to
   the key, this complying with the KEY-TYPE, and whose dextral moeity
   specifies the affiliated value, of the VALUE-TYPE, for both holds
   the generic sentinel ``*'' as a default."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype token-type ()
  "The ``token-type'' enumerates the recognized variation on token
   categories."
  '(member
    :add
    :address
    :cll
    :comma
    :cpy
    :div
    :end-of-file
    :end-of-line
    :identifier
    :in
    :integer
    :jmp
    :label-definition
    :mul
    :mvp
    :out
    :push
    :snd
    :sub
    :swp
    :undefined))

;;; -------------------------------------------------------

(deftype arithmetic-operator ()
  "The ``arithmetic-operator'' type enumerates the recognized specimens
   of binary arithmetic operators."
  '(member :add :div :mul :sub))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines a 6673846771 program as a
   one-dimensional simple array comprising zero or more ``Instruction''
   objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype label-table ()
  "The ``label-table'' type defines a unidirectional mapping betwixt a
   label name and its zero-based position into the ensconcing 6673846771
   program's instruction vector, its manifestation that of a hash table
   whose simple string keys ally with fixnum position designators."
  '(hash-table-of simple-string fixnum))

;;; -------------------------------------------------------

(deftype address ()
  "The ``address'' type defines a cell address as an unsigned integer
   number greater than or equal to zero (0)."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the 6673846771 program memory as a
   sparse association betwixt a non-negative integer address and a
   signed integer datum, its manifestation edified upon a hash table
   whose keys accommodate the cell addresses, while the values map to
   the allied cell content."
  '(hash-table-of address integer))

;;; -------------------------------------------------------

(deftype integer-buffer ()
  "The ``integer-buffer'' type defines a resizable salvatory of signed
   integer numbers, the catena's capacitation involving random-access
   by non-negative integral subscripts, and which towards the dextral
   laterality wists of no bourne to its dispansion, being realized in
   an adjustable vector of integer-valued elements."
  '(vector integer *))

;;; -------------------------------------------------------

(deftype sparse-integer-vector ()
  "The ``sparse-integer-vector'' type defines a sparse one-dimensional
   array of signed integer numbers, impounded in their mickleness by no
   natural bourne, and entalented with an amenability to non-negative
   integer subscripts, its entelechy a hash table mapping from the
   unsigned integral keys to signed integral values."
  '(hash-table-of (integer 0 *) (integer * *)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the condition types.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition 6673846771-Error (error)
  ()
  (:documentation
    "The ``6673846771-Error'' condition type serves as the common
     firmament's furnishment for all conditions pursuing the reporting
     of anomalous circumstances in the context of a 6673846771 program's
     recepetion, lexical analyzation, parsing, or execution."))

;;; -------------------------------------------------------

(define-condition Duplicate-Label-Error (6673846771-Error)
  ((name
    :initarg       :name
    :initform      (error "Missing name.")
    :reader        duplicate-label-error-name
    :type          simple-string
    :documentation "The label name whose attempted definition, maugre
                    an already extant establishment, served as this
                    erroneous circumstance's instigator."))
  (:report
    (lambda (condition stream)
      (declare (type Duplicate-Label-Error condition))
      (declare (type stream                stream))
      (format stream "The label name ~s has already been defined."
        (duplicate-label-error-name condition))))
  (:documentation
    "The ``Duplicate-Label-Error'' condition type serves in the apprizal
     about the attempt to define a label by an identifier already
     registered for such purpose."))

;;; -------------------------------------------------------

(define-condition Missing-Label-Error (6673846771-Error)
  ((name
    :initarg       :name
    :initform      (error "Missing name.")
    :reader        missing-label-error-name
    :type          simple-string
    :documentation "The label name whose attempted visitation, maugre
                    its definition's lacuna, served as this erroneous
                    circumstance's instigator."))
  (:report
    (lambda (condition stream)
      (declare (type Missing-Label-Error condition))
      (declare (type stream              stream))
      (format stream "No label amenable to the name ~s could be ~
                      detected."
        (missing-label-error-name condition))))
  (:documentation
    "The ``Missing-Label-Error'' condition type serves in the apprizal
     about the attempt to visit a label whose definition has never been
     established in the first place."))

;;; -------------------------------------------------------

(define-condition Invalid-Address-Error (6673846771-Error)
  ((pseudo-address
    :initarg       :pseudo-address
    :initform      (error "Missing pseudo-address.")
    :reader        invalid-address-error-pseudo-address
    :type          integer
    :documentation "The integral object whose inconcinnity with a cell
                    address' stipulations served to instigate this
                    anomalous circumstance."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Address-Error condition))
      (declare (type stream                stream))
      (format stream "The identifier ~d cannot be naited as an address."
        (invalid-address-error-pseudo-address condition))))
  (:documentation
    "The ``Invalid-Address-Error'' condition type serves in the apprizal
     about the attempt at an invalid address' employment for a cell's
     retrieval."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-to-a-boolean-value (object)
  "Interprets the OBJECT in its facette as a \"generalized boolean\"
   and produces a veridicous Boolean tantamount thereof, responding for
   a non-``NIL'' input with a ``boolean'' value of ``T''; otherwise,
   for a ``NIL'' OBJECT, delivers the ``NIL'' sentinel itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the linked-based based table.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Linked-Table
  (:constructor
    prepare-an-empty-linked-table (&key (entries  NIL)
                                        (key-test #'eql))))
  "The ``Linked-Table'' class applies itself to the representation of
   an associated data structure edified upon the hypostasis of a singly
   linked list."
  (entries  NIL   :type (association-list-of * *) :read-only NIL)
  (key-test #'eql :type (function (* *) *)        :read-only T))

;;; -------------------------------------------------------

(defun make-a-linked-table-from-a-property-list
    (initial-keys-and-values
     &optional (key-test #'eql))
  "Creates and returns a fresh ``Linked-Table'' from the property
   list INITIAL-KEYS-AND-VALUES, optionally configured to employ in its
   keys' equiparation process the KEY-TEST."
  (declare (type (list-of T) initial-keys-and-values))
  (the Linked-Table
    (prepare-an-empty-linked-table
      :entries
        (loop
          for (current-key current-value)
            of-type (T T)
            on      initial-keys-and-values
            by      #'cddr
          collect
            (cons current-key current-value))
      :key-test key-test)))

;;; -------------------------------------------------------

(defun look-up-the-entry-for (table key)
  "Returns the entry cons associated with the KEY in the linked TABLE;
   or, upon its disrespondency, produces the ``NIL'' sentinel."
  (declare (type Linked-Table table))
  (declare (type T            key))
  (the (or null (cons * *))
    (assoc key
      (linked-table-entries table)
      :test (linked-table-key-test table))))

;;; -------------------------------------------------------

(defun look-up-the-value-for (table key &optional (default NIL))
  "Queries the value allied with the KEY in the linked TABLE and
   returns two values:
     (1) If the KEY could be detected in the TABLE, the affiliated
         value, otherwise the DEFAULT object.
     (2) If the KEY could be detected in the TABLE, a ``boolean'' value
         of ``T'', otherwise ``NIL''."
  (declare (type Linked-Table table))
  (declare (type T            key))
  (declare (type T            default))
  (let ((associated-entry (look-up-the-entry-for table key)))
    (declare (type (or null (cons * *)) associated-entry))
    (the (values T boolean)
      (values
        (or (cdr associated-entry)
            default)
        (resolve-to-a-boolean-value associated-entry)))))

;;; -------------------------------------------------------

(defun insert-an-entry (table key value)
  "Associates the KEY with the VALUE in the linked TABLE and returns no
   value.
   ---
   Please heed that any entry already affiliated with the KEY will be
   tacitly superseded by the new KEY-VALUE alliance."
  (declare (type Linked-Table table))
  (declare (type T            key))
  (declare (type T            value))
  (let ((extant-entry (look-up-the-entry-for table key)))
    (declare (type (or null (cons * *)) extant-entry))
    (if extant-entry
      (setf (cdr extant-entry) value)
      (push (cons key value)
        (linked-table-entries table))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun character-designates-a-space-p (candidate)
  "Determines whether the CANDIDATE represents a spacing character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (member candidate '(9 11 32)
        :key  #'code-char
        :test #'char=))))

;;; -------------------------------------------------------

(defun character-designates-an-identifier-p (candidate)
  "Determines whether the CANDIDATE represents a constituent admissive
   to the edification of an identifier name, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (and
        (graphic-char-p candidate)
        (not (character-designates-a-space-p candidate))
        (not (find candidate ",:$" :test #'char=))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Returns a simple string representation of the SOURCE, either
   producing a fresh object adhering to the aforementioned type, or,
   upon its extant subsumption into thilk, returns the SOURCE in its
   verbatim unmodified form."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (resolve-to-a-boolean-value
      (zerop
        (length source)))))

;;; -------------------------------------------------------

(defun string-starts-with-a-sign-p (source)
  "Determines whether the SOURCE string commences with an arithmetic
   sign, the membership of which emerges from the diorism admitting the
   twissel \"+\" and \"-\", returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type simple-string source))
  (the boolean
    (resolve-to-a-boolean-value
      (and
        (plusp (length source))
        (find (schar source 0) "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun separate-the-numeric-and-textual-parts (word)
  "Extracts from the WORD the purely numeric and the contingent
   subsequent tmemata and returns two values:
     (1) The parsed unsigned integer part at the WORD's front, if
         extant; otherwise the ``NIL'' sentinel is produced.
     (2) A fresh simple string comprehending all characters in the WORD
         succeeding the prevenient numeric part (see -> return value
         (1)). This may be an empty string, if the numeric content
         dispands across the intext's entirety."
  (declare (type simple-string word))
  (let ((end-index-of-numeric-tmema
          (or (position-if-not #'digit-char-p word
                :start (if (string-starts-with-a-sign-p word) 1 0))
              (length word))))
    (declare (type fixnum end-index-of-numeric-tmema))
    (the (values (or null integer) simple-string)
      (values
        (ignore-errors
          (parse-integer word
            :start 0
            :end   end-index-of-numeric-tmema))
        (subseq word end-index-of-numeric-tmema)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label name operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun valid-label-name-p (candidate)
  "Determines whether the CANDIDATE represents a valid name for a label,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type simple-string candidate))
  (the boolean
    (resolve-to-a-boolean-value
      (every #'character-designates-an-identifier-p candidate))))

;;; -------------------------------------------------------

(defun validate-the-label-name (candidate)
  "Determines whether the CANDIDATE represents a valid name for a label,
   on confirmation returning the CANDIDATE in its ipsissima verba form;
   otherwise signals an error of an unspecified type."
  (declare (type simple-string candidate))
  (the simple-string
    (if (valid-label-name-p candidate)
      candidate
      (error "The identifier ~s does not designate a valid label name."
        candidate))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the address operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun validate-the-address (contingent-address)
  "Determines whether the CONTINGENT-ADDRESS represents a veridicous
   address, by constituting an incolant of the non-negative integral
   species, on confirmation returning the CONTINGENT-ADDRESS in its
   ipsissima verba constitution; otherwise an error of an unspecified
   type is signaled."
  (declare (type integer contingent-address))
  (the address
    (if (minusp contingent-address)
      (error 'Invalid-Address-Error :pseudo-address contingent-address)
      contingent-address)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the identifier table.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Linked-Table +IDENTIFIERS+))

;;; -------------------------------------------------------

(defparameter +IDENTIFIERS+
  (make-a-linked-table-from-a-property-list
    '("add"  :add
      "cll"  :cll
      "cpy"  :cpy
      "div"  :div
      "in"   :in
      "jmp"  :jmp
      "mul"  :mul
      "mvp"  :mvp
      "out"  :out
      "push" :push
      "snd"  :snd
      "sub"  :sub
      "swp"  :swp)
    #'string=)
  "Associated the recognized 6673846771 language keyword names with
   representative token types.")

;;; -------------------------------------------------------

(defun look-up-the-identifier (identifier)
  "Attempts to return the token type allied with the IDENTIFIER name;
   or, upon its disrespondency, replies with the Procrustean
   ``:identifier'' signification."
  (declare (type simple-string identifier))
  (the token-type
    (look-up-the-value-for +IDENTIFIERS+ identifier :identifier)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the line scanner.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type simple-string *current-line*))
(declaim (type fixnum        *current-line-length*))
(declaim (type fixnum        *current-column*))
(declaim (type character     *current-character*))
(declaim (type boolean       *current-line-is-exhausted-p*))
(declaim (type boolean       *source-is-exhausted-p*))
(declaim (type token-type    *current-token-type*))
(declaim (type T             *current-token-value*))
(declaim (type fixnum        *current-token-start-point*))
(declaim (type fixnum        *current-token-end-point*))
(declaim (type simple-string *current-token-substring*))

;;; -------------------------------------------------------

(defparameter *current-line* ""
  "The currently processed 6673846771 source code line.")

(define-symbol-macro *current-line-length*
  (the fixnum
    (length *current-line*)))

(defparameter *current-column* 0
  "The zero-based current column into the *CURRENT-LINE*.")

(define-symbol-macro *current-character*
  (the character
    (schar *current-line* *current-column*)))

(define-symbol-macro *current-line-is-exhausted-p*
  (the boolean
    (not (array-in-bounds-p *current-line* *current-column*))))

(defparameter *source-is-exhausted-p* NIL
  "A Boolean flag which determines whether the furnished piece of
   6673846771 source code has been processed in its entirety.")

(defparameter *current-token-type* :undefined
  "The most recently extracted token's categorizing type.")

(defparameter *current-token-value* ""
  "The most recently extracted token's parsed value.")

(defparameter *current-token-start-point* 0
  "The inclusive zero-based position into the *CURRENT-LINE* at which
   the *CURRENT-TOKEN* commences.")

(defparameter *current-token-end-point* 0
  "The exclusive zero-based position into the *CURRENT-LINE* immediately
   before which the *CURRENT-TOKEN* terminates.")

(define-symbol-macro *current-token-substring*
  (the simple-string
    (subseq
      *current-line*
      *current-token-start-point*
      *current-token-end-point*)))

;;; -------------------------------------------------------

(defun skip-accolent-spaces ()
  "Proceeding from the contemporaneously occupied position into the
   *CURRENT-LINE*, skips a catena encompassing zero or more accolent
   spaces and returns no value."
  (setf *current-column*
    (or (position-if-not
          #'character-designates-a-space-p
          *current-line*
          :start *current-column*)
        *current-line-length*))
  (values))

;;; -------------------------------------------------------

(defun relocate-to-the-end-of-the-current-line ()
  "Relocates the position cursor to the end of the *CURRENT-LINE* and
   returns no value."
  (psetf
    *current-column*            *current-line-length*
    *current-token-type*        :end-of-line
    *current-token-value*       ""
    *current-token-start-point* *current-line-length*
    *current-token-end-point*   *current-line-length*)
  (values))

;;; -------------------------------------------------------

(defun skip-spaces-and-comments ()
  "Proceeding from the contemporaneously occupied position into the
   *CURRENT-LINE*, skips a catena encompassing zero or more accolent
   spaces, as well as any contingent commentary section, and returns no
   value."
  (skip-accolent-spaces)
  (when (and (not   *current-line-is-exhausted-p*)
             (char= *current-character* #\$))
    (relocate-to-the-end-of-the-current-line))
  (values))

;;; -------------------------------------------------------

(defun reset-the-line-scanner ()
  "Restores the line scanner's state variables to their pristine
   configurations and returns no value."
  (psetf
    *current-line*              ""
    *current-column*            0
    *source-is-exhausted-p*     NIL
    *current-token-type*        :undefined
    *current-token-value*       ""
    *current-token-start-point* 0
    *current-token-end-point*   0)
  (skip-spaces-and-comments)
  (values))

;;; -------------------------------------------------------

(defun read-the-next-line (source)
  "Reads the next line from the SOURCE stream utilizing the line
   scanner, if possible, and returns, in the case of this operation's
   application on a non-exhausted SOURCE, a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type string-stream source))
  (let ((next-line (read-line source NIL NIL)))
    (declare (type (or null string) next-line))
    (psetf
      *current-line*
        (if next-line
          (convert-into-a-simple-string next-line)
          "")
      *current-column*            0
      *current-token-type*        (if next-line :undefined :end-of-file)
      *current-token-value*       ""
      *current-token-start-point* 0
      *current-token-end-point*   0
      *source-is-exhausted-p*     (null next-line)))
  (the boolean
    (not *source-is-exhausted-p*)))

;;; -------------------------------------------------------

(defun extract-the-next-word ()
  "Proceeding from the current position into the *CURRENT-LINE*,
   extracts the next identifier, configures the token data accordingly,
   and returns no value."
  (skip-spaces-and-comments)
  (psetf
    *current-token-start-point* *current-column*
    *current-token-end-point*
      (or (position-if-not
            #'character-designates-an-identifier-p
            *current-line*
            :start *current-column*)
          *current-line-length*)
    *current-token-type* :undefined)
  (setf *current-column* *current-token-end-point*)
  (values))

;;; -------------------------------------------------------

(defun read-a-comma ()
  "Proceeding from the current position into the *CURRENT-LINE*, reads
   a comma (\",\"), configures the token data accordingly, and returns
   no value."
  (psetf
    *current-token-type*        :comma
    *current-token-value*       *current-character*
    *current-token-start-point* *current-column*
    *current-token-end-point*   (1+ *current-column*))
  (incf *current-column*)
  (values))

;;; -------------------------------------------------------

(defun read-a-potential-address-or-integer-literal ()
  "Attempts to interpret the current token either as an address, or,
   upon its disrespondency, as a signed or unsigned integer literal,
   resorting in its ultimity, in the case of both trials' abortion, to
   an Procrustean identifier, and returns no value."
  (extract-the-next-word)
  (multiple-value-bind (numeric-part textual-part)
      (separate-the-numeric-and-textual-parts *current-token-substring*)
    (declare (type (or null integer) numeric-part))
    (declare (type simple-string     textual-part))
    (cond
      ;; Address?
      ((and numeric-part
            (string= textual-part "c"))
        (psetf
          *current-token-type*  :address
          *current-token-value*
            (validate-the-address numeric-part)))
      ;; Pure integer literal?
      ((and numeric-part
            (string-is-empty-p textual-part))
        (psetf
          *current-token-type*  :integer
          *current-token-value* numeric-part))
      ;; Either no numeric prefix, or a numeric prefix with a
      ;; non-address additament?
      (T
        (psetf
          *current-token-type*  :identifier
          *current-token-value* *current-token-substring*))))
  (values))

;;; -------------------------------------------------------

(defun read-a-label-definition ()
  "Proceeding from the current position into the *CURRENT-LINE*, reads
   a label definition, configures the token data accordingly, and
   returns no value."
  (let ((next-colon-position
          (position #\: *current-line*
            :start (1+ *current-column*)
            :test  #'char=)))
    (declare (type (or null fixnum) next-colon-position))
    (cond
      (next-colon-position
        (psetf
          *current-token-type*        :label-definition
          *current-token-start-point* *current-column*
          *current-token-end-point*   (1+ next-colon-position)
          *current-token-value*
            (validate-the-label-name
              (subseq *current-line*
                (1+ *current-column*)
                next-colon-position)))
        (setf *current-column* (1+ next-colon-position)))
      (T
        (error "Unterminated label definition starting at the ~
                position ~d."
          *current-column*))))
  (values))

;;; -------------------------------------------------------

(defun read-an-identifier ()
  "Proceeding from the current position into the *CURRENT-LINE*, reads
   an identifier, either representing an instruction name or a general
   character sequence, configures the token data accordingly, and
   returns no value."
  (extract-the-next-word)
  (psetf
    *current-token-type*
      (look-up-the-identifier *current-token-substring*)
    *current-token-value* *current-token-substring*)
  (values))

;;; -------------------------------------------------------

(defun extract-the-next-token ()
  "Proceeding from the current position into the *CURRENT-LINE*,
   extracts the next following token, configures the lexer's state
   accordingly, and returns no value."
  (skip-spaces-and-comments)
  (cond
    ;; End of line or start of comment?
    ((or *current-line-is-exhausted-p*
         (char= *current-character* #\$))
      (relocate-to-the-end-of-the-current-line))
    
    ;; Operand merist (semicolon)?
    ((char= *current-character* #\,)
      (read-a-comma))
    
    ;; Label definition?
    ((char= *current-character* #\:)
      (read-a-label-definition))
    
    ;; Integer literal, address, or identifier commencing with a digit?
    ((digit-char-p *current-character*)
      (read-a-potential-address-or-integer-literal))
    
    ;; Instruction name or Procrustean identifier?
    (T
      (read-an-identifier)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the instruction operands.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface's dever is delineated in a common foundry's
   edification upon which all classes are developed intending to
   represent an instruction operand, or argument, in a form ostending
   homogeneity.")

;;; -------------------------------------------------------

(defstruct (Numeric-Operand
  (:include Operand))
  "The ``Numeric-Operand'' interface serves in the establishment of a
   firmament nuncupated to all classes in a pursuit to represent a
   numeric operand, either immediately or by indirection.")

;;; -------------------------------------------------------

(defstruct (Literal-Operand
  (:include     Numeric-Operand)
  (:constructor make-literal-operand (value)))
  "The ``Literal-Operand'' class serves in the ensconcement of an
   immediate numeric operand in the guise of a signed or unsigned
   integer literal."
  (value (error "Missing literal operand value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Address-Operand
  (:include     Numeric-Operand)
  (:constructor make-address-operand (index)))
  "The ``Address-Operand'' class serves in the ensconcement of an
   indirect numeric operand in the guise of an address reference."
  (index (error "Missing address operand index.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Conduit-Operand
  (:include     Operand)
  (:constructor make-conduit-operand (direction)))
  "The ``Conduit-Operand'' class serves in the representation of a
   communication conduit, or \"structure\" as an operand."
  (direction (error "Missing conduit operand direction.")
             :type      (member :input :output)
             :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Name-Operand
  (:include     Operand)
  (:constructor make-label-name-operand (name)))
  "The ``Label-Name'' class serves in the representation of a label name
   as an operand."
  (name (error "Missing label name.")
        :type      string
        :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the instructions.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' interface serves as the firmament to all classes
   to whom the dever of a 6673846771 instruction's representation is
   allotted.")

;;; -------------------------------------------------------

(defstruct (Arithmetic-Instruction
  (:include Instruction))
  "The ``Arithmetic-Instruction'' class applies itself to the dever of
   a 6673846771 instruction's encapsulation appertaining to an binary
   arithmetic operation's adhibition, the reification of which may be
   retrieved in the \"add\", \"div\", \"mul\", or \"sub\" operations."
  (operator      (error "Missing operator.")
                 :type      arithmetic-operator
                 :read-only T)
  (left-operand  (error "Missing left operand.")
                 :type      Numeric-Operand
                 :read-only T)
  (right-operand (error "Missing right operand.")
                 :type      Numeric-Operand
                 :read-only T)
  (target        (error "Missing target.")
                 :type      Address-Operand
                 :read-only T))

;;; -------------------------------------------------------

(defstruct (Push-Instruction
  (:include Instruction))
  "The ``Push-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to an immediately
   or indirectly specified datum's assignment to a cell, the reification
   of which may be retrieved in the \"push\" operation."
  (target (error "Missing target.")
          :type      Address-Operand
          :read-only T)
  (source (error "Missing source.")
          :type      Numeric-Operand
          :read-only T))

;;; -------------------------------------------------------

(defstruct (Label-Definition-Instruction
  (:include Instruction))
  "The ``Label-Definition-Instruction'' class applies itself to the
   dever of a 6673846771 instruction's encapsulation appertaining to a
   program label's definition for a future navigation's contingency, the
   reification of which may be retrieved in the \":label:\" operation."
  (name (error "Missing label name to define.")
        :type      Label-Name-Operand
        :read-only T))

;;; -------------------------------------------------------

(defstruct (Jmp-Instruction
  (:include Instruction))
  "The ``Jmp-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to a conditional
   navigation to an optated label definition's position, the
   reification of which may be retrieved in the \"jmp\" operation."
  (guard (error "Missing guard.")
         :type      Address-Operand
         :read-only T)
  (label (error "Missing label.")
         :type      Label-Name-Operand
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Snd-Instruction
  (:include Instruction))
  "The ``Snd-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to a buffering
   communication attempt with a conduit, the reification of which may
   be retrieved in the \"snd\" operation."
  (conduit (error "Missing conduit.")
           :type      Conduit-Operand
           :read-only T)
  (message (error "Missing message.")
           :type      Address-Operand
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Mvp-Instruction
  (:include Instruction))
  "The ``Mvp-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to a motion's
   actuation on a communication conduit, the reification of which may
   be retrieved in the \"mvp\" operation."
  (conduit (error "Missing conduit.")
           :type      Conduit-Operand
           :read-only T)
  (offset  (error "Missing offset.")
           :type      Numeric-Operand
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Cll-Instruction
  (:include Instruction))
  "The ``Cll-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to a
   communication conduit's activation, the reification of which may be
   retrieved in the \"cll\" operation."
  (conduit (error "Missing conduit.")
           :type      Conduit-Operand
           :read-only T))

;;; -------------------------------------------------------

(defstruct (Swp-Instruction
  (:include Instruction))
  "The ``Swp-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to the
   cambistry's application on two memory cells' contents, the
   reification of which may be retrieved in the \"swp\" operation."
  (first-address  (error "Missing first address.")
                  :type      Address-Operand
                  :read-only T)
  (second-address (error "Missing second address.")
                  :type      Address-Operand
                  :read-only T))

;;; -------------------------------------------------------

(defstruct (Cpy-Instruction
  (:include Instruction))
  "The ``Cpy-Instruction'' class applies itself to the dever of a
   6673846771 instruction's encapsulation appertaining to a transfer
   from one memory cell to another instant, the reification of which
   may be retrieved in the \"cpy\" operation."
  (source (error "Missing first address.")
          :type      Address-Operand
          :read-only T)
  (target (error "Missing second address.")
          :type      Address-Operand
          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-an-empty-program ()
  "Creates and returns an empty 6673846771 ``program''."
  (the program
    (coerce NIL
      '(simple-array Instruction (*)))))

;;; -------------------------------------------------------

(defun assemble-a-program-from (instructions)
  "Creates and returns a fresh ``program'' representation comprehending
   6673846771 INSTRUCTIONS."
  (declare (type (list-of Instruction) instructions))
  (the program
    (coerce instructions
      '(simple-array Instruction (*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the parser.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expect-a-comma ()
  "Requests the next token, expecting thilk to represent a comma,
   returning on confirmation no value; otherwise signal an error of an
   unspecified type."
  (extract-the-next-token)
  (unless (eq *current-token-type* :comma)
    (error "Expected a comma token, but encountered the type ~s."
      *current-token-type*))
  (values))

;;; -------------------------------------------------------

(defun parse-an-address (&key (expects-comma-p NIL))
  "Requests the next token, contingently preceded by the prevenient
   one's consumption as a comma, if EXPECTS-COMMA-P amounts to ``T'',
   parses the obtained token as an address, and returns a connable
   ``Address-Operand'' representation thereof."
  (declare (type boolean expects-comma-p))
  (when expects-comma-p
    (expect-a-comma))
  (extract-the-next-token)
  (the Address-Operand
    (if (eq *current-token-type* :address)
      (make-address-operand *current-token-value*)
      (error "Expected a token of the type ``:address', but ~
              encountered ~s."
        *current-token-type*))))

;;; -------------------------------------------------------

(defun parse-a-numeric-operand (&key (expects-comma-p NIL))
  "Requests the next token, contingently preceded by the prevenient
   one's consumption as a comma, if EXPECTS-COMMA-P amounts to ``T'',
   parses the obtained token as a numeric operand, the diorism
   affiliated to whose parcery serves to subsume both an address or an
   integral literal, and returns a connable ``Numeric-Operand''
   representation thereof."
  (declare (type boolean expects-comma-p))
  (when expects-comma-p
    (expect-a-comma))
  (extract-the-next-token)
  (the Numeric-Operand
    (case *current-token-type*
      (:address
        (make-address-operand *current-token-value*))
      (:integer
        (make-literal-operand *current-token-value*))
      (otherwise
        (error "Expected either an address or integer literal token, ~
                but encountered the type ~s."
          *current-token-type*)))))

;;; -------------------------------------------------------

(defun parse-a-conduit (&key (expects-comma-p NIL))
  "Requests the next token, contingently preceded by the prevenient
   one's consumption as a comma, if EXPECTS-COMMA-P amounts to ``T'',
   parses the obtained token as a conduit signification, and returns a
   connable ``Conduit-Operand'' representation thereof."
  (declare (type boolean expects-comma-p))
  (when expects-comma-p
    (expect-a-comma))
  (extract-the-next-token)
  (the Conduit-Operand
    (case *current-token-type*
      (:in  (make-conduit-operand :input))
      (:out (make-conduit-operand :output))
      (otherwise
        (error "Expected a conduit token, but encountered the type ~s."
          *current-token-type*)))))

;;; -------------------------------------------------------

(defun parse-the-binary-operands ()
  "Requests the five subsequent tokens and parses the incipient and
   third ones as numeric operands, the desinent specimen as an address,
   their vincula a comma token inwith each interstice, and returns
   three values:
     (1) The left  operand     as a  ``Numeric-Operand'' object.
     (2) The right operand     as a  ``Numeric-Operand'' object.
     (3) The assignment target as an ``Address-Operand'' object."
  (the (values Numeric-Operand Numeric-Operand Address-Operand)
    (values
      (parse-a-numeric-operand)
      (parse-a-numeric-operand :expects-comma-p T)
      (parse-an-address        :expects-comma-p T))))

;;; -------------------------------------------------------

(defun parse-an-address-twissel ()
  "Requests the three subsequent tokens and parses the incipient and
   desinent ones as addresses, their merist an aefauld comma token, and
   returns two values:
     (1) The first  address as an ``Address-Operand''.
     (2) The second address as an ``Address-Operand''."
  (the (values Address-Operand Address-Operand)
    (values
      (parse-an-address)
      (parse-an-address :expects-comma-p T))))

;;; -------------------------------------------------------

(defun parse-a-label-name (&key (expects-comma-p NIL))
  "Requests the next token, contingently preceded by the prevenient
   one's consumption as a comma, if EXPECTS-COMMA-P amounts to ``T'',
   parses the obtained token as a label name, and returns a connable
   ``Label-Name-Operand'' representation thereof."
  (declare (type boolean expects-comma-p))
  (when expects-comma-p
    (expect-a-comma))
  (extract-the-next-token)
  (the Label-Name-Operand
    (make-label-name-operand
      (validate-the-label-name *current-token-substring*))))

;;; -------------------------------------------------------

(defun expect-the-end-of-the-line ()
  "Requests the next token, expecting thilk to designate no further
   content, that is, producing the end-of-line signification, returning
   on confirmation no value; otherwise an error of an unspecified type
   will be signaled.
   ---
   This operation's utility should be naited as a sequela to a parsed
   instruction's conclusion, serving as a vouch for the desinence's
   carency of any invalid additaments."
  (extract-the-next-token)
  (unless (eq *current-token-type* :end-of-line)
    (error "Expected the line to terminate, but encountered a ~
            token of the type ~s with the value ~s, inchoant at the ~
            position ~d."
      *current-token-type*
      *current-token-value*
      *current-token-start-point*))
  (values))

;;; -------------------------------------------------------

(defun parse-an-instruction ()
  "Parses a 6673846771 instruction and returns a connable
   ``Instruction'' representation thereof, or, upon the current line's
   vacancy, responds with the ``NIL'' sentinel."
  (extract-the-next-token)
  (the (or null Instruction)
    (prog1
      (case *current-token-type*
        ((:end-of-line :end-of-file)
          NIL)
        (:push
          (make-push-instruction
            :target (parse-an-address)
            :source (parse-a-numeric-operand :expects-comma-p T)))
        (:cpy
          (multiple-value-bind (first-address second-address)
              (parse-an-address-twissel)
            (declare (type Address-Operand first-address))
            (declare (type Address-Operand second-address))
            (make-cpy-instruction
              :source first-address
              :target second-address)))
        (:swp
          (multiple-value-bind (first-address second-address)
              (parse-an-address-twissel)
            (declare (type Address-Operand first-address))
            (declare (type Address-Operand second-address))
            (make-swp-instruction
              :first-address  first-address
              :second-address second-address)))
        ((:add :div :mul :sub)
          (let ((operator-type *current-token-type*))
            (declare (type token-type operator-type))
            (multiple-value-bind (left-operand right-operand target)
                (parse-the-binary-operands)
              (declare (type Numeric-Operand left-operand))
              (declare (type Numeric-Operand right-operand))
              (declare (type Address-Operand target))
              (make-arithmetic-instruction
                :operator      operator-type
                :left-operand  left-operand
                :right-operand right-operand
                :target        target))))
        (:label-definition
          (make-label-definition-instruction
            :name (make-label-name-operand *current-token-value*)))
        (:jmp
          (make-jmp-instruction
            :guard (parse-an-address)
            :label (parse-a-label-name :expects-comma-p T)))
        (:snd
          (make-snd-instruction
            :conduit (parse-a-conduit)
            :message (parse-an-address :expects-comma-p T)))
        (:mvp
          (make-mvp-instruction
            :conduit (parse-a-conduit)
            :offset  (parse-a-numeric-operand :expects-comma-p T)))
        (:cll
          (make-cll-instruction :conduit (parse-a-conduit)))
        (otherwise
          (error "Non-instruction token with type ~s and value ~s."
            *current-token-type* *current-token-value*)))
      (expect-the-end-of-the-line))))

;;; -------------------------------------------------------

(defun parse-the-program (input-stream)
  "Parses the lines supplied by the INPUT-STREAM, gathers the amplected
   instructions, and returns a ``program'' representation of the
   extracted operations in their encountered arrangement."
  (declare (type string-stream input-stream))
  (the program
    (assemble-a-program-from
      (loop
        do    (read-the-next-line input-stream)
        until *source-is-exhausted-p*
        append
          (let ((next-instruction (parse-an-instruction)))
            (declare (type (or null Instruction) next-instruction))
            (when next-instruction
              (list next-instruction)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the label table.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-label-table ()
  "Creates and returns a fresh ``lable-table'' which at its inchoacy
   ostends a vacant constitution."
  (the label-table
    (make-hash-table :test #'equal)))

;;; -------------------------------------------------------

(defun label-name-is-registered-p (labels name)
  "Determines whether the labels TABLE entails an entry amenable to the
   label NAME, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type label-table   labels))
  (declare (type simple-string name))
  (the boolean
    (resolve-to-a-boolean-value
      (nth-value 1
        (gethash name labels)))))

;;; -------------------------------------------------------

(defun register-the-label (labels instruction position)
  "Extracts from the label definition INSTRUCTION the ensconced label
   name, affiliates thilk with the definition's zero-based POSITION into
   the entailing program in the LABELS table, and returns no value.
   ---
   Upon an entry's presence amenable to the label name, an error of the
   type ``Duplicate-Label-Error'' is signaled."
  (declare (type label-table                  labels))
  (declare (type Label-Definition-Instruction instruction))
  (declare (type fixnum                       position))
  (let ((label-name
          (label-name-operand-name
            (label-definition-instruction-name instruction))))
    (declare (type simple-string label-name))
    (if (label-name-is-registered-p labels label-name)
      (error 'Duplicate-Label-Error :name label-name)
      (setf (gethash label-name labels) position)))
  (values))

;;; -------------------------------------------------------

(defun collect-the-labels-in (program)
  "Creates and returns a fresh ``label-table'' whose entries are desumed
   from the 6673846771 PROGRAM's statements."
  (declare (type program program))
  (let ((labels (prepare-an-empty-label-table)))
    (declare (type label-table labels))
    (loop
      for current-instruction
        of-type Instruction
        across  program
      and current-position
        of-type fixnum
        from    0
        by      1
      when (label-definition-instruction-p current-instruction) do
        (register-the-label
          labels
          current-instruction
          current-position))
    (the label-table labels)))

;;; -------------------------------------------------------

(defun locate-the-label (labels name)
  "Returns the zero-based line number associated with the label NAME as
   registered at the LABELS table; or, upon its disrespondency, signals
   an error of the type ``Missing-Label-Error''."
  (declare (type label-table   labels))
  (declare (type simple-string name))
  (the fixnum
    (if (label-name-is-registered-p labels name)
      (gethash name labels)
      (error 'Missing-Label-Error :name name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (integer 9999 9999) +INPUT-POINTER-CELL-ADDRESS+))

;;; -------------------------------------------------------

(defparameter +INPUT-POINTER-CELL-ADDRESS+ 9999
  "Maintains the address of the memory cell dedicated to the storage
   of the cell address into which the next user input response shall be
   transmitted.")

;;; -------------------------------------------------------

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``memory'' object whose cells at this
   state of inchoacy assume in their entirety the default zero (0)
   value."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun cell-value-at (memory address)
  "Returns the value stored in the MEMORY cell amenable to the ADDRESS."
  (declare (type memory  memory))
  (declare (type address address))
  (the integer
    (gethash address memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the ADDRESS and
   returns no value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type address address))
  (setf (gethash address memory 0) new-value)
  (values))

;;; -------------------------------------------------------

(defun query-the-input-cell-address (memory)
  "Returns the address of the MEMORY cell contemporaneously intended for
   the reception of user inputs."
  (declare (type memory memory))
  (the integer
    (cell-value-at memory +INPUT-POINTER-CELL-ADDRESS+)))

;;; -------------------------------------------------------

(defun reset-the-memory (memory)
  "Resets the MEMORY's cells to their incipial state of zero (0) and
   returns the modifid MEMORY."
  (declare (type memory memory))
  (the memory
    (clrhash memory)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input and output operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun request-an-input ()
  "Displays a prompt message on the standard output conduit, queries
   the standard input channel for a signed or unsigned integer number,
   and returns the parsed response."
  (format T "~&>> ")
  (finish-output)
  (the integer
    (prog1
      (parse-integer
        (read-line NIL NIL "0"))
      (clear-input))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the integer buffer operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-integer-buffer ()
  "Creates and returns an integer buffer which at this point of inchoacy
   remains destitute of any content."
  (the integer-buffer
    (make-array 0
      :element-type    'integer
      :initial-element 0
      :adjustable      T
      :fill-pointer    T)))

;;; -------------------------------------------------------

(defun accommodate-space-for-the-integer-buffer-index (buffer
                                                       optated-index)
  "Ensures that the integer BUFFER's size metes at least a sufficient
   mickleness homologating the zero-based OPTATED-INDEX's accommodation
   and returns the contingently modified BUFFER."
  (declare (type integer-buffer buffer))
  (declare (type integer        optated-index))
  (the integer-buffer
    (loop
      while   (<= (fill-pointer buffer) optated-index)
      do      (vector-push-extend 0 buffer)
      finally (return buffer))))

;;; -------------------------------------------------------

(defun request-the-integer-buffer-element-at (buffer index)
  "Returns the integer value stored in the integer BUFFER amenable to
   the zero-based INDEX, contingently resizing the BUFFER in a
   sufficient mete so as to accommodate the INDEX."
  (declare (type integer-buffer buffer))
  (declare (type integer        index))
  (accommodate-space-for-the-integer-buffer-index buffer index)
  (the integer
    (aref buffer index)))

;;; -------------------------------------------------------

(defun set-the-integer-buffer-element-at (buffer index new-value)
  "Stores the NEW-VALUE in the integer BUFFER position amenable to the
   zero-based INDEX and returns no value."
  (declare (type integer-buffer buffer))
  (declare (type integer        index))
  (declare (type integer        new-value))
  (accommodate-space-for-the-integer-buffer-index buffer index)
  (setf (aref buffer index) new-value)
  (values))

;;; -------------------------------------------------------

(defun request-the-integer-buffer-size (buffer)
  "Returns the integer BUFFER's size."
  (declare (type integer-buffer buffer))
  (the fixnum
    (fill-pointer buffer)))

;;; -------------------------------------------------------

(defun clear-the-integer-buffer (buffer)
  "Purges the integer BUFFER and returns no value."
  (declare (type integer-buffer buffer))
  (setf (fill-pointer buffer) 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input buffer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Buffer ()
  ((elements
    :initform      (make-hash-table :test #'eql)
    :type          sparse-integer-vector
    :documentation "A hash-table-based sparse vector of signed
                    integer-valued cells, the pristine entries of which
                    assume the ``NIL'' sentinel."))
  (:documentation
    "The ``Input-Buffer'' class serves in the establishment of a
     salvatory dedicated to the castaldy of signed integer elements,
     its capacitation for adits ensuing from non-negative integral
     indices."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-input-buffer ()
  "Creates and returns an initially empty ``Input-Buffer''."
  (the Input-Buffer
    (make-instance 'Input-Buffer)))

;;; -------------------------------------------------------

(defun input-buffer-element-is-defined-at-p (buffer index)
  "Determines whether the input BUFFER contains an explicitly set
   element at the zero-based INDEX, returning on confirmation a
   ``boolean'' of ``T'', otherwise ``NIL''."
  (declare (type Input-Buffer  buffer))
  (declare (type (integer 0 *) index))
  (the boolean
    (resolve-to-a-boolean-value
      (nth-value 1
        (gethash index
          (slot-value buffer 'elements))))))

;;; -------------------------------------------------------

(defun request-the-input-buffer-element-at (buffer index)
  "Returns the input BUFFER element located at the zero-based INDEX, or,
   upon its lacuna, responds with the default value of zero (0)."
  (declare (type Input-Buffer  buffer))
  (declare (type (integer 0 *) index))
  (the (integer * *)
    (with-slots (elements) buffer
      (declare (type sparse-integer-vector elements))
      (gethash index elements 0))))

;;; -------------------------------------------------------

(defun set-the-input-buffer-element-at (buffer index new-value)
  "Stores the NEW-VALUE int the input BUFFER at the zero-based INDEX
   and returns no value."
  (declare (type Input-Buffer  buffer))
  (declare (type (integer 0 *) index))
  (declare (type (integer * *) new-value))
  (with-slots (elements) buffer
    (declare (type sparse-integer-vector elements))
    (setf (gethash index elements 0) new-value))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the "Conduit" interface.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Conduit ()
  ()
  (:documentation
    "The ``Conduit'' interface establishes the common foundry
     entreparted by all entities whose telos constitutes an incolant
     in the representation of a communication conduit, or \"structure\",
     as the terminology naited by the 6673846771 programming
     language."))

;;; -------------------------------------------------------

(defgeneric send-a-message-to-the-conduit (conduit message)
  (:documentation
    "Transmits the MESSAGE to the CONDUIT and returns no value.
     ---
     A conduit whose capacitation does not incorporate the expediency
     for such a communication enjoys the permission of this trial's
     tacit neglect.
     ---
     This operation equiparates with the 6673846771 instruction
     \"snd\"."))

;;; -------------------------------------------------------

(defgeneric move-the-conduit-pointer (conduit offset)
  (:documentation
    "Moves the CONDUIT's pointer by the OFFSET relative to its
     contemporaneous configuration and returns no value.
     ---
     A conduit whose capacitation does not incorporate the expediency
     for such a motation enjoys the permission of this trial's tacit
     neglect.
     ---
     This operation equiparates with the 6673846771 instruction
     \"mvp\"."))

;;; -------------------------------------------------------

(defgeneric call-the-conduit (conduit)
  (:documentation
    "Instigates the CONDUIT's actuation and returns no value.
     ---
     This operation equiparates with the 6673846771 instruction
     \"cll\"."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the "Conduit" interface.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Abstract-Conduit (Conduit)
  ((pointer
    :initform      0
    :type          integer
    :documentation "The zero-based index into the Integer BUFFER,
                    designating the currently selected element."))
  (:documentation
    "The ``Abstract-Conduit'' abstract class accoutres a common foundry
     and partial implementation for all classes implementing the
     ``Conduit'' interface, while bearing the indicium of a requisite
     structure pointer.
     ---
     As a dation of supererogation, this abstract class furnishes a
     vacuous and ineffectual implementation of the method
     ``send-a-message-to-the-conduit'', as well as a realization of the
     ``move-the-conduit-pointer'' operation, the latter's endowment
     attends to actual sensibility's chevisance. As an aefauld produce
     whose existence adnates with a carency, the ``call-the-conduit''
     generic function's entelechia merits a mentioning accompt."))

;;; -------------------------------------------------------

(defmethod send-a-message-to-the-conduit ((conduit Abstract-Conduit)
                                          (message integer))
  (declare (type Abstract-Conduit conduit))
  (declare (ignore                conduit))
  (declare (type integer          message))
  (declare (ignore                message))
  (values))

;;; -------------------------------------------------------

(defmethod move-the-conduit-pointer ((conduit Abstract-Conduit)
                                     (offset  integer))
  (declare (type Abstract-Conduit conduit))
  (declare (type integer          offset))
  (with-slots (pointer) conduit
    (declare (type integer pointer))
    (setf pointer
      (max 0
        (+ pointer offset))))
  (values))

;;; -------------------------------------------------------

(defun reset-the-conduit-pointer (conduit)
  "Resets the CONDUIT's pointer to its incipial position of zero (0)
   and returns no value."
  (declare (type Abstract-Conduit conduit))
  (setf (slot-value conduit 'pointer) 0)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the output conduit.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Output-Conduit (Abstract-Conduit)
  ((buffer
    :initform      (prepare-an-empty-integer-buffer)
    :type          integer-buffer
    :documentation "Maintains a random-access sequence of signed integer
                    values."))
  (:documentation
    "The ``Output-Conduit'' class furnishes an implementation of a
     conduit, or \"structure\", whose capacitation satisfies the dever
     of an output channel's provision, buffering received messages in
     an internal salvatory, whence the potential for a future issuance
     on the standard output conduit emerges."))

;;; -------------------------------------------------------

(defmethod send-a-message-to-the-conduit ((conduit Output-Conduit)
                                          (message integer))
  (declare (type Output-Conduit conduit))
  (declare (type integer        message))
  (with-slots (buffer pointer) conduit
    (declare (type integer-buffer buffer))
    (declare (type integer        pointer))
    (set-the-integer-buffer-element-at buffer pointer message))
  (move-the-conduit-pointer conduit 1)
  (values))

;;; -------------------------------------------------------

(defmethod call-the-conduit ((conduit Output-Conduit))
  (declare (type Output-Conduit conduit))
  (with-slots (buffer) conduit
    (declare (type integer-buffer buffer))
    (dotimes (current-index (request-the-integer-buffer-size buffer))
      (declare (type fixnum current-index))
      (format T "~c"
        (code-char
          (request-the-integer-buffer-element-at
            buffer
            current-index))))
    (clear-the-integer-buffer buffer))
  (reset-the-conduit-pointer conduit)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input conduit.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Conduit (Abstract-Conduit)
  ((buffer
    :initform      (prepare-a-pristine-input-buffer)
    :type          Input-Buffer
    :documentation "Maintains the hitherto received input numbers.")
   (memory
    :initarg       :memory
    :type          memory
    :documentation "The cell-based 6673846771 program memory whose
                    dedicated cell shall entertain the user inputs'
                    admission."))
  (:documentation
    "The ``Input-Conduit'' class serves in the furnishment of a
     communication channel, or structure, dedicated to the request,
     reception, and storage of user input, its obtention's provenance
     the standard input conduit."))

;;; -------------------------------------------------------

(defmethod call-the-conduit ((conduit Input-Conduit))
  (declare (type Input-Conduit conduit))
  (with-slots (buffer pointer memory) conduit
    (declare (type Input-Buffer  buffer))
    (declare (type (integer 0 *) pointer))
    (declare (type memory        memory))
    (let ((target-address
            (validate-the-address
              (query-the-input-cell-address memory))))
      (declare (type address target-address))
      (unless (input-buffer-element-is-defined-at-p buffer pointer)
        (set-the-input-buffer-element-at buffer pointer
          (request-an-input)))
      (setf (cell-value-at memory target-address)
        (request-the-input-buffer-element-at buffer pointer))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type program        *program*))
(declaim (type fixnum         *ip*))
(declaim (type boolean        *program-is-exhausted-p*))
(declaim (type Instruction    *current-instruction*))
(declaim (type label-table    *labels*))
(declaim (type memory         *memory*))
(declaim (type Output-Conduit *output-conduit*))
(declaim (type Input-Conduit  *input-conduit*))

;;; -------------------------------------------------------

(defparameter *program*
  (create-an-empty-program)
  "The 6673846771 program to execute in the guise of an ``Instruction''
   vector.")

(defparameter *ip* 0
  "The contemporaneously occupied instruction pointer (IP) position as
   a zero-based index into the *PROGRAM* vector.")

(define-symbol-macro *program-is-exhausted-p*
  (the boolean
    (not
      (array-in-bounds-p *program* *ip*))))

(define-symbol-macro *current-instruction*
  (the Instruction
    (aref *program* *ip*)))

(defparameter *labels*
  (prepare-an-empty-label-table)
  "A mapping from the defined label names to their zero-based positions
   into the *PROGRAM* vector.")

(defparameter *memory*
  (prepare-a-pristine-memory)
  "The 6673846771 program memory as a sparse vector of signed
   integer-valued cells, amenable to non-negative integral addresses.")

(defparameter *input-conduit*
  (make-instance 'Input-Conduit :memory *memory*)
  "The conduit, or \"structure\", responsible for the reception of user
   input.")

(defparameter *output-conduit*
  (make-instance 'Output-Conduit)
  "The conduit, or \"structure\", responsible for the issuance of
   output.")

;;; -------------------------------------------------------

(defun prepare-the-interpreter-for-a-new-program (new-program)
  "Adhibits the parasceves necessitated for the interpreter to execute
   the NEW-PROGRAM and returns no value."
  (declare (type program new-program))
  (psetf
    *program*        new-program
    *ip*             0
    *labels*         (collect-the-labels-in new-program)
    *memory*         (reset-the-memory *memory*)
    *input-conduit*  (make-instance 'Input-Conduit :memory *memory*)
    *output-conduit* (make-instance 'Output-Conduit))
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-instruction ()
  "Advances the instruction pointer (*IP*) to the next position in the
   *PROGRAM*, if possible, and returns no value."
  (setf *ip*
    (min (1+ *ip*)
      (length *program*)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-the-label (label)
  "Relocates the position of the LABEL's definition in the *PROGRAM*
   and returns no value."
  (declare (type simple-string label))
  (setf *ip*
    (locate-the-label *labels* label))
  (values))

;;; -------------------------------------------------------

(defgeneric resolve-the-operand (operand)
  (:documentation
    "Returns the concrete value ensconced or represented by the
     OPERAND.")
  
  (:method ((operand Address-Operand))
    "Returns the signed integer value under the castaldy of the memory
     cell amenable to the address OPERAND's index."
    (declare (type Address-Operand operand))
    (the integer
      (cell-value-at *memory*
        (address-operand-index operand))))
  
  (:method ((operand Conduit-Operand))
    (declare (type Conduit-Operand operand))
    (the Conduit
      (case (conduit-operand-direction operand)
        (:input  *input-conduit*)
        (:output *output-conduit*)
        (otherwise
          (error "Unrecognized conduit direction: ~s."
            (conduit-operand-direction operand))))))
  
  (:method ((operand Label-Name-Operand))
    "Returns the label name borne in the label name OPERAND."
    (declare (type Label-Name-Operand operand))
    (the simple-string
      (label-name-operand-name operand)))
  
  (:method ((operand Literal-Operand))
    "Returns the literal OPERAND's integral datum."
    (declare (type Literal-Operand operand))
    (the integer
      (literal-operand-value operand))))

;;; -------------------------------------------------------

(defgeneric write-to-the-memory-cell (address new-value)
  (:documentation
    "Stores the NEW-VALUE in the *MEMORY* cell amenable to the ADDRESS
     and returns no value.")
  
  (:method ((address   Address-Operand)
            (new-value Numeric-Operand))
    (declare (type Address-Operand address))
    (declare (type Numeric-Operand new-value))
    (write-to-the-memory-cell
      (address-operand-index address)
      (resolve-the-operand   new-value))
    (values))
  
  (:method ((address   integer)
            (new-value Numeric-Operand))
    (declare (type integer         address))
    (declare (type Numeric-Operand new-value))
    (write-to-the-memory-cell
      address
      (resolve-the-operand new-value))
    (values))
  
  (:method ((address   Address-Operand)
            (new-value integer))
    (declare (type Address-Operand address))
    (declare (type integer         new-value))
    (write-to-the-memory-cell
      (address-operand-index address)
      new-value)
    (values))
  
  (:method ((address   integer)
            (new-value integer))
    (declare (type address address))
    (declare (type integer new-value))
    (setf (cell-value-at *memory* address) new-value)
    (values)))

;;; -------------------------------------------------------

(defun obtain-the-arithmetic-function (instruction)
  "Returns the dyadic function associated with the arithmetic
   INSTRUCTION's operator."
  (declare (type Arithmetic-Instruction instruction))
  (the function
    (case (arithmetic-instruction-operator instruction)
      (:add #'+)
      (:div #'floor)
      (:mul #'*)
      (:sub #'-)
      (otherwise
        (error "Unrecognized arithmetic operator: ~s."
          (arithmetic-instruction-operator instruction))))))

;;; -------------------------------------------------------

(defun apply-the-arithmetic-instruction (instruction)
  "Applies the dyadic function associated with the arithmetic
   INSTRUCTION's operator to its operands and returns the result."
  (declare (type Arithmetic-Instruction instruction))
  (the integer
    (funcall
      (obtain-the-arithmetic-function instruction)
      (resolve-the-operand
        (arithmetic-instruction-left-operand instruction))
      (resolve-the-operand
        (arithmetic-instruction-right-operand instruction)))))

;;; -------------------------------------------------------

(defgeneric process-the-instruction (instruction)
  (:documentation
    "Processes the INSTRUCTION and returns no value.")
  
  (:method ((instruction Push-Instruction))
    (declare (type Push-Instruction instruction))
    (write-to-the-memory-cell
      (push-instruction-target instruction)
      (push-instruction-source instruction))
    (values))
  
  (:method ((instruction Arithmetic-Instruction))
    (declare (type Arithmetic-Instruction instruction))
    (write-to-the-memory-cell
      (arithmetic-instruction-target    instruction)
      (apply-the-arithmetic-instruction instruction))
    (values))
  
  (:method ((instruction Label-Definition-Instruction))
    (declare (type Label-Definition-Instruction instruction))
    (declare (ignore                            instruction))
    (values))
  
  (:method ((instruction Jmp-Instruction))
    (declare (type Jmp-Instruction instruction))
    (unless (zerop
              (resolve-the-operand
                (jmp-instruction-guard instruction)))
      (jump-to-the-label
        (resolve-the-operand
          (jmp-instruction-label instruction))))
    (values))
  
  (:method ((instruction Snd-Instruction))
    (declare (type Snd-Instruction instruction))
    (send-a-message-to-the-conduit
      (resolve-the-operand
        (snd-instruction-conduit instruction))
      (resolve-the-operand
        (snd-instruction-message instruction)))
    (values))
  
  (:method ((instruction Mvp-Instruction))
    (declare (type Mvp-Instruction instruction))
    (move-the-conduit-pointer
      (resolve-the-operand
        (mvp-instruction-conduit instruction))
      (resolve-the-operand
        (mvp-instruction-offset instruction)))
    (values))
  
  (:method ((instruction Cll-Instruction))
    (declare (type Cll-Instruction instruction))
    (call-the-conduit
      (resolve-the-operand
        (cll-instruction-conduit instruction)))
    (values))
  
  (:method ((instruction Swp-Instruction))
    (declare (type Swp-Instruction instruction))
    
    
    
    (values))
  
  (:method ((instruction Cpy-Instruction))
    (declare (type Cpy-Instruction instruction))
    (write-to-the-memory-cell
      (cpy-instruction-target instruction)
      (cpy-instruction-source instruction))
    (values)))

;;; -------------------------------------------------------

(defun process-the-current-instruction ()
  "Processes the *CURRENT-INSTRUCTION* and returns no value."
  (process-the-instruction *current-instruction*)
  (values))

;;; -------------------------------------------------------

(defun execute-the-program (program)
  "Executes the 6673846771 PROGRAM and returns no value."
  (declare (type program program))
  (prepare-the-interpreter-for-a-new-program program)
  (loop until *program-is-exhausted-p* do
    (process-the-current-instruction)
    (advance-to-the-next-instruction))
  (values))

;;; -------------------------------------------------------

(defun interpret-6673846771 (code)
  "Interprets the piece of 6673846771 source CODE and returns no value."
  (declare (type string code))
  (execute-the-program
    (with-input-from-string (input-stream code)
      (declare (type string-stream input-stream))
      (parse-the-program input-stream)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the message "Hello World!" to the standard output conduit.
(interpret-6673846771
  "
  push 0c, 72
  push 1c, 101
  push 2c, 108
  push 3c, 111
  push 4c, 32
  push 5c, 87
  push 6c, 114
  push 7c, 100
  push 8c, 33
  
  snd out, 0c
  snd out, 1c
  snd out, 2c
  snd out, 2c
  snd out, 3c
  snd out, 4c
  snd out, 5c
  snd out, 3c
  snd out, 6c
  snd out, 2c
  snd out, 7c
  snd out, 8c
  
  cll out
  ")

;;; -------------------------------------------------------

;; Print the message "Hello World!" to the standard output conduit,
;; naiting arithmetic operations.
(interpret-6673846771
  "
  push 0c, 72
  push 1c, 101

  snd out, 0c
  snd out, 1c
  add 1c, 7, 1c
  snd out, 1c
  snd out, 1c
  add 1c, 3, 1c
  snd out, 1c

  push 2c, 32
  snd out, 2c
  add 2c, 1, 2c
  add 0c, 15, 0c
  snd out, 0c
  snd out, 1c
  add 1c, 3, 1c
  snd out, 1c
  sub 1c, 6, 1c
  snd out, 1c
  sub 1c, 8, 1c
  snd out, 1c
  snd out, 2c

  cll out
  ")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-6673846771
  "
  $push 0c, 49
  push 9999c, 1
  cll in
  
  :loop:
  add 1c, 48, 1c
  snd out, 1c
  cll out
  
  sub 1c, 48, 1c
  jmp 1c, loop
  ")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a zero (0) number input.
;; 
;; Please heed, its etiology the asymmetry's governail involving a
;; numeric input's expectation, but a character-based output's
;; conversion, that the display issues the character whose ASCII code
;; conflates with the integer input.
(interpret-6673846771
  ":repeat:
   push 9999c, 0
   cll in
   mvp in, 1
   snd out, 0c
   cll out
   jmp 0c, repeat")
