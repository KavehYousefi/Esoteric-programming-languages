;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "BrainfisHQ9+", invented by the Esolang user "Dtuser1337"
;; and presented on March 19th, 2020, the woning of its proprium the
;; language prial's conflation that enumerates Urban Mueller's
;; "brainfuck", Jonathan Todd Skinner's "Deadfish", and Cliff L.
;; Biffle's "HQ9+", in an ultimity whose operation on a bilaterally
;; infinite tape of signed integer number proceeds in a perpetual and
;; interactive read-eval-print-loop (REPL).
;; 
;; 
;; Concept
;; =======
;; The BrainfisHQ9+ programming language's gendrure ensues from the
;; treble coalescence of brainfuck, Deadfish, and HQ9+, operating in a
;; perpetual read-eval-print-loop (REPL), whose inchoation is always
;; established in the request for a line of code to execute during the
;; respective secle, the subject of its pursued bournes a bilaterally
;; infinite signed integer-valued tape's perquisition and manipulation.
;; 
;; == BRAINFISHQ9+ = [BRAIN]FUCK + DEAD[FIS] + [HQ9+] ==
;; The BrainfisHQ9+ programming language's gendure emerges as pair
;; royale's confluence, scilicet, the language *brain*fuck, Dead*fish*,
;; and *HQ9+*, whose competences' coalescence, as well as the data
;; castaldy's principles, paravaunt in brainfuck's ambit, in a lesser
;; degree desumed from the other twissel, engage in a collaboration.
;; 
;; == THE MEMORY: A TAPE OF INTEGER CELLS ==
;; The program memory's manifestation is defined in terms of a
;; bilaterally infinite dispansion of cells, each such member's capacity
;; accompting for an integer number that wists of no imposition
;; conerning the sign and mickleness.
;; 
;; At any instant during a program's execution, a dedicated cursor, the
;; cell pointer, serves as a reference to the contemporaneously selected
;; cell, thilk betokens the aefauld entity invested with an amenability
;; to perquisitions and modulations. Its mobile nature homologates
;; stillatim translations along both airts of the tape for the pointer.
;; 
;; == THE INSTRUCTIONS: SINGLE-SYMBOL IDENTIFIERS ==
;; Ensuing from the tripartite provenance's consuetude, an instruction's
;; identification does not rise aboon a single symbol's guise.
;; 
;; Any content eloigned from a utile employment retains no epiphenomenal
;; wike, and its admissibility constitutes a paregal of its neglect.
;; 
;; == PROGRAMS OPERATE IN A PERPETUAL READ-EVAL-PRINT-LOOP (REPL) ==
;; The BrainfisHQ9+ interpreter's operation adheres to an iterance
;; principle, thilk accompts for Deadfish's and HQ9+'s entheus, and
;; governed by perpetuation, each cycle's inchoation designated by a
;; querying of the standard input conduit for a line of code, preceded
;; by the prompt message, which concludes in a single space character:
;; 
;;   >> 
;; 
;; Succeeding the zero or more symbols' obtention, the reading process
;; segues into the execution stage, the tier whose telos is delineated
;; by the recognized instructions' application; ere an iterum
;; advancement via the input request tier commences.
;; 
;; == NO-OPERATIONS ACCOMPASS NO CAUSATA ==
;; Desumed from its consanguinity with brainfuck, those symbols to whom
;; no operative bailiwick is vouchsafed are consigned to a homologation
;; that limns a paregal to their neglect.
;; 
;; 
;; Instructions
;; ============
;; A duodecimal accompt of operative warklumes serves in BrainfisHQ9+'s
;; instruction set's exhaustion, the variegation commorant in its ambits
;; covering the aspects of rudimentary memory management, basic
;; arithmetics, input and output communications, and a single
;; conditional iterance construct.
;; 
;; A patration of disrespondency appertains to characters not entalented
;; with a behest's entelechy.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be realized in the communication
;; of a requisite mete of nortelry anent the language's operative
;; faculties, complemented by the listing of each elucidated member's
;; provenances:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect                                      | Provenance
;;   --------+---------------------------------------------+-----------
;;   >       | Translates the cell pointer one step to the | brainfuck
;;           | right.                                      | 
;;   ..................................................................
;;   <       | Translates the cell pointer one step to the | brainfuck
;;           | left.                                       | 
;;   ..................................................................
;;   +       | Increments the current cell value by one    | brainfuck,
;;   i       | (1).                                        | Deadfish,
;;   I       |                                             | HQ9+
;;   ..................................................................
;;   -       | Decrements the current cell value by one    | brainfuck,
;;   d       | (1).                                        | Deadfish
;;   D       |                                             | 
;;   ..................................................................
;;   s       | Squares the current cell value.             | Deadfish
;;   S       |                                             | 
;;   ..................................................................
;;   ,       | Queries the standard input conduit for a    | brainfuck
;;           | character and stores its ASCII code in the  | 
;;           | current cell.                               | 
;;   ..................................................................
;;   .       | Prints the character whose ASCII code       | brainfuck
;;   o       | corresponds to the current cell value to    | 
;;   O       | standard output conduit.                    | 
;;   ..................................................................
;;   h       | Prints the message "Hello, World" to the    | HQ9+
;;   H       | standard output conduit, succeeded by a     | 
;;           | single newline.                             | 
;;   ..................................................................
;;   9       | Prints the lyrics of the song "99 Bottles   | HQ9+
;;           | of Beer" to the standard output conduit,    | 
;;           | succeeded by a single newline.              | 
;;   ..................................................................
;;   q       | Prints the currently executed program's     | HQ9+
;;   Q       | source code to the standard output conduit, | 
;;           | succeeded by a single newline.              | 
;;   ..................................................................
;;   [       | If the current cell value equals zero (0),  | brainfuck
;;           | moves the instruction pointer (IP) forward  | 
;;           | to the position immediately succeeding the  | 
;;           | matching "]" token; otherwise proceeds as   | 
;;           | usual.
;;   ..................................................................
;;   ]       | If the current cell value does not equal    | brainfuck
;;           | zero (0), moves the instruction pointer     | 
;;           | (IP) back to the position immediately       | 
;;           | succeeding the matching "[" token;          | 
;;           | otherwise proceeds as usual.                | 
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its entheuses' mature specifications, the BrainfisHQ9+'s
;; protolog experiences a few inroads of ambiguity's tholance; a subset
;; thereof shall be desumed as the following sections' cynosure.
;; 
;; == WHICH POLICY APPLIES TO UNRECOGNIZED CONTENT? ==
;; The triunity's applicability to BrainfisHQ9+'s cleronomy, scilicet,
;; brainfuck, Deadfish, and HQ9+, conduces the gendrure of the
;; descendant's scheme for non-operative symbols' participation in an
;; evaluated piece of code.
;; 
;; The contingency wists of a treble ramosity in this predicament's
;; resolution:
;; 
;;   (1) NO CAUSATUM APPLIES:
;;       This deportment, autochthonous in both brainfuck and HQ9+,
;;       abstains from any epiphenomenal conclusion's assignment to
;;       no-operation tokens.
;;   
;;   (2) NEWLINE CHARACTERS ARE ISSUED:
;;       A kenspeckle aspect of its diorism, each non-instruction
;;       character's procession in Deadfish is rendered the provenance
;;       of a separate newline character's issuance on the standard
;;       output conduit, rather than an error's parturition.
;;   
;;   (3) AN ERROR IS INSTIGATED:
;;       An ultima ratio, the most severe reaction is delineated by a
;;       fatal error's incurrence.
;; 
;; Concluding from a stern perpensity, it has been adjudged to incline
;; towards the first (1) option's imputation, administering no effective
;; value to symbols lacking an operative affiliation.
;; 
;; 
;; Implementation
;; ==============
;; This project's realization has been achieved in the programming
;; language Common Lisp, the object of its causata's accompassing the
;; incipial or user-supplied source code string itself.
;; 
;; A dioristic attribute commorant in this project, as a warklume for
;; the concrete memory model's ambivalent nature in the BrainfisHQ9+
;; programming language's protolog, the actually deployed tape
;; implementation is subject to the user's deliberations, this ramosity
;; in the deportment constituting a derivation from a "Tape" interface
;; whose palpable opificers are contributed as the "Byte-Tape", for
;; unsigned byte-valued cells, as is a popular consuetude in the
;; brainfuck realm, and the "Integer-Tape", the default's establishment,
;; which wists of no impositions in its integral cells' mickleness and
;; sign.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-09-01
;; 
;; Sources:
;;   [esolang2022BrainfisHQ9+]
;;   The Esolang contributors, "BrainfisHQ9+", November 15th, 2022
;;   URL: "https://esolangs.org/wiki/BrainfisHQ9%2B"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose conformation
   admits zero or more entries, everichon among these a twissel
   composite accompting for a key compliant with the KEY-TYPE and a
   value engaged in an affiliation withal of the VALUE-TYPE, for both
   defines the generic sentinel ``*'' the default."
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
                (and (typep current-key   key-type)
                     (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte compact of eight accolent
   bits, and, occupying, as a consectary, the closed integral interval
   [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list whose componency's admission
   homologates zero or more elements of the ELEMENT-TYPE, for thilk
   governs the stipulation as the generic sentinel ``*'' as a default."
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (number)
  "Multiplies the NUMBER by itself and returns the resulting product."
  (declare (type integer number))
  (the (integer 0 *)
    (* number number)))

;;; -------------------------------------------------------

(define-modify-macro square-in-place ()
  square
  "Assigns to its associated place the squared value of itself, as a
   concomitant destructively modifying thilk, and returns the resulting
   product.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Jump-Table".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of fixnum fixnum)
    :documentation "Associates the jump start and end points in a
                    bidirectional fashion by their zero-based source
                    code positions' adminiculum."))
  (:documentation
    "The ``Jump-Table'' class accompts for the recipient of that wike
     involving the castaldy of a BrainfisHQ9+'s jump points, thilk
     filsts in its capacitation to manifest conditional and iterative
     constructs."))

;;; -------------------------------------------------------

(defun prepare-empty-jump-table ()
  "Creates and returns a ``Jump-Table'' whose connections at the
   gendrure's instant are vacant."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-jump-points (table start-point end-point)
  "Connects the jump START-POINT and END-POINT in a bidirectional
   manner in the jump TABLE and returns no value."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point (slot-value table 'connections)) end-point
    (gethash end-point   (slot-value table 'connections)) start-point)
  (values))

;;; -------------------------------------------------------

(defun collect-jump-points (table code)
  "Collates the jump points partaking of the BrainfisHQ9+ source CODE
   in the jump TABLE and returns the same.
   ---
   Please heed that extant entries are not subject to a purge from the
   TABLE."
  (declare (type Jump-Table    table))
  (declare (type simple-string code))
  (let ((forward-jump-points NIL))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for current-token    of-type character across code
      for current-position of-type fixnum    from   0 by 1
      
      if (char= current-token #\[) do
        (push current-position forward-jump-points)
      else if (char= current-token #\]) do
        (if forward-jump-points
          (connect-jump-points table
            (pop forward-jump-points)
            current-position)
          (error "Unmatched back jump point at position ~d."
            current-position))
      end
      
      finally
        (when forward-jump-points
          (error "Unmatched forward jump point~d at position~:p ~
                  ~{~d~^, ~}."
            (length forward-jump-points)
            forward-jump-points)))
    (the Jump-Table table)))

;;; -------------------------------------------------------

(defun get-destination-jump-point (table point-of-departure)
  "Returns for the POINT-OF-DEPARTURE the allied destination jump point
   as registered in the jump TABLE; or, upon its disrespondency, signals
   an error of an unspecified type."
  (declare (type Jump-Table table))
  (declare (type fixnum     point-of-departure))
  (the fixnum
    (or (gethash point-of-departure (slot-value table 'connections))
        (error "No destination point associated with the position ~d."
          point-of-departure))))

;;; -------------------------------------------------------

(defun clear-jump-points (table)
  "Removes all entries from the jump TABLE and returns no value."
  (declare (type Jump-Table table))
  (clrhash
    (slot-value table 'connections))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Tape".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tape ()
  ()
  (:documentation
    "The ``Tape'' interface is apportioned the wike of a common
     foundry's provision for all classes intent on the representation
     of the BrainfisHQ9+ memory's tape."))

;;; -------------------------------------------------------

(defgeneric current-cell-value (tape)
  (:documentation
    "Returns the datum stored in the TAPE cell selected by the cell
     pointer."))

;;; -------------------------------------------------------

(defgeneric (setf current-cell-value) (new-value tape)
  (:documentation
    "Stores the NEW-VALUE in the TAPE cell selected by the cell pointer
     and returns no value."))

;;; -------------------------------------------------------

(defgeneric move-cell-pointer-right (tape)
  (:documentation
    "Translates the TAPE's cell pointer one step to the right, if
     possible, otherwise accompassing no epiphenomenon, and returns in
     any case no value."))

;;; -------------------------------------------------------

(defgeneric move-cell-pointer-left (tape)
  (:documentation
    "Translates the TAPE's cell pointer one step to the left, if
     possible, otherwise accompassing no epiphenomenon, and returns in
     any case no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Integer-Tape".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Integer-Tape (Tape)
  ((cells
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of integer integer)
    :documentation "A sparse vector of signed integer cells, entalented
                    with an amenability to signed integer indices.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The cell pointer, realized as the key corresponding
                    to the contemporaneously selected into the CELLS
                    hash table."))
  (:documentation
    "The ``Integer-Tape'' class' furnishment is realized in the
     provision of a bilaterally infinite dispansion of signed
     integer-valued cells, whose mickleness limns a tantamount to its
     polarity in its magnanimous liberty."))

;;; -------------------------------------------------------

(defun make-integer-tape ()
  "Creates and returns a fresh ``Integer-Tape'' whose entirety of cells
   resides in the incipial zero-valued state."
  (the Integer-Tape
    (make-instance 'Integer-Tape)))

;;; -------------------------------------------------------

(defmethod current-cell-value ((tape Integer-Tape))
  (declare (type Integer-Tape tape))
  (with-slots (cells pointer) tape
    (declare (type (hash-table-of integer integer) cells))
    (declare (type integer                         pointer))
    (the integer
      (gethash pointer cells 0))))

;;; -------------------------------------------------------

(defmethod (setf current-cell-value) ((new-value integer)
                                      (tape      Integer-Tape))
  (declare (type integer      new-value))
  (declare (type Integer-Tape tape))
  (with-slots (cells pointer) tape
    (declare (type (hash-table-of integer integer) cells))
    (declare (type integer                         pointer))
    (setf (gethash pointer cells 0) new-value))
  (values))

;;; -------------------------------------------------------

(defmethod move-cell-pointer-right ((tape Integer-Tape))
  (declare (type Integer-Tape tape))
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defmethod move-cell-pointer-left ((tape Integer-Tape))
  (declare (type Integer-Tape tape))
  (decf (slot-value tape 'pointer))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Byte-Tape".                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Byte-Tape (Tape)
  ((bits
    :initform      #b00000000
    :type          unsigned-byte
    :documentation "The cell states encoded in a single integer datum's
                    binary representation, each consequent octuple of
                    bits serving in one cell's byte content's
                    definition.
                    ---
                    The cell with the small index yet visited by the
                    cell POINTER wones at the least significant bit
                    (LSB) position, occupying these first eight
                    positions inside the BITS sequence; the subsequent
                    cells appropriate increasingly higher positions.")
   (pointer
    :initform      0
    :type          integer
    :documentation "The signed integer cell pointer.
                    ---
                    If pursuing to access the respective cell's byte
                    datum, this contingently negative cursor
                    necessitates its conversion into a non-negative
                    offset into the BITS.")
   (smallest-accessed-cell-index
    :initform      0
    :type          integer
    :documentation "The smallest cell index yet assumed by the cell
                    POINTER."))
  (:documentation
    "The ``Byte-Tape'' class' furnishment accompts for that of a
     bilaterally infinite catena of unsigned byte-valued cells,
     everichon from this membership meted in its capacity to lend a
     salvatory to an integral number from the range [0, 255], wrapping
     around at any of its bournes in a transgression's cheason."))

;;; -------------------------------------------------------

(defun make-byte-tape ()
  "Creates and returns a fresh ``Byte-Tape'' whose entire dispansion of
   unsigned byte-valued cells remain in the default state of zero (0)."
  (the Byte-Tape
    (make-instance 'Byte-Tape)))

;;; -------------------------------------------------------

(defun get-byte-specifier-for-current-cell (tape)
  "Returns a byte specifier capacitated to select the eight consecutive
   bits from the TAPE's underlying bit sequence of the cell referenced
   by the TAPE's cell pointer.
   ---
   Please heed that the concrete species of the thus produced byte
   specifier accompts for an implementation-dependent definition."
  (declare (type Byte-Tape tape))
  (with-slots (pointer smallest-accessed-cell-index) tape
    (declare (type integer pointer))
    (declare (type integer smallest-accessed-cell-index))
    (the T
      (byte 8
        (* (- pointer smallest-accessed-cell-index)
           8)))))

;;; -------------------------------------------------------

(defmethod current-cell-value ((tape Byte-Tape))
  "Returns the unsigned byte value stored in the byte TAPE's currently
   selected cell."
  (declare (type Byte-Tape tape))
  (the octet
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (slot-value tape 'bits))))

;;; -------------------------------------------------------

(defmethod (setf current-cell-value) ((new-value integer)
                                      (tape      Byte-Tape))
  "Stores the NEW-VALUE in the TAPE's currently selected cells,
   administering as a contingency for the imposed unsigned byte range's
   satisfaction a parasceuastic wrapping of the inducted datum in order
   to ensure the integral interval [0, 255], and returns no value."
  (declare (type integer   new-value))
  (declare (type Byte-Tape tape))
  (setf
    (ldb
      (get-byte-specifier-for-current-cell tape)
      (slot-value tape 'bits))
    (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defmethod move-cell-pointer-right ((tape Byte-Tape))
  (declare (type Byte-Tape tape))
  (incf (slot-value tape 'pointer))
  (values))

;;; -------------------------------------------------------

(defmethod move-cell-pointer-left ((tape Byte-Tape))
  (declare (type Byte-Tape tape))
  (with-slots (pointer smallest-accessed-cell-index) tape
    (declare (type integer pointer))
    (declare (type integer smallest-accessed-cell-index))
    (decf pointer)
    (when (< pointer smallest-accessed-cell-index)
      (setf smallest-accessed-cell-index pointer)
      (with-slots (bits) tape
        (declare (type unsigned-byte bits))
        (setf bits
          (ash bits 8)))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-simple-string (source)
  "Creates and returns a simple string representation of the SOURCE."
  (declare (type string source))
  (the simple-string
    (coerce source 'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of output operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-99-bottles-of-beer-lyrics ()
  "Prints the lyrics of the song and programming problem
   \"99 Bottles of Beer\" to the standard output conduit and returns no
   value."
  (loop for number-of-bottles of-type (integer 0 99) from 99 downto 1 do
    (format *standard-output*
      "~&~d bottle~:p of beer on the wall,~:*~
       ~&~d bottle~:p of beer.~
       ~&Take one down, pass it around,~
       ~&~[No~:;~:*~d~] bottle~:p of beer on the wall.~2%"
      number-of-bottles
      (1- number-of-bottles)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input operations.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-for-a-line-of-code ()
  "Queries the standard input conduit for a line of BrainfisHQ9+ code
   and returns the same."
  (format        *standard-output* "~&>> ")
  (finish-output *standard-output*)
  (the simple-string
    (prog1
      (convert-into-a-simple-string
        (read-line *standard-input* NIL ""))
      (clear-input))))

;;; -------------------------------------------------------

(defun query-for-a-character ()
  "Queries the standard input conduit for a character and returns
   thilk."
  (format        *standard-output* "~&Please enter a character: ")
  (finish-output *standard-output*)
  (the character
    (prog1
      (read-char   *standard-input* NIL #\Null)
      (clear-input *standard-input*))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BrainfisHQ9+ (&key (initial-code "")
                                    (tape         (make-integer-tape)))
  "Starts the BrainfisHQ9+ interpreter, at its inchoation executing the
   optional INITIAL-CODE, ere during each cycle a new code line is,
   request, upon desideration deploying a bespoke TAPE for these
   program tmemata's memory as an alternative to the default
   ``Integer-Tape'' instance, and, if ceasing its operation in any case,
   returns no value."
  (declare (type string initial-code))
  (declare (type Tape   tape))
  
  (let ((ip         0)
        (jump-table (prepare-empty-jump-table)))
    (declare (type fixnum     ip))
    (declare (type Jump-Table jump-table))
    
    (loop
      for current-code
        of-type simple-string
        =       (convert-into-a-simple-string initial-code)
        then    (query-for-a-line-of-code)
      
      do
        (clear-jump-points   jump-table)
        (collect-jump-points jump-table current-code)
        
        (setf ip 0)
        
        (loop while (< ip (length current-code)) do
          (case (char current-code ip)
            (#\>
              (move-cell-pointer-right tape))
            
            (#\<
              (move-cell-pointer-left tape))
            
            ((#\+ #\i #\I)
              (incf
                (current-cell-value tape)))
            
            ((#\- #\d #\D)
              (decf
                (current-cell-value tape)))
            
            ((#\s #\S)
              (square-in-place
                (current-cell-value tape)))
            
            ((#\. #\o #\O)
              (format *standard-output* "~c"
                (code-char
                  (current-cell-value tape))))
            
            (#\,
              (setf (current-cell-value tape)
                (char-code
                  (query-for-a-character))))
            
            (#\[
              (when (zerop (current-cell-value tape))
                (setf ip
                  (get-destination-jump-point jump-table ip))))
            
            (#\]
              (unless (zerop (current-cell-value tape))
                (setf ip
                  (get-destination-jump-point jump-table ip))))
            
            ((#\h #\H)
              (format *standard-output* "~&Hello, World~%"))
            
            (#\9
              (print-99-bottles-of-beer-lyrics))
            
            ((#\q #\Q)
              (format *standard-output* "~a~%" current-code))
            
            (otherwise
              NIL))
          
          (incf ip))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Launch the BrainfisHQ9+ interpreter without an initial program.
(interpret-BrainfisHQ9+)

;;; -------------------------------------------------------

;; Print the lyrics of the song "99 Bottles of Beer".
(interpret-BrainfisHQ9+ :initial-code "9")

;;; -------------------------------------------------------

;; Print the message "Hello, World" thrice.
(interpret-BrainfisHQ9+ :initial-code "iii[H-]")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-BrainfisHQ9+ :initial-code ",[o,]")

;;; -------------------------------------------------------

;; Truth-machine which requires negative cells.
(interpret-BrainfisHQ9+ :initial-code ",.[[-<+>>+<]>-]<<<[<<]>[.]")

;;; -------------------------------------------------------

;; Truth-machine which requires a tape composed of byte-valued cells.
(interpret-BrainfisHQ9+
  :initial-code ",.[-->+[>>]<[.]<<]"
  :tape         (make-byte-tape))
