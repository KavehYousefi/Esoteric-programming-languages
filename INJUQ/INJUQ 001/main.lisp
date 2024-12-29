;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "INJUQ", created by the Esolang author "ChuckEsoteric08"
;; and presented on April 22nd, 2023, the dioristic proprium of which
;; wones in its conflation of code and data in a entreparted
;; entreparted memory of integer-valued cells, whence each quadruple
;; composite of accolent elements forms an instruction's parameters.
;; 
;; 
;; Concept
;; =======
;; The INJUQ programming language is founded upon an infinite memory as
;; its substratum, its woning signed integer numbers which concomitantly
;; provide the instruction parameters and the program data, the
;; coefficiency in this union ensuing from a cyclic stage of parameters
;; obtention, a cell's incrementation, its value's juxtaposition with a
;; desideratum, and, in the affirmative case, the instruction pointer's
;; jumping to a destination, or a simple advancement as the failure's
;; ultimity.
;; 
;; == INJUQ: [IN]CREMENT AND [JU]MP IF E[Q]UAL ==
;; The INJUQ language's norning constitutes an epiphenomenon begotten by
;; its fundamental tenets, scilicet, its workings produced during a
;; course ordained to "INcrement and JUmp if eQual".
;; 
;; == INJUQ: ONE INSTRUCTION, FOUR PARAMETERS, MANIFOLD CAUSATA ==
;; The INJUQ programming language subsumes into the category of
;; "One Instruction Set Computer" (OISC), its haecceity's gendrure that
;; of an aefauld instruction, however, producing discrepant causata in
;; response its parameters' configuration.
;; 
;; In the case of INJUQ, this componency enumerates a quadruple account,
;; usually agnominated as, in this exact order: "a", "b", "c", "d".
;; 
;; == INJUQ: CODE AND DATA CONFLATE IN THE INTEGER-VALUED MEMORY ==
;; The existencies of the program code and the data occur in a system
;; of perfect symphytism, the twissel's presence molded into the memory,
;; a theoretically infinite dispansion of cells.
;; 
;; Each such entity acts as a salvatory to a scalar integer value of
;; any sign and mickleness; the inchoation's state conflating with a
;; value of zero (0), and in this paregal to any not explicitly
;; modulated constituent.
;; 
;; References to cells follow the design signed integer addresses,
;; iterum without any bournes' impositions along the axes.
;; 
;; A twain of special addresses, -1 and -2, exists for the input and
;; output facilities' optional entelech.
;; 
;; == INJUQ PROGRAMS: INCREMENT, JUXTAPOSE, JUMP OR ADVANCE ==
;; An INJUQ program's operative deportment proceeds from the following
;; stipulations:
;; 
;;   (1) INITIALIZE THE INSTRUCTION POINTER:
;;       Empight the instruction pointer (IP) on the memory cell
;;       amenable to the index zero (0).
;;   
;;   (2) EXTRACT INSTRUCTION PARAMETERS:
;;       Extract the four subsequent memory cell values inchoating from
;;       the instruction pointer's current location:
;;         a = memory[ip + 0]
;;         b = memory[ip + 1]
;;         c = memory[ip + 2]
;;         d = memory[ip + 3]
;;   
;;   (3) INCREMENT OR PRINT:
;;       (a) If b equals -1 and the input capability is activated,
;;           query the standard input conduit for some form of input,
;;           return its integral response, and increment the value of
;;           the cell at memory[a] by this datum.
;;       (b) If b equals -2 and the output capability is activated,
;;           print the value of the cell at memory[a] in some form to
;;           the standard output.
;;       (c) In any other case, increment the value of the cell at
;;           memory[a] by the value at memory[b].
;;   
;;   (4) RELOCATE THE INSTRUCTION POINTER:
;;       (a) If the value in the cell at memory[a] and the value in
;;           memory[c] are equal, relocate the instruction pointer to
;;           the value d.
;;       (b) Otherwise advance the instruction pointer forward by four
;;           positions in order to access the subsequent argument
;;           quadruple.
;;   
;;   (5) HALT OR CONTINUE:
;;       (a) If the instruction pointer has assumed the index -1 in the
;;           course of the prevenient step (4), immediately halt the
;;           program.
;;       (b) Otherwise continue with the step (2).
;; 
;; A formulation of the treatise invested with superior formality shall
;; be limned in the pseudocode alow:
;; 
;;   Given:
;;     memory: The program memory; with memory[i] constituting the
;;             value at the zero-based address i.
;;     ip:     The instruction pointer (IP) location.
;;   
;;   let ip <- 0
;;   
;;   while ip != -1 do
;;     let a <- memory[ip]
;;     let b <- memory[ip + 1]
;;     let c <- memory[ip + 2]
;;     let d <- memory[ip + 3]
;;     
;;     { a* <- user input }
;;     if b = -1 then
;;       memory[a] <- memory[a] + query user input
;;     else if b = -2 then
;;       print a
;;     else
;;       { a* <- a* + b* }
;;       memory[a] <- memory[a] + memory[b]
;;     end if
;;     
;;     { a* = c*? => Go to d. }
;;     if memory[a] = memory[c]
;;       ip <- d
;;     else
;;       ip <- ip + 4
;;     end if
;;   end while
;; 
;; == INPUT AND OUTPUT CONSTITUTE OPTIONAL SERVICES ==
;; The wikes involving the reception of input and the issuance of output
;; do not partake of a mandatory ligation in the language; the
;; nomothesia in this matter relates of homologation, and redes a
;; character-based and/or an immediate integer communication solution.
;; 
;; The potential policies in regard to inputs shall be enumerated alow:
;; 
;;   ------------------------------------------------------------------
;;   Input policy | Deportment
;;   -------------+----------------------------------------------------
;;   None         | Refuses any input.
;;   ..................................................................
;;   Character    | Queries the standard input for a character and
;;                | returns its ASCII code or Unicode code point.
;;   ..................................................................
;;   Integer      | Queries the standard input for a signed or unsigned
;;                | integer number and returns thilk.
;;   ------------------------------------------------------------------
;; 
;; The athwart airt's involvement, from the external contributions to
;; the viscera's expression, will be attended in the coming tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Output policy | Deportment
;;   --------------+---------------------------------------------------
;;   None          | Refuses any output.
;;   ..................................................................
;;   Character     | Prints the character whose ASCII code or Unicode
;;                 | code point concurs with the requested memory
;;                 | cell's value.
;;   ..................................................................
;;   Integer       | Prints a cell's value in its verbatim numeric form
;;                 | to the standard output.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation has been realized in the
;; programming language Common Lisp, the process' entirety a conjunction
;; of the transcription tier, which from the INJUQ source code string
;; produces the program memory's a sparse vector representation, on
;; thilk the operative aspect expediates its labor.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-12-27
;; 
;; Sources:
;;   [esolang2024INJUQ]
;;   The Esolang contributors, "INJUQ", February 29th, 2024
;;   URL: "https://esolangs.org/wiki/INJUQ"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Produces a veridical Boolean equivalency to the OBJECT as construed
   in its agency as a \"generalized boolean\" entity, returning for a
   non-``NIL'' input a ``boolean'' value of ``T'', otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generic-type-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER represents the generic type
   sentinel ``*'', returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type T type-specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))

;;; -------------------------------------------------------

(defun conforms-with-type-p (candidate expected-type)
  "Determines whether the CANDIDATE complies with the EXPECTED-TYPE,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type T candidate))
  (declare (type T expected-type))
  (the boolean
    (get-boolean-value-of
      (or (generic-type-p expected-type)
          (typep          candidate expected-type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency
   enumerates a membership of zero or more entries, everichon among
   these a composite of a key, adhering to the KEY-TYPE, and a value
   desumed from the VALUE-TYPE, with both species conflating with the
   generic sentinel ``*''."
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
                  (conforms-with-type-p current-key   key-type)
                  (conforms-with-type-p current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype sparse-integer-vector ()
  "The ``sparse-integer-vector'' type defines a sparse one-dimensional
   array of signed integer numbers in a hash table's guise, thilk's keys
   furnish the signed integral addresses, responding with values of the
   same numeric realm."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which, without the semblance of exhaustion, tallies the
   functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   a species whose dioristic amplectation encompasses spaces, horizontal
   tabs, newlines, and carriage return behests, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (or (char= candidate #\Newline)
          (char= candidate #\Return)
          (char= candidate #\Space)
          (char= candidate #\Tab)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   comprehending zero or more accolent whitespaces and returns the
   location immediately succeeding the omitted parcel in the START."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-start-of-word (source start)
  "Proceeding from the START position into the SOURCE, and skipping any
   prevenient whitespaces, locates the nearest word and returns its
   index into the SOURCE."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (skip-whitespaces source start)))

;;; -------------------------------------------------------

(defun locate-end-of-word (source start)
  "Proceeding from the START position into the SOURCE, locates the
   index immediately succeeding the nearest word's occupied parcel, and
   return thilk."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (or (position-if #'whitespace-character-p source :start start)
        (length source))))

;;; -------------------------------------------------------

(defun locate-next-word (source start)
  "Proceeding from the START position into the SOURCE, locates and
   extracts the nearest word and returns two values:
     (1) The start index into the SOURCE of the nearest word.
     (2) The position into the SOURCE immediately succeeding the
         detected word's occupied parcel.
   ---
   The detected word start and end positions will conflate if and only
   if the SOURCE tmema commencing from the START index is either of
   zero length or comprehends exclusively whitespaces."
  (declare (type string source))
  (declare (type fixnum start))
  (let ((word-start-position (locate-start-of-word source start)))
    (declare (type fixnum word-start-position))
    (the (values fixnum fixnum)
      (values word-start-position
        (locate-end-of-word source word-start-position)))))

;;; -------------------------------------------------------

(defun read-next-memory-element (source start)
  "Proceeding from the START position into the SOURCE, extracts the
   next memory element and returns two values:
     (1) The nearest memory element as a signed or unsigned integer
         number.
     (2) The position into the SOURCE immediately succeeding the parcel
         occupied by the extracted element."
  (declare (type string source))
  (declare (type fixnum start))
  (multiple-value-bind (element-start-position element-end-position)
      (locate-next-word source start)
    (declare (type fixnum element-start-position))
    (declare (type fixnum element-end-position))
    (the (values integer fixnum)
      (values
        (parse-integer source
          :start element-start-position
          :end   element-end-position)
        element-end-position))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ((cells
    :initform      (make-hash-table :test #'eql)
    :reader        memory-cells
    :type          sparse-integer-vector
    :documentation "A sparse vector of signed integer cells."))
  (:documentation
    "The ``Memory'' class furnishes a reification of the INJUQ program
     memory as a theoretically infinite dispansion of integer-valued
     cells, amenable to integral addresses.
     ---
     Proceeding from a hash table as the ultimate firmament, this
     one-dimensional array emulation operates in a sparse fashion,
     storing merely those entries explicitly modulated in the mapping
     structure."))

;;; -------------------------------------------------------

(defun make-pristine-memory ()
  "Creates and returns a fresh ``Memory'' instance initialized in all
   of its locations with zeroes (0)."
  (the Memory
    (make-instance 'Memory)))

;;; -------------------------------------------------------

(defun memory-cell-at (memory address)
  "Returns the value stored in MEMORY cell amenable to the ADDRESS."
  (declare (type Memory  memory))
  (declare (type integer address))
  (with-slots (cells) memory
    (declare (type sparse-integer-vector cells))
    (the integer
      (gethash address cells 0))))

;;; -------------------------------------------------------

(defun (setf memory-cell-at) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the ADDRESS and
   returns no value."
  (declare (type integer new-value))
  (declare (type Memory  memory))
  (declare (type integer address))
  (with-slots (cells) memory
    (declare (type sparse-integer-vector cells))
    (setf (gethash address cells 0) new-value))
  (values))

;;; -------------------------------------------------------

(defun extract-four-memory-cells-from (memory start-address)
  "Proceeding from the inclusive START-ADDRESS, extracts four subsequent
   cells values from the MEMORY and returns these as four values in
   their correct order."
  (declare (type Memory  memory))
  (declare (type integer start-address))
  (the (values integer integer integer integer)
    (values
      (memory-cell-at memory (+ start-address 0))
      (memory-cell-at memory (+ start-address 1))
      (memory-cell-at memory (+ start-address 2))
      (memory-cell-at memory (+ start-address 3)))))

;;; -------------------------------------------------------

(defun initialize-memory (memory initial-cell-values)
  "Stores the INITIAL-CELL-VALUES in the MEMORY cells inchoating with
   the address zero (0) and returns the modified MEMORY object."
  (declare (type Memory memory))
  (declare (type list   initial-cell-values))
  (loop
    for initial-value   of-type integer in   initial-cell-values
    and current-address of-type integer from 0 by 1
    do  (setf (memory-cell-at memory current-address) initial-value))
  (the Memory memory))

;;; -------------------------------------------------------

(defun determine-boundary-addresses (memory)
  "Determines the inclusive smallest and the inclusive largest address
   occupied by the MEMORY's cells and returns two values:
     (1) The inclusive minimum address as a signed integer number.
     (2) The inclusive maximum address as a signed integer number."
  (declare (type Memory memory))
  (with-slots (cells) memory
    (declare (type sparse-integer-vector cells))
    (the (values integer integer)
      (loop
        for current-address
          of-type integer
          being   the hash-keys in cells
        minimize current-address
          into    minimum-address
          of-type integer
        maximize current-address
          into    maximum-address
          of-type integer
        finally
          (return
            (values minimum-address maximum-address))))))

;;; -------------------------------------------------------

(defun sum-cell-values (memory augend-address addend-address)
  "Increments the value stored in the MEMORY cell amenable to the
   AUGEND-ADDRESS by the value maintained in the ADDEND-ADDRESS and
   returns no value."
  (declare (type Memory  memory))
  (declare (type integer augend-address))
  (declare (type integer addend-address))
  (incf (memory-cell-at memory augend-address)
        (memory-cell-at memory addend-address))
  (values))

;;; -------------------------------------------------------

(defun increment-cell-value-by (memory augend-address addend)
  "Increments the value stored in the MEMORY cell amenable to the
   AUGEND-ADDRESS by the ADDEND and returns no value."
  (declare (type Memory  memory))
  (declare (type integer augend-address))
  (declare (type integer addend))
  (incf (memory-cell-at memory augend-address)
        addend)
  (values))

;;; -------------------------------------------------------

(defun cell-values-are-equal-p (memory first-address second-address)
  "Determines whether the MEMORY cell amenable to the FIRST-ADDRESS
   comprehends a value paregal to that at the SECOND-ADDRESS, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Memory  memory))
  (declare (type integer first-address))
  (declare (type integer second-address))
  (the boolean
    (get-boolean-value-of
      (= (memory-cell-at memory first-address)
         (memory-cell-at memory second-address)))))

;;; -------------------------------------------------------

(defmethod print-object ((memory Memory) (stream T))
  (declare (type Memory      memory))
  (declare (type destination stream))
  (multiple-value-bind (minimum-address maximum-address)
      (determine-boundary-addresses memory)
    (declare (type integer minimum-address))
    (declare (type integer maximum-address))
    (loop
      for current-address
        of-type integer
        from    minimum-address
        to      maximum-address
      do
        (format stream "~&Memory[~d] = ~d" current-address
          (memory-cell-at memory current-address)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of memory parser.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-cell-values-into (source destination)
  "Parses the initial memory elements comprehended in the piece of INJUQ
   SOURCE code, stores these in the DESTINATION memory object's first
   cells, and returns the modified DESTINATION."
  (declare (type Memory destination))
  (declare (type string source))
  (the Memory
    (initialize-memory destination
      (loop
        with current-position
          of-type fixnum
          =       (skip-whitespaces source 0)
        and  current-element
          of-type integer
          =       0
        while (< current-position (length source)) collect
          (progn
            (multiple-value-setq (current-element current-position)
              (read-next-memory-element source current-position))
            (setf current-position
              (skip-whitespaces source current-position))
            current-element)))))

;;; -------------------------------------------------------

(defun parse-memory (source)
  "Parses the initial memory elements comprehended in the piece of INJUQ
   SOURCE code, stores these in a freshly created ``Memory'' object's
   first cells, and returns the thus prepared memory."
  (declare (type string source))
  (the Memory
    (parse-cell-values-into source
      (make-pristine-memory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Input-Handler".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Input-Handler ()
  ()
  (:documentation
    "The ``Input-Handler'' interface furnisehs a common foundry for all
     concrete classes allotted the onus of handling user input in any
     form."))

;;; -------------------------------------------------------

(defclass No-Input-Handler (Input-Handler)
  ()
  (:documentation
    "The ``No-Input-Handler'' class serves as a paragon of mateotechny
     in its lack of any epiphenomenal involvement upon an input request,
     merely responding with a specified default value."))

;;; -------------------------------------------------------

(defclass Character-Input-Handler (Input-Handler)
  ((prompt-message
    :initarg       :prompt-message
    :initform      "~&>> "
    :type          string
    :documentation ""))
  (:documentation
    "The ``Character-Input-Handler'' applies itself to the request of a
     character from the standard input, preceded by a specified prompt
     message's issuance to the standard output conduit."))

;;; -------------------------------------------------------

(defclass Integer-Input-Handler (Input-Handler)
  ((prompt-message
    :initarg       :prompt-message
    :initform      "~&Please input an integer: "
    :type          string
    :documentation ""))
  (:documentation
    "The ``Integer-Input-Handler'' applies itself to the request of a
     signed or unsigned integer number from the standard input, preceded
     by a specified prompt message's issuance to the standard output
     conduit."))

;;; -------------------------------------------------------

(defgeneric can-handle-input-p (handler)
  (:documentation
    "Determines whether the input HANDLER accepts input requests,
     returning on confirmation a ``boolean'' value of ``T'', otherwise
     ``NIL''.")
  
  (:method ((handler No-Input-Handler))
    (declare (type No-Input-Handler handler))
    (declare (ignore                handler))
    (the boolean NIL))
  
  (:method ((handler Character-Input-Handler))
    (declare (type Character-Input-Handler handler))
    (declare (ignore                       handler))
    (the boolean T))
  
  (:method ((handler Integer-Input-Handler))
    (declare (type Integer-Input-Handler handler))
    (declare (ignore                     handler))
    (the boolean T)))

;;; -------------------------------------------------------

(defgeneric handle-input (handler)
  (:documentation
    "Invokes the input HANDLER in order to request an input from a
     responsible conduit in some form and returns a signed integer
     representation of the response.")
  
  (:method ((handler No-Input-Handler))
    (declare (type No-Input-Handler handler))
    (declare (ignore                handler))
    (the integer 0))
  
  (:method ((handler Character-Input-Handler))
    (declare (type Character-Input-Handler handler))
    (format T "~@?"
      (slot-value handler 'prompt-message))
    (finish-output)
    (the fixnum
      (char-code
        (prog1
          (read-char NIL NIL #\Null)
          (clear-input)))))
  
  (:method ((handler Integer-Input-Handler))
    (declare (type Integer-Input-Handler handler))
    (format T "~@?"
      (slot-value handler 'prompt-message))
    (finish-output)
    (the integer
      (prog1
        (parse-integer
          (read-line NIL NIL "0"))
        (clear-input)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Output-Handler".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Output-Handler ()
  ()
  (:documentation
    "The ``Output-Handler'' applies itself to the issuance of an
     integral value's display in some specific form to the standard
     output conduit."))

;;; -------------------------------------------------------

(defclass No-Output-Handler (Output-Handler)
  ()
  (:documentation
    "The ``No-Output-Handler'' class serves as a paragon of mateotechny
     in its lack of any epiphenomenal involvement upon an output
     behest."))

;;; -------------------------------------------------------

(defclass Character-Output-Handler (Output-Handler)
  ()
  (:documentation
    "The ``Character-Output-Handler'' class is entalented with the
     printing of a numeric argument in the guise of the character whose
     character code matches the value."))

;;; -------------------------------------------------------

(defclass Integer-Output-Handler (Output-Handler)
  ()
  (:documentation
    "The ``Character-Output-Handler'' class is entalented with the
     printing of a numeric argument in its verbatim form."))

;;; -------------------------------------------------------

(defgeneric handle-output (handler value)
  (:documentation
    "Invokes the output HANDLER in order to issue output involving the
     integer VALUE to a responsible conduit in some form and returns no
     value.")
  
  (:method ((handler No-Output-Handler)
            (value   integer))
    (declare (type No-Output-Handler handler))
    (declare (ignore                 handler))
    (declare (type integer           value))
    (declare (ignore                 value))
    (values))
  
  (:method ((handler Character-Output-Handler)
            (value   integer))
    (declare (type Character-Output-Handler handler))
    (declare (ignore                        handler))
    (declare (type integer                  value))
    (format T "~c"
      (code-char value))
    (finish-output)
    (values))
  
  (:method ((handler Integer-Output-Handler)
            (value   integer))
    (declare (type Integer-Output-Handler handler))
    (declare (ignore                      handler))
    (declare (type integer                value))
    (format T "~&~d" value)
    (finish-output)
    (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of standard input and output handlers.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type No-Input-Handler         +STANDARD-NO-INPUT+))
(declaim (type Character-Input-Handler  +STANDARD-CHARACTER-INPUT+))
(declaim (type Integer-Input-Handler    +STANDARD-INTEGER-INPUT+))
(declaim (type No-Output-Handler        +STANDARD-NO-OUTPUT+))
(declaim (type Character-Output-Handler +STANDARD-CHARACTER-OUTPUT+))
(declaim (type Integer-Output-Handler   +STANDARD-INTEGER-OUTPUT+))

;;; -------------------------------------------------------

(defparameter +STANDARD-NO-INPUT+
  (make-instance 'No-Input-Handler)
  "The default input handler for rejection of input.")

(defparameter +STANDARD-CHARACTER-INPUT+
  (make-instance 'Character-Input-Handler)
  "The default input handler for character input.")

(defparameter +STANDARD-INTEGER-INPUT+
  (make-instance 'Integer-Input-Handler)
  "The default input handler for integer input.")

(defparameter +STANDARD-NO-OUTPUT+
  (make-instance 'No-Output-Handler)
  "The default output handler for the omission of output.")

(defparameter +STANDARD-CHARACTER-OUTPUT+
  (make-instance 'Character-Output-Handler)
  "The default output handler for character output.")

(defparameter +STANDARD-INTEGER-OUTPUT+
  (make-instance 'Integer-Output-Handler)
  "The default output handler for integer output.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((memory
    :initarg       :memory
    :initform      (error "Missing program memory.")
    :accessor      program-memory
    :type          Memory
    :documentation "The program memory as a sparse integer vector.")
   (ip
    :initform      0
    :accessor      program-ip
    :type          integer
    :documentation "The instruction pointer's (IP) zero-based address
                    into the MEMORY.")
   (input-handler
    :initarg       :input-handler
    :initform      +STANDARD-INTEGER-INPUT+
    :accessor      interpreter-input-handler
    :type          Input-Handler
    :documentation "The entity responsible for the obtention of user
                    input.")
   (output-handler
    :initarg       :output-handler
    :initform      +STANDARD-INTEGER-OUTPUT+
    :accessor      interpreter-output-handler
    :type          Output-Handler
    :documentation "The entity responsible for the issuance of printing
                    facilities.")
   (program-halted-p
    :initform      NIL
    :accessor      program-halted-p
    :type          boolean
    :documentation "A Boolean flag which determines whether the program
                    shall be terminated."))
  (:documentation
    "The ``Interpreter'' class is ordained to the wike of accompassing
     actual efficacy to an INJUQ program committed in the form of its
     memory."))

;;; -------------------------------------------------------

(defun current-instruction-parameters (interpreter)
  "Returns the current instruction parameters extracted from the
   INTERPRETER's underlying memory."
  (declare (type Interpreter interpreter))
  (with-slots (memory ip) interpreter
    (declare (type Memory  memory))
    (declare (type integer ip))
    (the (values integer integer integer integer)
      (extract-four-memory-cells-from memory ip))))

;;; -------------------------------------------------------

(defun accepts-input-p (interpreter)
  "Determines whether the INTERPRETER is capacitated to request user
   inputs, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (can-handle-input-p
      (interpreter-input-handler interpreter))))

;;; -------------------------------------------------------

(defun query-user-input (interpreter)
  "Queries the responsible input conduit for an input by adminiculum of
   the INTERPRETER's internally managed input handler and returns an
   integral representation of the obtained response."
  (declare (type Interpreter interpreter))
  (the integer
    (handle-input
      (interpreter-input-handler interpreter))))

;;; -------------------------------------------------------

(defun print-memory-cell (interpreter cell-address)
  "Prints the value of the INTERPRETER's memory cell amenable to the
   CELL-ADDRESS in a form covenable with the internally managed output
   handler to the standard output conduit and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     cell-address))
  (with-slots (output-handler memory) interpreter
    (declare (type Output-Handler output-handler))
    (declare (type Memory         memory))
    (handle-output output-handler
      (memory-cell-at memory cell-address)))
  (values))

;;; -------------------------------------------------------

(defun execute-current-instruction (interpreter)
  "Executes the INJUQ program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  
  (with-slots (memory) interpreter
    (declare (type Memory  memory))
    
    (multiple-value-bind (a b c d)
        (extract-four-memory-cells-from memory
          (program-ip interpreter))
      (declare (type integer a))
      (declare (type integer b))
      (declare (type integer c))
      (declare (type integer d))
      
      ;; Increment or print "a".
      (cond
        ((and (= b -1) (accepts-input-p interpreter))
          (increment-cell-value-by memory a
            (query-user-input interpreter)))
        ((= b -2)
          (print-memory-cell interpreter a))
        (T
          (sum-cell-values memory a b)))
      
      ;; Go to "d", if necessary.
      (if (cell-values-are-equal-p memory a c)
        (setf (program-ip interpreter) d)
        (incf (program-ip interpreter) 4)))
    
    ;; Halt the program if necessary.
    (when (= (program-ip interpreter) -1)
      (setf (program-halted-p interpreter) T)))
  
  (values))

;;; -------------------------------------------------------

(defun execute-program (interpreter)
  "Executes the INJUQ program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type Interpreter interpreter))
  (loop until (program-halted-p interpreter) do
    (execute-current-instruction interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-INJUQ (code
                        &key (input-handler  +STANDARD-INTEGER-INPUT+)
                             (output-handler +STANDARD-INTEGER-OUTPUT+))
  "Interprets the piece of INJUQ source CODE, utilizing the optional
   INPUT-HANDLER for input obtentions and the OUTPUT-HANDLER for the
   issuance of printing behests, and returns no value."
  (declare (type string         code))
  (declare (type Input-Handler  input-handler))
  (declare (type Output-Handler output-handler))
  (execute-program
    (make-instance 'Interpreter
      :memory         (parse-memory code)
      :input-handler  input-handler
      :output-handler output-handler))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "HI" once.
(interpret-INJUQ
  " -3  -3  -3   8
    72  73   0   0 
     4  -2   4  12
     5  -2   5  -1"
  :input-handler  +STANDARD-NO-INPUT+
  :output-handler +STANDARD-CHARACTER-OUTPUT+)

;;; -------------------------------------------------------

;; Print "HI" perpetually.
(interpret-INJUQ
  " -3  -3  -3   8
    72  73   0   0 
     4  -2   4  12
     5  -2   4  -1"
  :input-handler  +STANDARD-NO-INPUT+
  :output-handler +STANDARD-CHARACTER-OUTPUT+)

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-INJUQ
  "12  -1  13   8
   12  -2  12  -1
   12  -2  13   8
    0   1   0   0"
  :input-handler  +STANDARD-INTEGER-INPUT+
  :output-handler +STANDARD-INTEGER-OUTPUT+)

;;; -------------------------------------------------------

;; Adder: Query two integers and print their sum to the standard output.
(interpret-INJUQ
  "16  -1  16   4
   17  -1  17   8
   16  17  16  12
   16  -2  16  -1
    0   0   0   0"
  :input-handler  +STANDARD-INTEGER-INPUT+
  :output-handler +STANDARD-INTEGER-OUTPUT+)

;;; -------------------------------------------------------

;; Infinite looping counter.
;; 
;; Code segment:
;;   memory[0]--memory[3]: IncrementCounter
;;     currentCounter, inc1, currentCounter, PrintStar
;;   memory[4]--memory[7]: PrintStar
;;     charCode(*), -2, charCode(*), CheckCountUp
;;   memory[8]--memory[11]: CheckCountUp
;;     currentCounter, inc0, maxCounter, PrintLinebreak
;;   memory[12]--memory[15]: PrepareNextStar
;;     currentCounter, inc0, currentCounter, IncrementCounter
;;   memory[16]--memory[19]: PrintLinebreak
;;     charCode(\n), -2, charCode(\n), NextRow
;;   memory[20]--memory[23]: NextRow
;;     maxCounter, inc1, maxCounter, ResetCounterStart
;;   memory[24]--memory[27]: ResetCounterStart
;;     currentCounter, dec1, counterInit, IncrementCounter
;;   memory[28]--memory[31]: ResetCounterFurther
;;     currentCounter, inc0, currentCounter, ResetCounterStart
;; 
;; Data segment:
;;   memory[32] = counterInit (=  0)
;;   memory[33] = inc0        (=  0)
;;   memory[34] = inc1        (=  1)
;;   memory[35] = dec1        (= -1)
;;   
;;   memory[36] = maxCounter     (= 1, then increment gradually)
;;   memory[37] = currentCounter (= 0, then increment or reset to 0)
;;   memory[38] = charCode(*)    (= 42 = ASCII code of "*")
;;   memory[39] = charCode(\n)   (= 10 = ASCII code of newline)
;; 
;; Symbolic program code:
;;   currentCounter  inc1  currentCounter  4
;;   charCode(*)     -2    charCode(*)     8
;;   currentCounter  inc0  maxCounter      16
;;   currentCounter  inc0  currentCounter  0
;;   charCode(\n)    -2    charCode(\n)    20
;;   maxCounter      inc1  maxCounter      24
;;   currentCounter  dec1  counterInit     0
;;   currentCounter  inc0  currentCounter  24
;;   
;;   0  0   1  -1
;;   1  0  42  10
(interpret-INJUQ
  "37  34  37  4
   38  -2  38  8
   37  33  36  16
   37  33  37  0
   39  -2  39  20
   36  34  36  24
   37  35  32  0
   37  33  37  24
   
   0   0   1   -1
   1   0   42  10"
  :input-handler  +STANDARD-NO-INPUT+
  :output-handler +STANDARD-CHARACTER-OUTPUT+)
