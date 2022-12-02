;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "Binerdy", invented by the Esolang user "Bas".
;; 
;; Concept
;; =======
;; The language's concept originates from a nostalgic evocation of
;; erstwhile times in which programming, immersed in its infancy,
;; imposed upon the programmer a perforce exposure to the low-level bit
;; representation of his trade. A corollary of this, Binerdy programs
;; are designed as sequences composed of bits only, represented strictly
;; by zeroes (0) and ones (1). Bits of alternating value,
;; indiscriminative regarding the order, and demarcated by the
;; occurrence of two equal bits, define a group, the tally of its
;; members, being the count of alternations, acting in the identity of
;; the respective command. In addition to control structures, a stack
;; manages an arbitrary number of byte values.
;; 
;; == ALTERNATING BITS FORM A GROUP ==
;; A piece of Binerdy source code may only contain the characters "0"
;; and "1". Bits of alternating value in adjacency compose a group,
;; terminated either by the occurrence of two bits of equal content or
;; by the end of the program. If the demarcation concurs with the
;; confrontation of two identical characters, the left bit is tallied
;; among the closing group, while its peer accounts for the pioneer
;; member of the subsequent compound.
;; 
;; == THE GROUP SIZE DETERMINES THE COMMAND ==
;; Commands in Binerdy do not depend on arguments and abstain from
;; identification by agnomination; instead, each bit group forms an
;; instruction, with its number of bits defining the type. The order of
;; bits carries no significance at all; this means that the sequence
;; "0101" is paregal to "1010", with both alluding to the same
;; instruction designated by four bits. Valid group sizes range from
;; inclusive zero (0) to inclusive thirteen (13); any other cardinality
;; incurs a violation and provokes an error.
;; 
;; == DATA IS STORED IN A STACK ==
;; All commands operate on a stack of infinite capacity, the elements of
;; the same constitute bytes composed of eight bits and capable of
;; transcription from and to ASCII characters.
;; 
;; 
;; Architecture
;; ============
;; Binerdy resorts in its architecture to a stack, not bounded by its
;; capacity, each element of which constitutes a simple 8-bit value,
;; that is, an integer confined to the range [0, 255] and in linguistic
;; currency also as an octet. A further administrative utility manifests
;; in the label registry for control flow management.
;; 
;; == STACK ==
;; Binerdy's program storage is incorporated in an infinitely commodious
;; stack, dedicated to the maintenance of byte elements. A logical
;; consectary of this choice, all operations appertain to the data
;; structure's head, or top item.
;; 
;; == LABEL REGISTRY ==
;; The mechanism reserved for the provision of control flow facilities
;; is established by means of labels. Such a designating entity entails
;; a bipartite unit, associating with an identifying byte in the role of
;; a name the instruction pointer location inside of the processed
;; instruction list. Binerdy's potentials embraces two complementary
;; modes of conditional navigation founded upon the perusal of the
;; ensconcing label registry.
;; 
;; 
;; Data Types
;; ==========
;; The purposefully assumed low-level nature of Binerdy accommodates
;; exclusively bits and bytes as objects of currency.
;; 
;; The source code itself encompasses bits in the form of "0" and "1"
;; entities as its sole constituents. The data storage, the stack,
;; manifests as a salvatory of octets, a composition itself of eight
;; bits, and sufficiently potent to occupy the integer range [0, 255].
;; 
;; The operations provided as manipulators upon the stack are themselves
;; members of a variegated gamut of categories, including direct stack
;; modification, arithmetics, input and output, as well as control flow
;; helming.
;; 
;; The stack's byte type does not account for an accident, as it
;; responds to the ASCII character repertoire, itself encoded in eight
;; bits of a compatible range, and patently involved in interaction when
;; printing the top stack element as a character and converting a user
;; input from the latter to the prior realm.
;; 
;; 
;; Syntax
;; ======
;; Binerdy relies upon an utterly homogeneous and simple syntax, with
;; each program composed of zero or more bits, manifesting in the
;; characters "0" and "1". The language form can be expressed in the
;; following Extended Backus-Naur Form (EBNF) description:
;; 
;;   program := { bit } ;
;;   bit     := "0" | "1" ;
;; 
;; 
;; Instructions
;; ============
;; Instructions in Binerdy entail solely niladic operations, addressable
;; by the cardinality of a group of bits, and restrained to the integer
;; range [0, 13]. The command types enumerate as follows, where each
;; alternation, or group size, is juxtaposed with the resulting effect.
;; 
;;   ------------------------------------------------------------------
;;   Group size | Command | Effect
;;   -----------+---------+--------------------------------------------
;;    0         | exit    | Stops the program execution. This situation
;;              |         | can only occur in a program of zero length.
;;   ..................................................................
;;    1         | init    | Pushes the value 0 unto the stack.
;;   ..................................................................
;;    2         | inc     | Increases the top stack element by 1.
;;              |         | If the new value exceeds the upper bound
;;              |         | 255, it overflows to 0.
;;   ..................................................................
;;    3         | dec     | Decreases the top stack element by 1.
;;              |         | If the new value descends below the lower
;;              |         | bound of 0, it overflows to 255.
;;   ..................................................................
;;    4         | add     | Pops the top stack element and adds it
;;              |         | as an added to the new top element.
;;   ..................................................................
;;    5         | sub     | Pops the top stack element and subtracts it
;;              |         | from the new top element.
;;   ..................................................................
;;    6         | label   | Pops the top stack element and stores it
;;              |         | as a label name associated with the current
;;              |         | instruction pointer location.
;;   ..................................................................
;;    7         | je      | Pops the top stack element L and regards it
;;              |         | as a label name. Then, pops the next top
;;              |         | stack element A, and compares it to the new
;;              |         | top stack element B without its deletion.
;;              |         | If A equals B, moves the instruction
;;              |         | pointer to the location associated with the
;;              |         | label name L; otherwise, proceeds as usual.
;;              |         | An error occurs if L does not specify a
;;              |         | registered label name.
;;   ..................................................................
;;    8         | jne     | Pops the top stack element L and regards it
;;              |         | as a label name. Then, pops the next top
;;              |         | stack element A, and compares it to the new
;;              |         | top stack element B without its deletion.
;;              |         | If A differs from B, moves the instruction
;;              |         | pointer to the location associated with the
;;              |         | label name L; otherwise, proceeds as usual.
;;              |         | An error occurs if L does not specify a
;;              |         | registered label name.
;;   ..................................................................
;;    9         | input   | Queries the user for a byte and pushes its
;;              |         | value contingently adjusted to the range
;;              |         | [0, 255], value unto the stack.
;;   ..................................................................
;;    10        | swap    | Swaps the two top stack elements.
;;   ..................................................................
;;    11        | print   | Prints the ASCII character associated with
;;              |         | the top stack element.
;;   ..................................................................
;;    12        | char    | Prints the top stack element as a number.
;;   ..................................................................
;;    13        | rem     | Pops the top stack element.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; By adminiculum of Binerdy's plain conception and lucid presentation,
;; few latibula are accoutred vouchsafement of ambivalence and inroads
;; of eisegeses; natheless, the products of their detections shall be
;; listed below.
;; 
;; == ARE STACK ELEMENTS ALWAYS OCTETS? ==
;; The introduction of the stack being a witeless effort, the account of
;; its members' haecceity desists from investments to ensure a
;; contribution paragoned to the former. In parallel to the "inc" and
;; "sub" commands, which proclaim with pellucid diction the behavior of
;; the top stack element upon transgression of the interval [0, 255],
;; by overflowing or "wrapping", the remaining subset in conjunction and
;; potency of translating a comprehended item beyond the proper
;; boundaries, already exhausted by the triple "add", "sub", and
;; "input", retains destitution anenst such explications.
;; 
;; A corollary of the disregard would involve a tolerance towards
;; non-byte integers. As a conjecture, the three orra commands shall be
;; furnished with a logical consanguinity in that they also, tacitly,
;; apply the stack element under scrutiny to an overflow behavior, so as
;; to ascertain the octet range's conformance.
;; 
;; == WHAT USER INPUT SHALL BE ACCEPTED? ==
;; In counterdistinguishment from its bimodal output facilities,
;; attending as concomitants to the top stack element's introspection
;; as both a number and the affiliated ASCII character, the input
;; mechanism lacks an exact disquisition of the expected perspective.
;; 
;; An establishment is thus postulated that the input shall be prompted
;; and accepted exclusively in the form of integer numbers, not ligated
;; to any sign or magnitude, but naturalized into the basic byte range
;; [0, 255].
;; 
;; 
;; Implementation
;; ==============
;; This Common Lisp implementation of Binerdy is designed with the
;; intention of maximum simplicity. A prerequisite of the interpretation
;; process, an extraction of instructions delivers the necessitated
;; components, ere their evaluations produces an ultimate causatum.
;; 
;; == INSTRUCTION EXTRACTION ==
;; Questions of logistics and computational efficiency rede a
;; transcription of the source code string into a sequence of
;; instructions, a duty allotted to a preprocessing step whose reckoning
;; of alternations precedes the resolution of its affiliation with an
;; unambiguously corresponding command.
;; 
;; == THE STACK ==
;; With Common Lisp's natural perception of the basic list data type as
;; a last-in-first-out utility, its occupation establishes the prime
;; choice for the stack. The entity must experience an attendance of
;; elevated vigilance in order to align with type safety, as operations
;; empowered with an element's modification bear sufficient potential as
;; to transport the original datum from its commorancy inside the range
;; of [0, 255] into a realm incompatible with the byte nature. The
;; deployment of the modulus operation, alos known as wrapping, confines
;; an arbitrary integer into the tolerated marches.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-03-18
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Binerdy"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, defaulting to ``T''."
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
   a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype destination ()
  "The ``destination'' type defines a sink for write operations,
   including, without claim of exhaustion, the ``format'' and ''write''
   functions."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' defines an integer number in the range [0, 255]."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype instruction ()
  "The ``instruction'' type enumerates the recognized instructions in
   Binerdy, represented in a symbolic guise."
  '(member
    :exit
    :init
    :inc
    :dec
    :add
    :sub
    :label
    :je
    :jne
    :input
    :swap
    :print
    :char
    :rem))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array instruction (14)) +INSTRUCTION-TABLE+))

;;; -------------------------------------------------------

(defparameter +INSTRUCTION-TABLE+
  (make-array 14
    :element-type 'instruction
    :initial-contents
      '(:exit
        :init
        :inc
        :dec
        :add
        :sub
        :label
        :je
        :jne
        :input
        :swap
        :print
        :char
        :rem))
  "Associates with a tally of alternations an instruction identifier by
   utilizing the instruction's index in the agency of the tally.")

;;; -------------------------------------------------------

(defun get-instruction-for (alternations)
  "Returns the ``instruction'' corresponding to the count of
   ALTERNATIONS, signaling an error if the establishment of such an
   association fails."
  (declare (type (integer 0 13) alternations))
  (the instruction
    (if (array-in-bounds-p +INSTRUCTION-TABLE+ alternations)
      (aref +INSTRUCTION-TABLE+ alternations)
      (error "Invalid number of alternations: ~d." alternations))))

;;; -------------------------------------------------------

(defun extract-instructions (code)
  "Extracts from the piece of Binerdy CODE a vector of instructions and
   returns these."
  (declare (type string code))
  (let ((instructions NIL)
        (alternations 0))
    (declare (type (list-of instruction) instructions))
    (declare (type (integer 0 *)         alternations))
    (if (plusp (length code))
      (flet ((evaluate-alternations ()
              "Indagates the current ALTERNATIONS and prepends to the
               INSTRUCTIONS list the associated instruction, returning no
               value.
               ---
               An error is signaled if the ALTERNATIONS does not associate
               with any valid command."
              (push (get-instruction-for alternations) instructions)
              (values)))
        (loop
          for previous-bit
            of-type (or null character)
            =       NIL
            then    current-bit
          for current-bit
            of-type (or null character)
            across  code
          for position
            of-type fixnum
            from    0
          do
            (cond
              ;; The CURRENT-BIT is an invalid character?
              ;; => Signal an error.
              ((not (find current-bit "01" :test #'char=))
                (error "Invalid character ~s at position ~d."
                  current-bit position))
              ;; No PREVIOUS-BIT exists?
              ;; => Start the first group.
              ((null previous-bit)
                (incf alternations))
              ;; The PREVIOUS-BIT and CURRENT-BIT differ?
              ;; => Increase the group size.
              ((char/= previous-bit current-bit)
                (incf alternations))
              ;; The PREVIOUS-BIT and CURRENT-BIT are equal?
              ;; => Evaluate the current group, then start a new one.
              (T
                (evaluate-alternations)
                (setf alternations 1)))
          ;; Evaluate the desinent group, not terminated by two identical
          ;; bits in succession, but by the end of the CODE.
          finally
            (evaluate-alternations)))
      (push :exit instructions))
    (the (simple-array instruction (*))
      (coerce (nreverse instructions)
        '(simple-array instruction (*))))))

;;; -------------------------------------------------------

(defun process-instructions (instructions)
  "Processes the Binerdy INSTRUCTIONS and returns no value."
  (declare (type (vector instruction *) instructions))
  
  (when (plusp (length instructions))
    (let ((ip          0)
          (instruction (aref instructions 0))
          (stack       NIL)
          (labels      (make-hash-table :test #'eql)))
      (declare (type fixnum                       ip))
      (declare (type (or null instruction)        instruction))
      (declare (type (list-of octet)              stack))
      (declare (type (hash-table-of octet fixnum) labels))
      
      (labels
          ((advance ()
            "Moves the instruction pointer IP to the next instruction,
             if possible, updates the current INSTRUCTION, and returns
             no value."
            (setf instruction
              (when (< ip (1- (length instructions)))
                (aref instructions (incf ip))))
            (values))
           
           (move-to (new-ip)
            "Relocates the instruction pointer IP to the NEW-IP, updates
             the current INSTRUCTION, and returns no value."
            (declare (type fixnum ip))
            (setf ip new-ip)
            (setf instruction
              (when (array-in-bounds-p instructions ip)
                (aref instructions ip)))
            (values))
           
           (set-label (label-name label-position)
            "Associates the LABEL-NAME with the instruction pointer
             location LABEL-POSITION and returns no value."
            (declare (type octet  label-name))
            (declare (type fixnum label-position))
            (setf (gethash label-name labels) label-position)
            (values))
           
           (jump-to-label (label-name)
            "Relocates the instruction pointer IP to the instruction
             location associated with the LABEL-NAME and returns no
             value.
             ---
             An error is signaled if the LABEL-NAME cannot be detected
             in the label registry."
            (declare (type octet label-name))
            (multiple-value-bind (label-position contains-label-p)
                (gethash label-name labels)
              (declare (type (or null fixnum) label-position))
              (declare (type T                contains-label-p))
              (if contains-label-p
                (move-to label-position)
                (error "Invalid label name: ~d." label-name)))
            (values))
           
           (wrap-value (value)
            "Returns the VALUE contingently modified to overflow (wrap)
             inside of the range [0, 255]."
            (declare (type integer value))
            (the octet (mod value 256)))
           
           (dump-stack-data ()
            "If the STACK contains one or more elements, these are
             popped and printed in their numeric form, finally returning
             no value.
             ---
             The elements are printed on a dedicated line of their own,
             each two separated by a single space."
            (when stack
              (format T "~&~{~a~^ ~}" stack)
              (setf stack NIL))
            (values)))
        
        (loop do
          (case instruction
            ((NIL)
              (loop-finish))
            
            (:exit
              (loop-finish))
            
            (:init
              (push 0 stack)
              (advance))
            
            (:inc
              (setf (first stack)
                    (wrap-value (1+ (first stack))))
              (advance))
            
            (:dec
              (setf (first stack)
                    (wrap-value (1- (first stack))))
              (advance))
            
            (:add
              (let ((addend (pop stack)))
                (declare (type octet addend))
                (setf (first stack)
                      (wrap-value
                        (+ (first stack)
                           addend))))
              (advance))
            
            (:sub
              (let ((subtrahend (pop stack)))
                (declare (type octet subtrahend))
                (setf (first stack)
                      (wrap-value
                        (- (first stack)
                           subtrahend))))
              (advance))
            
            (:label
              (let ((label-name (pop stack)))
                (declare (type octet label-name))
                (set-label label-name ip))
              (advance))
            
            (:je
              (let ((label-name (pop   stack))
                    (a          (pop   stack))
                    (b          (first stack)))
                (declare (type octet label-name))
                (declare (type octet a))
                (declare (type octet b))
                (cond
                  ((= a b)
                    (jump-to-label label-name)
                    (advance))
                  (T
                    (advance)))))
            
            (:jne
              (let ((label-name (pop   stack))
                    (a          (pop   stack))
                    (b          (first stack)))
                (declare (type octet label-name))
                (declare (type octet a))
                (declare (type octet b))
                (cond
                  ((/= a b)
                    (jump-to-label label-name)
                    (advance))
                  (T
                    (advance)))))
            
            (:input
              (format T "~&Please input an integer: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (push (wrap-value input) stack))
              (advance))
            
            (:swap
              (rotatef (first stack) (second stack))
              (advance))
            
            (:print
              (write-char (code-char (first stack)))
              (advance))
            
            (:char
              (write (first stack))
              (advance))
            
            (:rem
              (pop stack)
              (advance))
            
            (otherwise
              (error "Invalid instruction ~s at position ~d."
                instruction ip))))
        
        (dump-stack-data))))
  
  (values))

;;; -------------------------------------------------------

(defun interpret-Binerdy (code)
  "Interprets the piece of Binerdy CODE and returns no value."
  (declare (type string code))
  (process-instructions
    (extract-instructions code))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Binerdy code generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (hash-table-of instruction (integer 0 13))
               +ALTERNATIONS-TABLE+))

;;; -------------------------------------------------------

(defparameter +ALTERNATIONS-TABLE+ (make-hash-table :test #'eq)
  "Associates each Binerdy instruction with an integer value in the
   range [0, 13], being tantamount to the number of bit alternations
   employed for its representation.")

;;; -------------------------------------------------------

(flet ((add-instruction (instruction alternations)
        "Associates the INSTRUCTION with the number of bit ALTERNATIONS
         and returns no value."
        (declare (type instruction    instruction))
        (declare (type (integer 0 13) alternations))
        (setf (gethash instruction +ALTERNATIONS-TABLE+) alternations)
        (values)))
  (add-instruction :exit  0)
  (add-instruction :init  1)
  (add-instruction :inc   2)
  (add-instruction :dec   3)
  (add-instruction :add   4)
  (add-instruction :sub   5)
  (add-instruction :label 6)
  (add-instruction :je    7)
  (add-instruction :jne   8)
  (add-instruction :input 9)
  (add-instruction :swap  10)
  (add-instruction :print 11)
  (add-instruction :char  12)
  (add-instruction :rem   13)
  (values))

;;; -------------------------------------------------------

(defun get-alternations-for (instruction)
  "Returns the number of alternations necessary for representing the
   INSTRUCTION.
   ---
   An error is signaled if the INSTRUCTION does not represent a
   recognized instruction identifier."
  (declare (type instruction instruction))
  (the (integer 0 13)
    (or
      (gethash instruction +ALTERNATIONS-TABLE+)
      (error "Invalid instruction: ~a." instruction))))

;;; -------------------------------------------------------

(defun generate-Binerdy-code (instructions
                              &key (start-bit   0)
                                   (destination T))
  (declare (type (vector instruction *) instructions))
  (declare (type bit                    start-bit))
  (declare (type destination            destination))
  
  (the (or null string)
    (if destination
      (let ((current-bit start-bit))
        (declare (type bit current-bit))
        
        (labels
            ((flip-bit ()
              (setf current-bit (- 1 current-bit))
              (values))
             
             (write-bit ()
              (format destination "~d" current-bit)
              (values))
             
             (write-alternations (alternations)
              (declare (type (integer 0 13) alternations))
              (loop for bit-index from 1 to alternations do
                (write-bit)
                (when (< bit-index alternations)
                  (flip-bit)))
              (values)))
          
          (loop
            for instruction
              of-type instruction
              across  instructions
            do
              (write-alternations
                (get-alternations-for instruction)))))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-Binerdy-code instructions
          :start-bit   start-bit
          :destination output)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello,_World!".
(interpret-Binerdy "001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001010101010011001100110011001100110011001100110011001100110011001100110101010101100110011001100101010101001010101010011001101010101011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011011010101010110110110110110110110110110110110110110101010101100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110011001100110010101010100100100100100100100100100101010101001100110101010101101101101101101101101010101011011011011011011011011011010101010110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110110101010101101101101101101101101101101101101101101101101101101101101101101101101101010101011010101010101")

;;; -------------------------------------------------------

;; Quine.
(interpret-Binerdy "0")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-Binerdy "1010101011010101010100101010101010")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which terminates if the user inputs
;; a value whose modulus produces zero.
(interpret-Binerdy "00101011010101011010101010100001010101")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which never terminates.
(interpret-Binerdy "001010110101010110101010101001010101010100000101010")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Binerdy "0101010100010101101010101010001110101011010101010101")

;;; -------------------------------------------------------

;; Generate the Binerdy code for reproducing the one-time cat program
;;   0101010100101010101011010101010101
;; and write it to the standard output.
(generate-Binerdy-code
  (coerce
    '(:input
      :char
      :rem)
    '(vector instruction *)))

;;; -------------------------------------------------------

;; Generate the Binerdy code for reproducing the one-time cat program
;;   1010101011010101010100101010101010
;; starting with a bit of 1, and write it to the standard output.
(generate-Binerdy-code
  (coerce
    '(:input
      :char
      :rem)
    '(vector instruction *))
  :start-bit 1)

;;; -------------------------------------------------------

;; Generate the Binerdy code for reproducing the one-time cat program
;;   0101010100101010101011010101010101
;; write it to a fresh string, and interpret the same as a program.
(interpret-Binerdy
  (generate-Binerdy-code
    (coerce
      '(:input
        :char
        :rem)
      '(vector instruction *))
    :destination NIL))

;;; -------------------------------------------------------

;; Generate the Binerdy code for an infinitely repeating cat program
;; which terminates if the user inputs a value whose modulus produces
;; a zero:
;;   0010101101010101101010101010010101010101000101010
;; Write it to a fresh string, and interpret the same as a program.
(interpret-Binerdy
  (generate-Binerdy-code
    (coerce
      '(:init
        :label
        :input
        :char
        :init
        :init
        :jne)
      '(vector instruction *))
    :destination NIL))

;;; -------------------------------------------------------

;; Generate the Binerdy code for an infinitely repeating cat program
;; which never terminates:
;;   0010101101010101101010101010010101010101000101010
;; Write it to a fresh string, and interpret the same as a program.
(interpret-Binerdy
  (generate-Binerdy-code
    (coerce
      '(:init
        :label
        :input
        :char
        :rem
        :init
        :init
        :init
        :je)
      '(vector instruction *))
    :destination NIL))

;;; -------------------------------------------------------

;; Generate the Binerdy code for a truth-machine:
;;   0101010100010101101010101010001110101011010101010101
;; Write it to a fresh string, and interpret the same as a program.
;; 
;; Please note that stack content adhibited as an apostil to the
;; instruction sequence.
(interpret-Binerdy
  (generate-Binerdy-code
    (coerce
      '(:input        ;; c
        :init         ;; 0 c
        :label        ;; c
        :char         ;; c
        :init         ;; 0 c
        :inc          ;; 1 c
        :init         ;; 0 1 c
        :je           ;; c
        :rem          ;; c
       )
      '(vector instruction *))
    :destination NIL))
