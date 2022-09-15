;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "gtltem", invented by the Esolang user "qwerty12302" in the
;; year 2015, and based upon a seven-bit memory which, by incrementing
;; and decrementing, may produce character output according to a
;; dioristic variant of the ASCII standard.
;; 
;; Concept
;; =======
;; The gtltem programming language operates on a single seven-bit
;; memory, amenable to augmentation and deduction, whose only
;; contribution to utility manifests in the output of characters.
;; 
;; == GTLTEM'S NAME: AN ACCOUNT OF ITS OPERATIONS ==
;; The agnomination "gtltem" serves as a vehicle of its three exclusive
;; characters in use for the definition of programs: the Greater Than
;; symbol (">"), Less Than ("<") token, and Exclamation Mark ("!").
;; 
;; == PROGRAMS OPERATE ON A SINGLE MEMORY DATUM ==
;; The gtltem program memory is realized in a scalar datum known as the
;; memory, a seven-bit storage responding to increment and decrement
;; operations, but destitute of any arithmetical capacities.
;; 
;; == GTLTEM CONSTITUTES AN OUTPUT-ONLY LANGUAGE ==
;; In the face of its modesty in the architectural and operational
;; departments, the question about the language's telos arises. gtltem
;; subsumes into the output-only category of esoteric programming
;; languages --- thus its communication with the user partakes of no
;; further means than text display on the standard output, usually the
;; system's console.
;; 
;; == THE MEMORY: AN ACCUMULATOR FOR PRINTING OPERATIONS ==
;; To this end, the memory must be incremented or decremented until the
;; code of the desiderated character has been modeled, ere a printing
;; command transmits its representation unto the output conduit.
;; 
;; == GTLTEM EMPLOYS A CUSTOM ASCII CHARACTER SET ==
;; A particular variation of the ASCII standard is employed for the
;; numeric encoding of characters, nemned "gtltemASCII". This mapping
;; associates with non-negative integers in the range [0, 94] a subset
;; of ASCII characters eschwing the control entities.
;; 
;; The following table demonstrates the 95 character codes and their
;; correspondences:
;; 
;;   ----------------
;;   Code | Character
;;   -----+----------
;;    0   |  
;;    1   | !
;;    2   | "
;;    3   | #
;;    4   | $
;;    5   | %
;;    6   | &
;;    7   | '
;;    8   | (
;;    9   | )
;;   10   | *
;;   11   | +
;;   12   | ,
;;   13   | -
;;   14   | .
;;   15   | /
;;   16   | 0
;;   17   | 1
;;   18   | 2
;;   19   | 3
;;   20   | 4
;;   21   | 5
;;   22   | 6
;;   23   | 7
;;   24   | 8
;;   25   | 9
;;   26   | :
;;   27   | ;
;;   28   | <
;;   29   | =
;;   30   | >
;;   31   | ?
;;   32   | @
;;   33   | A
;;   34   | B
;;   35   | C
;;   36   | D
;;   37   | E
;;   38   | F
;;   39   | G
;;   40   | H
;;   41   | I
;;   42   | J
;;   43   | K
;;   44   | L
;;   45   | M
;;   46   | N
;;   47   | O
;;   48   | P
;;   49   | Q
;;   50   | R
;;   51   | S
;;   52   | T
;;   53   | U
;;   54   | V
;;   55   | W
;;   56   | X
;;   57   | Y
;;   58   | Z
;;   59   | [
;;   60   | \
;;   61   | ]
;;   62   | ^
;;   63   | _
;;   64   | `
;;   65   | a
;;   66   | b
;;   67   | c
;;   68   | d
;;   69   | e
;;   70   | f
;;   71   | g
;;   72   | h
;;   73   | i
;;   74   | j
;;   75   | k
;;   76   | l
;;   77   | m
;;   78   | n
;;   79   | o
;;   80   | p
;;   81   | q
;;   82   | r
;;   83   | s
;;   84   | t
;;   85   | u
;;   86   | v
;;   87   | w
;;   88   | x
;;   89   | y
;;   90   | z
;;   91   | {
;;   92   | |
;;   93   | }
;;   94   | ~
;;   ----------------
;; 
;; == GTLTEM PROGRAMS: ADJUST THE MEMORY AND PRINT A TEXT ==
;; The scanty potence apportioned to the gtltem programming language,
;; exhausted already in a scalar datum's incrementing, decrementing, and
;; textual display, enforces a particular approach as the mere
;; harnessing of its facilities:
;; 
;;   (1) Increment or decrement the current memory value using the
;;       instructions ">" and "<" until it matches the pursued
;;       gtltemASCII character code.
;;   (2) Print the affiliated character by adminiculum of the "!"
;;       instruction.
;;   (3) Repeat the procedure starting with the step (1) while further
;;       output is optated.
;; 
;; 
;; Architecture
;; ============
;; In regards of its architecture, gtltem resorts to thorough plainness,
;; molding the whole program memory into a 7-bit non-negative integer
;; scalar; a corollary of this, the occupied range of [0, 127]
;; enumerates 2^7 = 128 different elements.
;; 
;; Upon the program's inchoation, the memory is set to zero (0).
;; 
;; 
;; Data Types
;; ==========
;; gtltem's data handling bifurcates into the seven-bit integer specimen
;; as its paravaunt representative, and a dioristic construe of ASCII
;; characters for the output facility.
;; 
;; == 7-BIT INTEGERS SUPPLY THE CHIEF CONTRIBUTION ==
;; The excellent participant in the description of data remains reserved
;; for unsigned integers composed of seven bits, occupants of the
;; naturally produced range [0, 127]. Most importantly, the memory, a
;; scalar salvatory tasked with the castaldy of the program data,
;; maintains a single element from this species, amenable to operations
;; for its manipulation.
;; 
;; == CHARACTERS OPERATE ON THE OUTPUT CONDUIT ==
;; The exclusive involvement of characters, the second and subordinate
;; commorant of gtltem's type system, restricts itself to the display of
;; text on the user's standard output. To this end, the memory's value
;; is construed as the code of a character according to a kenspeckle
;; version of the ASCII standard, the concrete listing of which please
;; peruse in the "Concept" subsection "GTLTEM EMPLOYS A CUSTOM ASCII
;; CHARACTER SET". The thus requested entry value is printed to the
;; output channel.
;; 
;; 
;; Syntax
;; ======
;; The language's donat does not involve a convolute imposition, with
;; three instructions and the tolerance for whitespaces as the sole
;; constituents.
;; 
;; == INSTRUCTIONS: SINGLE CHARACTERS ==
;; Each of the three instructions incorporated into the language, and
;; already exhausted by the set ">", "<", and "!", is represented
;; through a single character. No arguments appertain to their
;; dependencies.
;; 
;; == COMMENTS ==
;; gtltem in its current rendition denies any facilities for the
;; interspersion of comments in the source code.
;; 
;; == WHITESPACES ==
;; Whitespaces, whose diorism includes the space, tab and newline
;; entities, are embraced with tolerance, regardless of their
;; occurrence. Any other content beside instructions and such sepiments
;; remains a victim of interdiction.
;; 
;; == GRAMMAR ==
;; The following Extended Backus-Naur Form (EBNF) applies to the
;; language:
;; 
;;   program    := { command | whitespace } ;
;;   command    := ">" | "<" | "!" ;
;;   whitespace := " " | "\n" | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; gtltem, as an output-only language, combines in its facilities a very
;; modest variety, a treble members' composition that shall be
;; introduced in nexility:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   >       | Increments the memory value by one.
;;   ..................................................................
;;   <       | Decrements the memory value by one.
;;   ..................................................................
;;   !       | Prints to the standard output the gtltemASCII character
;;           | associated with the current memory value.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Maugre its witeless lucidity in description, the original
;; specification is, apprehensibly, inflicted with a few points
;; enshrouded with caligation. The following account shall enumerate the
;; most significant representatives from this set, without claiming its
;; exhaustion.
;; 
;; == HOW DOES THE MEMORY RESPOND ON ITS BOUNDARIES? ==
;; The 7-bit nature of the program memory and its amenability to
;; incremental augmentation and deduction coerces a perquisition of its
;; deportment in the case of its boundaries' pending transgressions.
;; Such circumstances incarnate if either the memory, residing at the
;; lower march of zero (0), is subjected to the hest of a decrementing,
;; or if, located at the maximum of 127, shall be incremented.
;; 
;; Several alternatives may be propounded:
;; 
;;   (1) WRAPPING
;;       The value wraps around upon transgression, that is:
;;       (a) If incrementing a memory of the value 127, the next state
;;           returns to the minimum of zero (0).
;;       (b) If decrementing a memory o the value 0, the next state
;;           relocates to the maximum of 127.
;;   (2) DISPRESPONDENCY
;;       The memory does not engage in any reaction when confronted with
;;       these edges case, retaining the current state while silently
;;       neglecting the committed instructions.
;;   (3) ERROR SIGNALING
;;       Any attempt at transgressions of the boundaries result in an
;;       error, with concomitant termination of the program.
;; 
;; It has been adjudged that the first option (1) proffers the most
;; utible and least alienating behavior, and thus has been incorporated
;; into the implementation. Its nature basically concords with an
;; overflow whose acquaintance may be steadable to the user.
;; 
;; == HOW DOES A PROGRAM REACT TO INVALID CHARACTER CODES? ==
;; Betwixt the memory, which homologates a variety of 128 different
;; values in the 7-bit range [0, 127], and the gtltemASCII character
;; set, whose codes span the 95 entries in the interval [0, 94], an
;; asymmetry governs. The thus transpiring incongruency intrudes into a
;; program during the print operation "!", whose request for a character
;; from the latter set by the former's adminiculum poses an element of
;; dubiosity.
;; 
;; The following ways of alleviation have been probed:
;; 
;;   (1) NO OUTPUT
;;       No output shall be committed, thus silently ignoring the
;;       request.
;;   (2) ERROR SIGNALING
;;       An error shall educate about the discord, while concomitantly
;;       terminating the program.
;; 
;; The second alternative (2) has been chosen as the canonical solution
;; to this predicament, based upon the potential for feedback about a
;; corrupted or unexpected memory state on the user's side.
;; 
;; 
;; Implementation
;; ==============
;; Based upon the gtltem language's utterly simplistic character, this
;; Common Lisp implementation operates directly on the source code,
;; without translations of any sorts.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-14
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Gtltem"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype septet ()
  "The ``septet'' type defines an unsigned byte composed of seven bits."
  '(unsigned-byte 7))

;;; -------------------------------------------------------

(deftype destination ()
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type septet             +MEMORY-LOWER-BOUND+))
(declaim (type septet             +MEMORY-UPPER-BOUND+))
(declaim (type (simple-string 95) +GTLTEMASCII-CHARACTER-TABLE+))

;;; -------------------------------------------------------

(defparameter +MEMORY-LOWER-BOUND+ 0
  "The smallest valid memory value.")

;;; -------------------------------------------------------

(defparameter +MEMORY-UPPER-BOUND+ 127
  "The largest valid memory value.")

;;; -------------------------------------------------------

(defparameter +GTLTEMASCII-CHARACTER-TABLE+
  " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
  "Associates each gtltemASCII character with its character code,
   defined in terms of its zero-based index in the sequence.")

;;; -------------------------------------------------------

(defun get-gtltemASCII-character-for-code (code)
  "Returns the gtltemASCII character associated with the CODE, or
   signals an error of an unspecified type if no correspondence exists."
  (declare (type septet code))
  (the character (schar +GTLTEMASCII-CHARACTER-TABLE+ code)))

;;; -------------------------------------------------------

(defun interpret-gtltem (code)
  "Interprets the piece of gtltem CODE and returns no value."
  (declare (type string code))
  
  (let ((memory 0))
    (declare (type septet memory))
    
    (flet
        ((increment-memory ()
          "Increments the memory by one and returns no value."
          (if (= memory +MEMORY-UPPER-BOUND+)
            (setf memory +MEMORY-LOWER-BOUND+)
            (incf memory))
          (values))
         
         (decrement-memory ()
          "Decrements the memory by one and returns no value."
          (if (= memory +MEMORY-LOWER-BOUND+)
            (setf memory +MEMORY-UPPER-BOUND+)
            (decf memory))
          (values))
         
         (print-memory ()
          "Writes the gtltemASCII character associated with the memory
           as the character code to the standard output and returns no
           value."
          (write-char (get-gtltemASCII-character-for-code memory))
          (values)))
      
      (loop
        for token    of-type character across code
        and position of-type fixnum    from   0
        do
          (case token
            (#\<
              (decrement-memory))
            
            (#\>
              (increment-memory))
            
            (#\!
              (print-memory))
            
            ((#\Space #\Tab #\Newline)
              NIL)
            
            (otherwise
              (error "Invalid character ~s at position ~d."
                token position))))))
  
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of gtltem text generator.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-gtltemASCII-code-for-character (character)
  "Returns the gtltemASCII code for the CHARACTER, or signals an error
   of an unspecified type if the CHARACTER does not represent a member
   of the gtltemASCII character repertoire."
  (declare (type character character))
  (the septet
    (or (position character +GTLTEMASCII-CHARACTER-TABLE+ :test #'char=)
        (error "The character ~s does not exist in the gtltemASCII ~
                repertoire."
          character))))

;;; -------------------------------------------------------

(defun generate-text-program (text
                              &key (destination      NIL)
                                   (output-separator NIL))
  "Generates a gtltem program capable of printing the TEXT to the
   standard output and writes it to the DESTINATION, returning for a
   non-``NIL'' DESTINATION the ``NIL'' value, otherwise responding with
   a freshly created string containing the result.
   ---
   If non-``NIL'', each two output operations will be separated by the
   OUTPUT-SEPARATOR, an arbitrary object whose aesthetical form will
   be written to the DESTINATION."
  (declare (type string      text))
  (declare (type destination destination))
  (declare (type T           output-separator))
  
  (the (or null string)
    (if destination
      (let ((memory 0))
        (declare (type septet memory))
        (flet
            ((adjust-memory (intended-memory)
              "Writes to the DESTINATION a series of instructions which
               increment or decrement the current MEMORY value in a mete
               so as to achive the INTENDED-MEMORY state, and returns no
               value."
              (declare (type septet intended-memory))
              (cond
                ;; The MEMORY must be incremented.
                ((< memory intended-memory)
                  (loop repeat (- intended-memory memory) do
                    (format destination ">"))
                  (setf memory intended-memory))
                ;; The MEMORY must be decremented.
                ((> memory intended-memory)
                  (loop repeat (- memory intended-memory) do
                    (format destination "<"))
                  (setf memory intended-memory))
                ;; The MEMORY does not change.
                (T
                  NIL))
              (values))
             
             (print-character ()
              "Writes to the DESTINATION the instruction for printing
               the gtltem character associated with the current MEMORY
               state to the standard output, and returns no value."
              (format destination "!")
              (values))
             
             (write-output-separator ()
              "If specified, writes the OUTPUT-SEPARATOR to the
               DESTINATION and returns no value."
              (when output-separator
                (format destination "~a" output-separator))
              (values)))
          
          (loop
            for character         of-type character across text
            for first-character-p of-type boolean   = T then NIL
            do
              (unless first-character-p
                (write-output-separator))
              
              ;; Build the current CHARACTER's gtltemASCII code in the
              ;; MEMORY.
              (adjust-memory
                (get-gtltemASCII-code-for-character character))
              ;; Print the character associated with the MEMORY's value
              ;; to the standard output.
              (print-character))))
      
      (with-output-to-string (output)
        (declare (type string-stream output))
        (generate-text-program text
          :destination      output
          :output-separator output-separator)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print the text "ABC".
(interpret-gtltem ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>!>!")

;;; -------------------------------------------------------

(interpret-gtltem ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>!!>>>!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!<<<<<<<<<<<<!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>!>>>!<<<<<<!<<<<<<<<!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!")

;;; -------------------------------------------------------

;; Generate a gtltem program capable of printing the text
;; "Hello, World!" and write it to the standard output.
;; The output constitutes
;;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>!!>>>!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!<<<<<<<<<<<<!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>!>>>!<<<<<<!<<<<<<<<!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
(generate-text-program "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Generate a gtltem program capable of printing the text
;; "Hello, World!" and write it to the standard output, demarcating each
;; two print operations by a newline character.
;; The output constitutes
;;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
;;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
;;   >>>>>>>!
;;   !
;;   >>>!
;;   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
;;   <<<<<<<<<<<<!
;;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
;;   >>>>>>>>>>>>>>>>>>>>>>>>!
;;   >>>!
;;   <<<<<<!
;;   <<<<<<<<!
;;   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
(generate-text-program "Hello, World!"
  :destination      T
  :output-separator #\Newline)

;;; -------------------------------------------------------

;; Generate a gtltem program capable of printing the text
;; "Hello, World!" and interpret it.
;; The thus produced program constitutes
;;   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>!!>>>!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!<<<<<<<<<<<<!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!>>>>>>>>>>>>>>>>>>>>>>>>!>>>!<<<<<<!<<<<<<<<!<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!
(interpret-gtltem
  (generate-text-program "Hello, World!"))
