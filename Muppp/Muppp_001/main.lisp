;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "Muppp", invented by the Esolang user "Hex96".
;; 
;; Concept
;; =======
;; Muppp operates on a stack, while the instructions are derived from
;; whitespace-separated words whose length, not content, determines the
;; identity. Basic arithmetic operations as well as facilities for input
;; and output exist.
;; 
;; == INSTRUCTIONS ARE ENCODED IN WORD LENGTHS ==
;; A Muppp program consists of words separated by one or more whitespace
;; characters, the words tolerating as their constituents any
;; non-separator entity. These tokens themselves do not carry any
;; semantic value; instead their length, tallied by the number of
;; characters, determines the significance. Ultimately, a piece of Muppp
;; code comprises a sequence of zero or more positive integer numbers
;; in the range [1, +infinity], with every such number n substituted by
;; an arbitrary string of n non-whitespace characters.
;; 
;; == A STACK MAINTAINS THE PROGRAM DATA ==
;; The program data is stored in a stack, its elements being integer
;; values of unrestricted expansion along both the positive and the
;; negative axis. However, if data ought to be displayed, the ASCII
;; character range of [0, 255] must be respected.
;; 
;; 
;; Architecture
;; ============
;; Programs in this language operate on a stack of integer numbers not
;; confined to any specific boundary on either laterality of the gamut.
;; This last-in-first-out (LIFO) structure is required to supply, apart
;; from the default potentials of adding to the top ("push"), removing
;; from same location ("pop"), and querying that position ("peek"), to
;; indagate the second element without removing, that is, "peeking" to
;; a depth of two.
;; 
;; 
;; Data Types
;; ==========
;; Muppp operates on integer data, while communications between the
;; system and the user are relayed to character entities. A stack
;; manages instances of the former type in order to ensure the program's
;; operation.
;; 
;; == INTEGERS ENJOY A PARAMOUNT ROLE ==
;; The primary data type manifests as the unbounded integer, an occupant
;; of the central stack instance as well as the object of manipulating
;; actions. In its specialized form as a positive value the integer type
;; designates the word lengths, also known in this language as "tokens".
;; 
;; == CHARACTERS INTERFACE WITH THE USER ==
;; Of secondary importance, character objects are in currency merely
;; when relating to input and output operations, which involves the
;; reception of user input in character form and the printing of
;; character data to the standard output. In both cases the internally
;; managed state resolves to an integer representation, and only the
;; interfacing to the user utilizes the character type.
;; 
;; == STACK ==
;; As mentioned before, the central data repository is established by
;; the stack, an object whose handling the language relegates to the
;; the various instructions.
;; 
;; 
;; Syntax
;; ======
;; A program consists of a sequence of arbitrary non-whitespace
;; characters representing by their tally the significant tokens, and
;; separated by whitespaces. The character set is restricted to the
;; ASCII repertoire.
;; 
;; Muppp's grammar can be explained by the following Extended
;; Backus-Naur Form (EBNF) specification:
;; 
;;   program       := [ whitespaces ] ,
;;                    command ,
;;                    { whitespaces , command } ,
;;                    [ whitespaces ] ;
;;   command       := nonWhitespace , { nonWhitespace }  ;
;;   nonWhitespace := asciiCharacter - whitespace ;
;;   whitespaces   := whitespace , { whitespace } ;
;;   whitespace    := " " | "\n" | "\t" ;
;; 
;; 
;; Instructions
;; ============
;; The Muppp language's representation of instructions and arguments
;; partakes of a very peculiar nature in that neither the token names
;; expose their identity, nor do demarcations find imposition as
;; constituents of the syntax, in lieu of these provisions the token
;; value is encoded in the length of a word. Such an always positive
;; integer number may either be an instruction or, if a dependence upon
;; further information requires such, the parameter value succeeding the
;; appertaining operation. The following table expresses the
;; relationship betwixt a word length and the associated command:
;; 
;;   Word length | Effect
;;   ------------+-----------------------------------------------------
;;    3          | Pops the top element from the stack.
;;   ..................................................................
;;    4          | Consumes the subsequent word and pushes its length
;;               | onto the stack. Naturally, the thus utilized word is
;;               | not processed again.
;;   ..................................................................
;;    5          | Requests from the user an ASCII character and pushes
;;               | its character code onto the stack.
;;   ..................................................................
;;    6          | Peeks the two topmost stack elements and compares
;;               | them; if they are equal, the value one (1) is pushed
;;               | unto the stack, otherwise a zero (0) is pushed.
;;   ..................................................................
;;    7          | Peeks the topmost stack element. If this element is
;;               | greater than zero, the subsequent word is consumed
;;               | and its length determines the number of spaces to
;;               | move backward in the code from the start position
;;               | of this instruction. If the topmost stack element
;;               | is less than or equal to zero, this instruction is
;;               | simply ignored, with the subsequent word being used
;;               | in the usual fashion.
;;   ..................................................................
;;    8          | Consumes the subsequent word and increments the
;;               | topmost stack element by the word's length, that is:
;;               |   stack.top = stack.top + length(nextWord)
;;   ..................................................................
;;    9          | Consumes the subsequent word and decrements the
;;               | topmost stack element by the word's length, that is:
;;               |   stack.top = stack.top - length(nextWord)
;;   ..................................................................
;;    10         | Consumes the subsequent word and multiplies the
;;               | topmost stack element by the word's length, that is:
;;               |   stack.top = stack.top * length(nextWord)
;;   ..................................................................
;;    11         | Consumes the subsequent word and divides the
;;               | topmost stack element by the word's length, that is:
;;               |   stack.top = stack.top / length(nextWord)
;;   ..................................................................
;;    12         | Consumes the subsequent word and sets the topmost
;;               | stack element to the rest of the division by the
;;               | word's length, that is:
;;               |   stack.top = stack.top modulo length(nextWord)
;;   ..................................................................
;;    13         | Peeks the topmost stack element. If this element is
;;               | greater than zero, the subsequent word is consumed
;;               | and its length determines the number of spaces to
;;               | move forward in the code from the end position of the
;;               | just consumed word. If the topmost stack element
;;               | is less than or equal to zero, this instruction is
;;               | simply ignored, with the subsequent word being used
;;               | in the usual fashion.
;;   ..................................................................
;;    14         | Prints the ASCII character associated with the value
;;               | of the topmost stack element to the standard output.
;;   ..................................................................
;;    15         | Duplicates the topmost stack element by pushing it
;;               | to the top.
;;   ..................................................................
;;    16         | Terminates the program.
;; 
;; 
;; Implementation
;; ==============
;; When implementing an interpreter for the Muppp language the approach
;; can be bifurcated into two modes, the first resolving to a direct and
;; repeated operation upon the source code; the second admitting the
;; avail of a mesothetic interpretation of the program as a list of the
;; word lengths as positive integer tokens, each twain segregated by a
;; single space, and superseding the original code representation in all
;; further uses.
;; 
;; == OPERATING CODE DIRECTLY: PLAIN BUT STRENUOUS ==
;; The per saltum application of the code defeats the incipient
;; convolutions imposed by intermediate formulations. The interpreter
;; consumes the supplied characters and discovers their semantics
;; ad hoc.
;; 
;; A processor dedicated to this concept circumvents the typical costs
;; levied by transcription and materialization into other forms, while
;; benefitting from the acquaintance with character-based approaches
;; universal to similar languages, for instance the esoteric programming
;; language "brainfuck".
;; 
;; However, this refusal of introducing a reformulation and exercising
;; a normalization is usually defrayed with its own set of predicaments.
;; In particular, the tally of word constituents must be recomputated
;; upon each encounter, even if having been of avail before. The lack
;; of whitespace normalization contributes to additional investments
;; being mandated in both the usual consumption of the code and the
;; skipping of tokens, as is the case with the commands ``7'' and
;; ``13'', the two of which define one space as a coherent unit of
;; adjacent whitespaces while navigating back or forth in an iterative
;; manner. In addition to these cumbrances, the source code frequently
;; consists of conspicuously more elements --- its characters --- than a
;; sequence holding the integer-typed sizes, which compresses words and
;; removes whitespaces. Movements across the original sequence are thus
;; more expensive than such performed upon the smaller adminicular
;; structure.
;; 
;; == WORD LENGTH SEQUENCES: COMPACTNESS PAYED IN MEMORY ==
;; As an alternative to the direct source code operation, the
;; significant constituents of the program, its words and the separating
;; whitespaces, might be analyzed, and the length of the former
;; gathered in a dedicated sequence. This manifestation of the code
;; exploits the fact that, imprimis, the tokens do not bear any
;; identifying characteristic except for their length, and, secondary,
;; the type and tally of separating spaces is negligible. To do so, each
;; sequence of adjacent letters is coalesced into a single positive
;; integer number tantamount to the token length, while consecutive
;; whitespaces contract into a single representative of a sepiment,
;; usually a simple space character, which, of no further employment,
;; will be discarded as non-collectible information. 
;; 
;; A representation of the code as a sequence of its word lengths with
;; normalized sepiments relieves the programmer of various intricacies
;; commorant in the direct version. Apart from the reduced program size,
;; the commands and arguments retain a state of precomputation,
;; obviating tautology in discovery and measurement. Navigating in this
;; environment designs an example of pragmatism, as commands and their
;; arguments are the collected items, whereas spaces are tacitly
;; interspersed as the distinguishment betwixt two consecutive objects.
;; 
;; There is, of course, a cost incorporated into the translation from
;; the original source code to the materialized destination structure,
;; and a further imposed by the residence and castaldy of this form in
;; the memory, the ultimate penalty of that remains a variable of the
;; implemented programming language itself. In Common Lisp adjustable
;; vectors --- as one means of gathering the token lengths --- depend
;; in their efficiency upon the concrete language implementation,
;; but are nearly mandatorily detrimental to the performance when
;; juxtaposed to their static counterparts.
;; 
;; == PLAINLESS PREVAILS IN THIS IMPLEMENTATION ==
;; This concrete implementation delegates its operation to the first
;; variant, that is, the direct handling of the source code, an exercise
;; eschewing adscititious structures in order to expose the underlying
;; concepts of the Muppp programming language without indirections.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-11-22
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/Muppp"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stack (&optional (element-type T))
  "The ``stack'' type defines a list-based stack of zero or more
   elements which are all of the ELEMENT-TYPE, defaulting to ``T''."
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

(deftype token ()
  "The ``token'' type defines a tally of zero or more adjacent,
   non-whitespace characters, agnominated as a \"token\" in the Muppp
   specification."
  '(integer 0 *))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Muppp (code)
  "Interprets the Muppp CODE and returns no value."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0)))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      
      (labels
          ((advance ()
            "Moves the POSITION one character forward, if possible, and
             returns no value."
            (if (< position (1- (length code)))
              (setf character (char code (incf position)))
              (setf character NIL))
            (values))
           
           (recede ()
            "Moves the POSITION one character backward, if possible, and
             returns no value."
            (when (plusp position)
              (setf character (char code (decf position))))
            (values))
           
           (move-to (new-position)
            "Moves the POSITION to the NEW-POSITION and returns no
             value."
            (declare (type fixnum new-position))
            (setf position  new-position)
            (setf character (char code position))
            (values))
           
           (whitespace-character-p (candidate)
            "Checks whether the CANDIDATE represents a whitespace
             character, returning a ``boolean'' value of ``T'' on
             confirmation or ``NIL'' otherwise."
            (declare (type character candidate))
            (not (null (member candidate '(#\Space #\Tab #\Newline) :test #'char=))))
           
           (skip-whitespaces ()
            "Starting at the current POSITION, moves forward past zero
             or more adjacent whitespaces, and locates the POSITION at
             the first non-whitespace character, returning no value."
            (loop
              while (and character (whitespace-character-p character))
              do    (advance))
            (values))
           
           (skip-preceding-whitespaces ()
            "Starting at the current POSITION, moves backward past zero
             or more adjacent whitespaces, and locates the POSITION at
             the first non-whitespace character, returning no value."
            (loop
              while (and character (whitespace-character-p character))
              do    (recede))
            (values))
           
           (read-word-length ()
            "Starting at the current POSITION, consumes the current
             word and returns its length."
            (the (integer 0 *)
              (loop
                while (and character (not (whitespace-character-p character)))
                count 1
                do    (advance)))))
        
        (let ((stack NIL))
          (declare (type (stack fixnum) stack))
          
          (loop
            with  word-length of-type token = 0
            while character
            do
              (skip-whitespaces)
              (setf word-length (read-word-length))
              
              (case word-length
                
                ;; Handle trailing whitespaces at the end of the CODE.
                (0
                  (loop-finish))
                
                (3
                  (pop stack))
                
                (4
                  (skip-whitespaces)
                  (push (read-word-length) stack))
                
                (5
                  (format T "~&Please input a character: ")
                  (let ((input (read-char)))
                    (declare (type character input))
                    (clear-input)
                    (push (char-code input) stack)))
                
                (6
                  (if (= (first stack) (second stack))
                    (push 1 stack)
                    (push 0 stack)))
                
                (7
                  (when (plusp (first stack))
                    (let ((word-start-position (- position 7)))
                      (declare (type fixnum word-start-position))
                      (skip-whitespaces)
                      (let ((number-of-spaces (read-word-length)))
                        (declare (type token number-of-spaces))
                        (move-to word-start-position)
                        (loop do
                          (cond
                            ((null character)
                              (loop-finish))
                            ((zerop position)
                              (loop-finish))
                            ((whitespace-character-p character)
                              (decf number-of-spaces)
                              (if (plusp number-of-spaces)
                                (skip-preceding-whitespaces)
                                (loop-finish)))
                            (T
                              (recede))))))))
                
                (8
                  (skip-whitespaces)
                  (let ((next-element (read-word-length)))
                    (declare (type token next-element))
                    (incf (first stack) next-element)))
                
                (9
                  (skip-whitespaces)
                  (let ((next-element (read-word-length)))
                    (declare (type token next-element))
                    (decf (first stack) next-element)))
                
                (10
                  (skip-whitespaces)
                  (let ((next-element (read-word-length)))
                    (declare (type token next-element))
                    (setf (first stack)
                          (* (first stack) next-element))))
                
                (11
                  (skip-whitespaces)
                  (let ((next-element (read-word-length)))
                    (declare (type token next-element))
                    (setf (first stack)
                          (round (first stack) next-element))))
                
                (12
                  (skip-whitespaces)
                  (let ((next-element (read-word-length)))
                    (declare (type token next-element))
                    (setf (first stack)
                          (mod (first stack) next-element))))
                
                (13
                  (when (plusp (first stack))
                    (skip-whitespaces)
                    (let ((number-of-spaces (read-word-length)))
                      (declare (type token number-of-spaces))
                      (loop do
                        (cond
                          ((null character)
                            (loop-finish))
                          ((whitespace-character-p character)
                            (skip-whitespaces)
                            (decf number-of-spaces)
                            (when (<= number-of-spaces 0)
                              (loop-finish)))
                          (T
                            (advance)))))))
                
                (14
                  (write-char (code-char (first stack))))
                
                (15
                  (push (first stack) stack))
                
                (16
                  (loop-finish))
                
                (otherwise
                  (error "Invalid word length: ~d." word-length))))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(interpret-Muppp "Hello multiplication")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-Muppp "Input asciiCharacter restart now")

;;; -------------------------------------------------------

;; The above infinitely repeating cat program using hexadecimal digits
;; instead of letters as hints.
(interpret-Muppp "55555 eeeeeeeeeeeeee 7777777 333")

;;; -------------------------------------------------------

;; Truth machine.
;; 5  - input
;; 9  - subtract 48 from input to obtain numeric value ('0' => 0, '1' => 1, etc.)
;; 4  - Push 1 for subsequent comparison ("6") operation
;; 6  - Compare if numeric user input equals 1, push 1 if so
;; 8  - Increase converted user input by 48 to reacquire ASCII character for printing
;; 14 - Print ASCII user input
;; 9  - Decrease original user input by 48 to acquire numeric value (0/1)
;; 7  - Check if converted input equals 1, repeat at -> 8 if so
(interpret-Muppp
"
55555
999999999 ------------------------------------------------
4444 1
666666

88888888 ++++++++++++++++++++++++++++++++++++++++++++++++ EEEEEEEEEEEEEE 999999999 ------------------------------------------------ 7777777 666666
")

;;; -------------------------------------------------------

;; Print "Hello, World!"
(interpret-Muppp
"
aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa aaaa aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa aaaaaaaaaaaaaa
")
