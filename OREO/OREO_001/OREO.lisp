;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the esoteric programming language "OREO",
;; devised by the Esolang user "Kamish".
;; 
;; Concept
;; =======
;; The OREO programming language belongs to a particular class of
;; esoteric languages known as "joke languages", the members of which
;; are justified primarily or exclusively by virtue of their jocular
;; effects; usefulness accounts for a coincidence at best.
;; 
;; The name OREO is chosen as a euonym in order to allude to the cookie
;; offered by the American company Mondelez International Inc., and
;; renown for its peculiar structure, being tripartite with two dark
;; biscuits surrounding a white disk composed of cream. The capability
;; to disassemble the three constituents in order to produce various
;; compositions of stacks has entered popular culture.
;; 
;; The OREO programming language emulates this craft by associating a
;; triplet of commands, called "RE", "O", and "&O", the first one of
;; which describes the cream, while the latter two represent biscuits.
;; The combination of instructions forming a program constructs phrases
;; conceptually visualizing the kinds of various Oreo cookies stackings.
;; 
;; 
;; Architecture
;; ============
;; An OREO program consists of an infinite tape of cells, each capable
;; of storing a non-negative integer number of infinitely large size.
;; The cell value is also called its "accumulator", and is initialized
;; to zero. The tape can only be traversed forward, that is, from one
;; cell to its successor, being located dextrally from it.
;; 
;; A pointer is defined on the tape and maintains the currently active
;; cell, with exactly one being in the focus at any time.
;; 
;; In its initial state, the tape consists of a single, zero-valued
;; cell, referenced by the pointer.
;; 
;; 
;; Commands
;; ========
;; The instruction set is composed of three commands only:
;;   
;;   Command | Effect
;;   --------+--------------------------------------------------------
;;   RE      | Increases the value of the currently active cell by 1.
;;   O       | Moves the pointer one position to the right.
;;   &O      | Prints the ASCII character associated with the value of
;;           | the currently active cell.
;; 
;; Obviously, the language is very restricted in the circumference of
;; its capabilities, most conspicuously lacking any input mechanism.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The language specifications leaves several questions unanswered,
;; including, but not limited to, the following:
;;   (1) How is the initial program state defined?
;;       o Most importantly, does an initial tape cell exist or ought
;;         such be prepared using the "O" command?
;;       o The provided example prepends its code with an "O" command.
;;   (2) Does an imposition on the valid range for a cell value exist?
;;       o Provided that the initial cell value equals zero, the only
;;         modifying command, "RE", increases the accumulator, thus
;;         excluding negative integers from the gamut.
;;       o However, by specifying that the cell value can be printed as
;;         an ASCII character, it is suggested that a range of [0, 255]
;;         may not be violated for this particular purpose.
;;       o Several possibilities are tenable, for instance:
;;         (a) Clamping the accumulator, that is, preventing it from
;;             exceeding the maximum ASCII character code 255.
;;         (b) Wrapping the accumulator, that is, resetting its value to
;;             zero upon transgression of the ASCII maximum 255.
;;         (c) Permitting an unbridled increase in the accumulator, thus
;;             encumbering the programmer with monitoring its sanity.
;;   (3) Are whitespace characters, including spaces, tabs, and
;;       newlines, homologated in the source code?
;;       o The only extant example, "Hello, world!", suggests such.
;; 
;; In response to these shortcomings, the following amendments have been
;; adjudged for the specification's completion:
;;   (1) The program starts with a single zero-valued cell, set as the
;;       active one, with any new cell sharing this minimum.
;;   (2) The cell value is not restricted to any range and is hence
;;       allowed to rise infinitely. The obviation of conflicts with the
;;       ASCII character code range constitute the programmer's onus.
;;   (3) Whitespaces betwixt commands are permitted, which include the
;;       following:
;;         - Space
;;         - Tab
;;         - Newline
;;         - Carriage return
;;         - Linefeed
;;       Any other character is inflicted with proscription.
;; 
;; 
;; Implementation
;; ==============
;; This implementation simulates the tape as a dynamic vector of
;; non-negative integer numbers of unbounded magnitude. An epiphenomenon
;; of this choice, the cell indexing is implicitly established as
;; zero-based, albeit no inquisitions nor effects emanate thereform.
;; Infinity in the tape length is simulated by appending a new
;; zero-valued element on each invocation of the move command "O".
;; 
;; A pointer to the current tape cell permits operations upon the
;; referenced location as well as navigations.
;; 
;; In its incipient state, the tape cell contains a single zero-valued
;; element, pointed to by the cursor.
;; 
;; A motion to the right, being the only possible direction for
;; relocation, appends a new element of the value zero to the sequence
;; tail. The pointer immediately moves to the newly introduced position.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-09-25
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/OREO"
;;   -> "https://esolangs.org/wiki/Joke_language_list"
;;       o A list of joke languages maintained by the Esolang community.
;;   -> "https://www.oreo.com/"
;;       o Homepage of the Oreo confectionary brand.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-OREO (code)
  "Executes the OREO code and returns ``NIL''."
  (declare (type string code))
  (let ((memory  (make-array 0
                   :element-type '(integer 0 *)
                   :adjustable   T
                   :fill-pointer 0))
        (pointer 0))
    (declare (type (vector (integer 0 *) *) memory))
    (declare (type fixnum                   pointer))
    (vector-push-extend 0 memory)
    (with-input-from-string (input code)
      (declare (type string-stream input))
      (loop
        for   character of-type (or null character) = (read-char input NIL NIL NIL)
        while character
        do
        (case character
          (#\R
            (let ((next-character (read-char input NIL NIL NIL)))
              (declare (type (or null character) next-character))
              (unless (char= next-character #\E)
                (error "Expected 'RE', but found 'R~a'" next-character)))
            (incf (aref memory pointer)))
          
          (#\O
            (setf pointer (fill-pointer memory))
            (vector-push-extend 0 memory))
          
          (#\&
            (let ((next-character (read-char input NIL NIL NIL)))
              (declare (type (or null character) next-character))
              (unless (char= next-character #\O)
                (error "Expected '&O', but found '&~a'" next-character)))
            (write-char (code-char (aref memory pointer))))
          
          ((#\Newline #\Space #\Tab)
            NIL)
          
          (otherwise
            (error "Invalid character ~s." character)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of additional operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-to-OREO (text &optional (destination NIL))
  "Writes to the DESTINATION a sequence of OREO commands which, when
   executed in an interpreter, will print the exact TEXT.
   ---
   If no DESTINATION is specified, a new string is created and returned
   containing the OREO instructions.
   ---
   Note that, for purposes of aesthetics, an actually superfluous \"O\"
   command will prepended, designing a text which more likely results
   in a form suggestive of the word \"OREO\".
   ---
   Examples:
     ;; Creates and returns a new string containing the OREO commands.
     (print-to-OREO \"Hello, world!\")
     
     ;; Prints to the standard output (console) the OREO commands.
     (print-to-OREO \"Hello, world!\" T)"
  (declare (type string                          text))
  (declare (type (or null (eql T) stream string) destination))
  (if destination
    (loop for character of-type character across text do
      (format destination "~&O")
      (loop repeat (char-code character) do
        (format destination "RE"))
      (format destination "&O"))
    (with-output-to-string (output)
      (declare (type string-stream output))
      (print-to-OREO text output))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original example.
;; Print: "HELLOOWORLD!"
(interpret-OREO "ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O")

;;; -------------------------------------------------------

;; Print "HELLO, WORLD!"
(interpret-OREO
"
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O

O
RERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE
&O

O
RERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE
&O

ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
")

;;; -------------------------------------------------------

(interpret-OREO
"ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O
ORERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERERE&O")

;;; -------------------------------------------------------

(print-to-OREO "Hello, world!")
