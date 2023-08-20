;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "QWERTY Keyboard Dot Language", conceived by the Esolang
;; user "Zzo38" and presented on December 18th, 2006, the same offers a
;; scheme accommodated to the printing of message only, the characters
;; being communicated by their positions on the keyboard, encoded as a
;; sequence of dots (".") or commas (",").
;; 
;; 
;; Concept
;; =======
;; The QWERTY Keyboard Dot Language is founded upon the assignment of
;; one-based indices to printable characters in reference to their
;; location on the keyboard, their tally is encoded in a series of dots
;; (".") or commas (",").
;; 
;; A bivial exposition applying to the keyboard's configurations, namely
;; the differentiating nature of modifiers, reverberates in the choice
;; of encoding: Characters accessible without a modifier answer to a
;; series of dots (".") tantamount to their one-based position; whereas
;; those entities amenable to the shift key respond to commas (",").
;; 
;; Ensuing from the above elucidated diorisms, the following
;; correspondences hold for a non-modified key's position (see the
;; "Pos." column), its represented character (see the "Char.") column,
;; and the encoding:
;; 
;;   ----------------------------------------------------------------
;;   Pos. | Char. | Encoding
;;   -----+-------+--------------------------------------------------
;;   1    | `     | .
;;   2    | 1     | ..
;;   3    | 2     | ...
;;   4    | 3     | ....
;;   5    | 4     | .....
;;   6    | 5     | ......
;;   7    | 6     | .......
;;   8    | 7     | ........
;;   9    | 8     | .........
;;   10   | 9     | ..........
;;   11   | 0     | ...........
;;   12   | -     | ............
;;   13   | =     | .............
;;   14   | TAB   | ..............
;;   15   | q     | ...............
;;   16   | w     | ................
;;   17   | e     | .................
;;   18   | r     | ..................
;;   19   | t     | ...................
;;   20   | y     | ....................
;;   21   | u     | .....................
;;   22   | i     | ......................
;;   23   | o     | .......................
;;   24   | p     | ........................
;;   25   | [     | .........................
;;   26   | ]     | ..........................
;;   27   | a     | ...........................
;;   28   | s     | ............................
;;   29   | d     | .............................
;;   30   | f     | ..............................
;;   31   | g     | ...............................
;;   32   | h     | ................................
;;   33   | j     | .................................
;;   34   | k     | ..................................
;;   35   | l     | ...................................
;;   36   | ;     | ....................................
;;   37   | '     | .....................................
;;   38   | \     | ......................................
;;   39   | z     | .......................................
;;   40   | x     | ........................................
;;   41   | c     | .........................................
;;   42   | v     | ..........................................
;;   43   | b     | ...........................................
;;   44   | n     | ............................................
;;   45   | m     | .............................................
;;   46   | ,     | ..............................................
;;   47   | .     | ...............................................
;;   48   | /     | ................................................
;;   49   | SPACE | .................................................
;;   ----------------------------------------------------------------
;;   
;; Siclike, the associations for a shifted keyboard assume:
;;   
;;   ----------------------------------------------------------------
;;   Pos. | Char. | Encoding
;;   -----+-------+--------------------------------------------------
;;   1    | ~     | ,
;;   2    | !     | ,,
;;   3    | @     | ,,,
;;   4    | #     | ,,,,
;;   5    | $     | ,,,,,
;;   6    | %     | ,,,,,,
;;   7    | ^     | ,,,,,,,
;;   8    | &     | ,,,,,,,,
;;   9    | *     | ,,,,,,,,,
;;   10   | (     | ,,,,,,,,,,
;;   11   | )     | ,,,,,,,,,,,
;;   12   | _     | ,,,,,,,,,,,,
;;   13   | +     | ,,,,,,,,,,,,,
;;   14   | TAB   | ,,,,,,,,,,,,,,
;;   15   | Q     | ,,,,,,,,,,,,,,,
;;   16   | W     | ,,,,,,,,,,,,,,,,
;;   17   | E     | ,,,,,,,,,,,,,,,,,
;;   18   | R     | ,,,,,,,,,,,,,,,,,,
;;   19   | T     | ,,,,,,,,,,,,,,,,,,,
;;   20   | Y     | ,,,,,,,,,,,,,,,,,,,,
;;   21   | U     | ,,,,,,,,,,,,,,,,,,,,,
;;   22   | I     | ,,,,,,,,,,,,,,,,,,,,,,
;;   23   | O     | ,,,,,,,,,,,,,,,,,,,,,,,
;;   24   | P     | ,,,,,,,,,,,,,,,,,,,,,,,,
;;   25   | {     | ,,,,,,,,,,,,,,,,,,,,,,,,,
;;   26   | }     | ,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   27   | A     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   28   | S     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   29   | D     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   30   | F     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   31   | G     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   32   | H     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   33   | J     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   34   | K     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   35   | L     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   36   | :     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   37   | "     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   38   | |     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   39   | Z     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   40   | X     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   41   | C     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   42   | V     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   43   | B     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   44   | N     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   45   | M     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   46   | <     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   47   | >     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   48   | ?     | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   49   | SPACE | ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
;;   ----------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-21
;; 
;; Sources:
;;   [esolang2009QWERTYKDL]
;;   The Esolang contributors, "QWERTY Keyboard Dot Language",
;;     January 18th, 2009
;;   URL: "https://esolangs.org/wiki/QWERTY_Keyboard_Dot_Language"
;;   
;;   [DasKeyboardStaff2020QWERTY]
;;   Das Keyboard Staff,
;;     "QWERTY vs. Dvorak vs. Colemak Keyboard Layouts",
;;     November 2nd, 2020
;;   URL: "https://www.daskeyboard.com/blog/
;;         qwerty-vs-dvorak-vs-colemak-keyboard-layouts/"
;;   Notes:
;;     - Illustrates various keyboard layouts, including QWERTY.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype keyboard ()
  "The ``keyboard'' type defines a physical keyboard's virtual
   representation as a simple string of 49 elements, thus tallying its
   keys."
  '(simple-string 49))

;;; -------------------------------------------------------

(deftype key-position ()
  "The ``key-position'' type defines one-based location of a keyboard
   key, represented by an integer value in the closed range [1, 49]."
  '(integer 1 49))

;;; -------------------------------------------------------

(deftype modifier ()
  "The ``modifier'' type defines the possible key modifiers for use in a
   keyboard."
  '(member :none :shift))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for writing operations,
   including, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of keyboard layout.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type keyboard +NORMAL-KEYBOARD+))
(declaim (type keyboard +SHIFT-KEYBOARD+))

;;; -------------------------------------------------------

(defparameter +NORMAL-KEYBOARD+
  (make-array 49 :element-type 'character :initial-contents
    (vector
      #\` #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\- #\=
      #\Tab #\q #\w #\e #\r #\t #\y #\u #\i #\o #\p #\[ #\]
      #\a #\s #\d #\f #\g #\h #\j #\k #\l #\; #\' #\\
      #\z #\x #\c #\v #\b #\n #\m #\, #\. #\/
      #\Space))
  "Maps each computer keyboard key without any modifier to a zero-based
   position on the device, tallied among the printable characters.")

;;; -------------------------------------------------------

(defparameter +SHIFT-KEYBOARD+
  (make-array 49 :element-type 'character :initial-contents
    (vector
      #\~ #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\_ #\+ 
      #\Tab #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\{ #\} 
      #\A #\S #\D #\F #\G #\H #\J #\K #\L #\: #\" #\| 
      #\Z #\X #\C #\V #\B #\N #\M #\< #\> #\? 
      #\Space))
  "Maps each computer keyboard key applied to the shift key to a
   zero-based position on the device, tallied among the printable
   characters.")

;;; -------------------------------------------------------

(defun get-keyboard-character (key-position &key (modifier :none))
  "Returns the character answering to the KEY-POSITION under the
   influence of the MODIFIER, which defaults to ``:none''."
  (declare (type key-position key-position))
  (declare (type modifier     modifier))
  (flet ((check-key-position (keyboard)
          "Checks whether the KEY-POSITION is recognized as a valid
           position in the KEYBOARD, throwing an error on failure, and
           returning no value if permissive."
          (declare (type keyboard keyboard))
          (unless (<= 1 key-position (length keyboard))
            (error "Invalid key position: ~d. Expected a value in the ~
                    range [1, ~d]."
              key-position (length keyboard)))
          (values)))
    (the character
      (case modifier
        (:none
          (check-key-position +NORMAL-KEYBOARD+)
          (aref +NORMAL-KEYBOARD+ (1- key-position)))
        (:shift
          (check-key-position +SHIFT-KEYBOARD+)
          (aref +SHIFT-KEYBOARD+ (1- key-position)))
        (otherwise
          (error "Invalid MODIFIER: ~s." modifier))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (subject)
  "Checks whether the SUBJECT represents a whitespace character,
   returning a ``boolean'' value of ``T'' on confirmation, or ``NIL'' on
   a mismatch."
  (declare (type character subject))
  (the boolean
    (not (null
      (member subject
        '(#\Linefeed #\Newline #\Return #\Space #\Tab)
        :test #'char=)))))

;;; -------------------------------------------------------

(defun interpret-QWERTY-Keyboard-Dot-Language (code
                                               &key (destination T))
  "Interprets the piece of QWERTY Keyboard Dot Language CODE and writes
   the result to the DESTINATION, returning for a non-``NIL''
   DESTINATION ``NIL'', otherwise responding with a fresh string
   comprehending the output."
  (declare (type string      code))
  (declare (type destination destination))
  (if destination
    (when (plusp (length code))
      (let ((position  0)
            (character (char code 0)))
        (declare (type fixnum              position))
        (declare (type (or null character) character))
        
        (labels
            ((advance ()
              "Advances the POSITION to the next character in the CODE,
               if possible, and updates the current CHARACTER, returning
               no value."
              (setf character
                (when (< position (1- (length code)))
                  (char code (incf position))))
              (values))
             
             (count-character (expected-character)
              "Starting at the current POSITION, tallies zero or more
               occurrences of the EXPECTED-CHARACTER in adjacency,
               returning the count as a non-negative integer value."
              (declare (type character expected-character))
              (the (integer 0 *)
                (loop
                  while (and character
                             (char= character expected-character))
                  count 1
                  do    (advance))))
             
             (skip-whitespaces ()
              "Starting at the current POSITION, skips zero or more
               adjacent whitespaces, moving the POSITION cursor to the
               first non-whitespace character in the CODE, and returning
               no value."
              (loop
                while (and character (whitespace-character-p character))
                do    (advance))
              (values)))
          
          (loop do
            (cond
              ((null character)
                (loop-finish))
              
              ((char= character #\.)
                (let ((number-of-dots (count-character #\.)))
                  (declare (type (integer 1 *) number-of-dots))
                  (write-char
                    (get-keyboard-character number-of-dots
                      :modifier :none)
                    destination)))
              
              ((char= character #\,)
                (let ((number-of-commas (count-character #\,)))
                  (declare (type (integer 1 *) number-of-commas))
                  (write-char
                    (get-keyboard-character number-of-commas
                      :modifier :shift)
                    destination)))
              
              ((whitespace-character-p character)
                (skip-whitespaces))
              
              (T
                (error "Invalid character ~s encountered at position ~d."
                  character position)))))))
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (interpret-QWERTY-Keyboard-Dot-Language code
          :destination output)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text program generator.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-character-key (character)
  "Searches for the keyboard key corresponding to the CHARACTER,
   returning two values on success:
     (1) The key position on the keyboard, or ``NIL'' if none could be
         detected.
     (2) The modifier which when conjoined with the key produces the
         CHARACTER; or, if no match could be found, returns ``NIL''."
  (declare (type character character))
  (let ((key-index NIL))
    (declare (type (or null key-position) key-index))
    (the (values (or null key-position) (or null modifier))
      (cond
        ((setf key-index
               (position character +NORMAL-KEYBOARD+ :test #'char=))
          (values (1+ key-index) :none))
        ((setf key-index
               (position character +SHIFT-KEYBOARD+ :test #'char=))
          (values (1+ key-index) :shift))
        (T
          (values NIL NIL))))))

;;; -------------------------------------------------------

(defun get-code-for-text (text &key (destination T)
                                    (separator   #\Space))
  "Generates the QWERTY Keyboard Dot Language code necessary to produce
   the TEXT, with each two encoded characters separated by the
   SEPARATOR, and writes the code to the DESTINATION."
  (declare (type string                     text))
  (declare (type destination                destination))
  (declare (type (or null character string) separator))
  (if destination
    (loop
      with is-first-character
        of-type boolean
        =       T
      for character
        of-type character
        across  text
      for (key-position modifier)
        of-type ((or null key-position) (or null modifier))
        =       (multiple-value-list (get-character-key character))
      do
        (if is-first-character
          (setf is-first-character NIL)
          (format destination "~a" separator))
        
        (cond
          ((and key-position (eq modifier :none))
            (loop repeat key-position do
              (write-char #\. destination)))
          ((and key-position (eq modifier :shift))
            (loop repeat key-position do
              (write-char #\, destination)))
          (key-position
            (error "Invalid modifier ~s in conjunction with ~
                    key position ~d."
              modifier key-position))
          (T
            (error "No key position for character '~a' found."
              character))))
    (the string
      (with-output-to-string (output)
        (declare (type string-stream output))
        (get-code-for-text text
          :destination output
          :separator   separator)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "hello".
(interpret-QWERTY-Keyboard-Dot-Language "................................ ................. ...................................
................................... .......................")

;;; -------------------------------------------------------

;; Print "HELLO".
(interpret-QWERTY-Keyboard-Dot-Language ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ,,,,,,,,,,,,,,,,, ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ,,,,,,,,,,,,,,,,,,,,,,,")

;;; -------------------------------------------------------

;; Print to the standard output the QWERTY Keyboard Dot Language code
;; for representing the text "Hello, World!".
(get-code-for-text "Hello, World!" :destination T)

;;; -------------------------------------------------------

;; Return a new string containing the QWERTY Keyboard Dot Language code
;; for representing the text "Hello, World!".
(get-code-for-text "Hello, World!" :destination NIL)

;;; -------------------------------------------------------

;; Print to the standard output the QWERTY Keyboard Dot Language code
;; for representing the text "Hello, World!", each encoded character
;; separated by a linebreak.
(get-code-for-text "Hello, World!" :destination T :separator #\Newline)

;;; -------------------------------------------------------

;; Generate the QWERTY Keyboard Dot Language code for representing the
;; text "Hello, World!" and interpret it, thus printing the plaintext
;; to the standard output.
(interpret-QWERTY-Keyboard-Dot-Language
  (get-code-for-text "Hello, World!" :destination NIL))
