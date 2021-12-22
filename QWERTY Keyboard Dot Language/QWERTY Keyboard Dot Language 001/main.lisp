;; Date: 2021-12-21
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/QWERTY_Keyboard_Dot_Language"
;;   -> "https://www.daskeyboard.com/blog/qwerty-vs-dvorak-vs-colemak-keyboard-layouts/"
;;       o Various keyboard layouts, including QWERTY.
;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype modifier ()
  "The ``modifier'' type defines the possible key modifiers for a use
   in a keyboard."
  '(member :none :shift))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for writing operations,
   including, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array character (49)) +NORMAL-KEYBOARD+))
(declaim (type (simple-array character (49)) +SHIFT-KEYBOARD+))

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
  (declare (type (integer 1 *) key-position))
  (declare (type modifier      modifier))
  (flet ((check-key-position (keyboard)
          "Checks whether the KEY-POSITION is recognized as a valid
           position in the KEYBOARD, throwing an error on failure, and
           returning no value if permissive."
          (declare (type (simple-array character *) keyboard))
          (unless (<= 1 key-position (length keyboard))
            (error "Invalid KEY-POSITION: ~d. Expected a value in the ~
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

;;; -------------------------------------------------------

(defun interpret-QWERTY-Keyboard-Dot-Language (code
                                               &key (destination T))
  "Interprets the piece of QWERTY Keyboard Dot Language CODE and writes
   the result to the DESTINATION."
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
             
             (whitespace-character-p (subject)
              "Checks whether the SUBJECT represents a whitespace
               character, returning a ``boolean'' value of ``T'' on
               confirmation, or ``NIL'' on a mismatch."
              (declare (type character subject))
              (the boolean
                (not
                  (null
                    (member subject
                      '(#\Linefeed #\Newline #\Return #\Space #\Tab)
                      :test #'char=)))))
             
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
;; -- Implementation of additional operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-character-key (character)
  "Searches for the keyboard key corresponding to the CHARACTER,
   returning two values on success: (1) the key position on the
   keyboard and (2) the modifier which when conjoined with the key
   produces the CHARACTER; or, if no match could be found, returns a
   single value of ``NIL''."
  (declare (type character character))
  (let ((key-index NIL))
    (declare (type (or null (integer 0 *)) key-index))
    (the (values (or null (integer 0 *)) (or null modifier))
      (cond
        ((setf key-index
               (position character +NORMAL-KEYBOARD+ :test #'char=))
          (values (1+ key-index) :none))
        ((setf key-index
               (position character +SHIFT-KEYBOARD+ :test #'char=))
          (values (1+ key-index) :shift))
        (T
          NIL)))))

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
        of-type ((or null (integer 0 *)) (or null modifier))
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
