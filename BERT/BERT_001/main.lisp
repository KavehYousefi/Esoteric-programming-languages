;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a decoder and an encoder for the esoteric
;; programming language "BERT", presented by the Esolang user
;; "Areallycoolusername" in the year 2019, being based around the
;; application of a rational trinomial equation in order to modify each
;; plaintext message character's code, thus producing a rational-valued
;; substitute in the encoding space.
;; 
;; Concept
;; =======
;; The BERT programming language serves as an encoding of a
;; character-based text, transformed by adminiculum of a rational
;; trinomial equation which, applying to each integer character code,
;; produces a sequence of signed rational numbers, ensconced in the
;; starting marker "BE" and the terminating sequence "RT".
;; 
;; == ENCODING ==
;; A BERT program's gist consists of a sequence of encoded character
;; codes, yieled by converting the plaintext character's ASCII code or
;; Unicode code point into a rational number by mediation of a rational
;; trinomial equation.
;; 
;; In concrete diction, the following formula holds for the encoding of
;; a plaintext character code "n":
;; 
;;   bertCode = (12 * plainCode) / 69 * (-21)
;; 
;; The resulting "bertCode" represents the signed rational entity, with
;; the fractional part being equipollent in its interpretation to the
;; integer moeity.
;; 
;; == DECODING ==
;; Constituting a inherent symmetrical concept, the decoding of a BERT
;; rational number code into the respective ASCII character code or
;; Unicode code point proceeds for a "bertCode" as follows:
;; 
;;   plainCode = ((bertCode / 12) * 69) / (-21)
;; 
;; The plaintext character code is realized in the "plainCode" output.
;; 
;; 
;; Syntax
;; ======
;; A BERT program consists of a sequence of rational numbers, most
;; commonly manifesting as floating-point values, bracketed by the
;; introducing keyword "BE" and the terminating token "RT", with
;; comments and whitespaces homologated as intervening members.
;; 
;; == PROGRAM STRUCTURE ==
;; BERT source code may be subjected to a destructuring into three
;; segregated blocks:
;; 
;;   (1) The "BE" keyword must be stated in order to introduce a BERT
;;       program.
;;   (2) A sequence of zero or more signed rational numbers follows.
;;       This portion may embrace whitespaces or comments in any
;;       quantity.
;;   (3) The "RT" keyword concludes the BERT source code, with no
;;       further content being permitted in the succession.
;; 
;; == COMMENTS ==
;; Provisions for comments are accommodated by the language, designating
;; such a section's incipiency with the at-sign ("@"), and terminating
;; immediately before the first occurrence of a mathematical sign ("+",
;; "-", or "−") or a decimal digit.
;; 
;; == WHITESPACES ==
;; Betwixt tokens, whitespaces, which embraces both spaces, tabs, and
;; newlines, may be incorporated liberally. Their usage inside of
;; effective entities, that is, in the "BE" or "RT" keyword or a
;; rational number, imposes a prohibition.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-11-20
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BERT"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype decoder-callback ()
  "The ``decoder-callback'' type defines the signature for a handler
   invoked during the decoding of a BERT program, designed as a function
   which accepts as its sole input the most recently parsed BERT number
   as a ``real'' datum, ignoring any return value."
  '(function (real) *))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of basic conversion operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contingently enhance floating-point precision by using the most
;; potent format ``long-float''.
(setf *read-default-float-format* 'long-float)

;;; -------------------------------------------------------

(defun encode-precisely (code)
  "Encodes the plaintext character CODE using the BERT rational
   trinomial equation, in some cases capable of returning a more precise
   rational representation in lieu of a lossy floating-point one."
  (declare (type fixnum code))
  (the real (* (/ (* 12 code) 69) -21)))

;;; -------------------------------------------------------

(defun encode (code)
  "Encodes the plaintext character CODE using the BERT rational
   trinomial equation and returns a floating-point representation
   thereof.
   ---
   Please note that, depending on the deployed Common Lisp
   implementation's characteristics, loss of precision may incur, with
   direct ramifications applying to the complete encoding process."
  (declare (type fixnum code))
  (the real
    (float (encode-precisely code))))

;;; -------------------------------------------------------

(defun decode-precisely (bert-code)
  "Decodes the BERT-CODE into the corresponding plaintext character code
   by applying to it the reverse of the encoding formula, potentially
   yielding a precise real-valued response, as opposed to the more
   practical unsigned integer datum."
  (declare (type real bert-code))
  (the real
    (/ (* (/ bert-code 12) 69) -21)))

;;; -------------------------------------------------------

(defun decode (bert-code)
  "Decodes the BERT-CODE into the corresponding plaintext character code
   by applying to it the reverse of the encoding formula, returning the
   practically useful integer result."
  (declare (type real bert-code))
  (the fixnum
    (nth-value 0
      (round (decode-precisely bert-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character predicates.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sign-character-p (character)
  "Checks whether the CHARACTER represents a mathematical sign,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (not (null
      (or (char= character #\+)
          (char= character #\-)
          (char= character #\−))))))

;;; -------------------------------------------------------

(defun number-character-p (character)
  "Checks whether the CHARACTER represents a possible first character in
   a rational number, that is, either a mathematical sign or a decimal
   digit, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (or (digit-char-p     character)
          (sign-character-p character))))))

;;; -------------------------------------------------------

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Newline #\Space #\Tab) :test #'char=)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Decoder".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Decoder ()
  ((source
    :initarg       :source
    :initform      ""
    :type          string
    :documentation "The piece of BERT code to decode.")
   (position
    :initarg       :position
    :initform      0
    :type          fixnum
    :documentation "The current position into the SOURCE.")
   (character
    :initarg       :character
    :initform      NIL
    :type          (or null character)
    :documentation "The character at the current POSITION into the
                    SOURCE."))
  (:documentation
    "The ``Decoder'' class implements a BERT program decoder, utible for
     obtaining from such a piece of code the plaintext characters."))

;;; -------------------------------------------------------

(defmacro with-decoder ((decoder) &body body)
  "Evaluates the DECODER, binds its slots ``source'', ``position'' and
   ''character'' to eponymous symbol macros for general access,
   evaluates the BODY forms, and returns the last form's results."
  (let ((evaluated-decoder (gensym)))
    (declare (type symbol evaluated-decoder))
    `(let ((,evaluated-decoder ,decoder))
       (declare (type Decoder ,evaluated-decoder))
       (with-slots (source position character) ,evaluated-decoder
         (declare (type string              source))
         (declare (type fixnum              position))
         (declare (type (or null character) character))
         (declare (ignorable                source))
         (declare (ignorable                position))
         (declare (ignorable                character))
         (flet
             ((advance ()
               "Moves the decoder's position cursor to the next location
                in the source, if possible, updates the current
                character, and returns no value."
               (setf character
                 (when (array-in-bounds-p source (1+ position))
                   (char source (incf position))))
               (values))
              
              (reset-state ()
               "Resets the decoder's position cursor and its current
                character and returns no value."
               (setf position 0)
               (setf character
                (when (array-in-bounds-p source position)
                  (aref source position)))
               (values)))
           
           ,@body)))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((decoder Decoder) &key)
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (setf character
      (when (array-in-bounds-p source position)
        (char source position))))
  (the Decoder decoder))

;;; -------------------------------------------------------

(defun make-decoder (&optional (source ""))
  "Creates and returns a new ``Decoder'', optionally initialized with
   the SOURCE."
  (declare (type string source))
  (the Decoder
    (make-instance 'Decoder :source source)))

;;; -------------------------------------------------------

(defun decoder-set-source (decoder new-source)
  "Sets the DECODER's source to the NEW-SOURCE, resets its state, and
   returns the modified DECODER."
  (declare (type Decoder decoder))
  (declare (type string  new-source))
  (with-decoder (decoder)
    (setf source new-source)
    (reset-state))
  (the Decoder decoder))

;;; -------------------------------------------------------

(defun decoder-reset (decoder)
  "Resets the DECODER's position and character and returns the modified
   DECODER."
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (reset-state))
  (the Decoder decoder))

;;; -------------------------------------------------------

(defun decoder-skip-whitespaces (decoder)
  "Starting at the current position into the DECODER's source, skips
   a sequence of zero or more adjacent whitespaces, and returns the
   modified DECODER."
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (advance)
    (loop while (and character (whitespace-character-p character)) do
      (advance))
    (the Decoder decoder)))

;;; -------------------------------------------------------

(defun decoder-skip-comment (decoder)
  "Starting at the current position into the DECODER's source, skips a
   comment section, and returns the modified DECODER."
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (advance)
    (loop until (or (null character) (number-character-p character)) do
      (advance))
    (the Decoder decoder)))

;;; -------------------------------------------------------

(defun decoder-expect-string (decoder expected-string)
  "Checks whether the DECODER's source, starting at its current
   position, matches the EXPECTED-STRING, on confirmation relocating the
   position cursor immediately after the matching source portion and
   returning the modified DECODER, otherwise signaling an error of an
   unspecified type."
  (declare (type Decoder decoder))
  (declare (type string  expected-string))
  (with-decoder (decoder)
    (loop
      for expected-character
        of-type character
        across expected-string
      do
        (cond
          ((null character)
            (error "Expected the character ~c, but encountered EOF ~
                    at position ~d."
              expected-character position))
          ((char/= character expected-character)
            (error "Expected the character ~c, but encountered ~c ~
                    at position ~d."
              expected-character character position))
          (T
            (advance))))
    (the Decoder decoder)))

;;; -------------------------------------------------------

(defun decoder-expect-end-of-file (decoder)
  "Checks whether the DECODER's position cursor is located at the end
   of its BERT program, returning on confirmation the DECODER, otherwise
   signaling an error of an unspecified type."
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (when character
      (error "Expected EOF, but encountered the character ~c at ~
              position ~d."
        character position)))
  (the Decoder decoder))

;;; -------------------------------------------------------

(defun decoder-parse-number (decoder)
  "Starting at the current position into the DECODER's source, consumes,
   parses, and returns a number."
  (declare (type Decoder decoder))
  (with-decoder (decoder)
    (the real
      (read-from-string
        (with-output-to-string (content)
          (declare (type string-stream content))
          
          (flet
              ((read-optional-sign ()
                "If the DECODER's current character constitutes a
                 mathematical sign, writes the same to the CONTENT, and
                 returns no value."
                (case character
                  ((NIL)
                    NIL)
                  ((#\+ #\-)
                    (write-char character content)
                    (advance))
                  (#\−
                    (write-char #\- content)
                    (advance)))
                (values))
               
               (read-zero-or-more-digits ()
                "Collects the next zero or more adjacent decimal digits,
                 writes them in their correct order to the CONTENT, and
                 return no value."
                (loop while (and character (digit-char-p character)) do
                  (write-char character content)
                  (advance))
                (values))
               
               (read-optional-dot ()
                "If the DECODER's current character represents a decimal
                 dot, writes the same to the CONTENT, and returns no
                 value."
                (when (and character (char= character #\.))
                  (write-char character content)
                  (advance))
                (values)))
            
            (read-optional-sign)
            (read-zero-or-more-digits)
            (read-optional-dot)
            (read-zero-or-more-digits)))))))

;;; -------------------------------------------------------

(defun decoder-parse-content (decoder callback)
  "Expecting the DECODER's position cursor to be located beyond the
   introducing \"BE\" marker, parses the BERT program code's body in the
   form of zero or more rational numbers, invoking for each detected
   entity the CALLBACK function, and returns the modified DECODER.
   ---
   As a consequence of a successful operation, the DECODER's position
   cursor will be located at the first character of the concluding
   \"RT\" marker following this function's termination."
  (declare (type Decoder          decoder))
  (declare (type decoder-callback callback))
  
  (with-decoder (decoder)
    (loop do
      (cond
        ;; End of program?
        ;; => Terminate.
        ((null character)
          (loop-finish))
        
        ;; Whitespace?
        ;; => Skip.
        ((whitespace-character-p character)
          (decoder-skip-whitespaces decoder))
        
        ;; Comment?
        ;; => Skip.
        ((char= character #\@)
          (decoder-skip-comment  decoder))
        
        ;; Potenital \"RT\" code termination marker?
        ;; => Terminate.
        ((char= character #\R)
          (loop-finish))
        
        ;; Digit or mathematical sign?
        ;; => Parse number and invoke the CALLBACK.
        ((number-character-p character)
          (funcall callback (decoder-parse-number decoder)))
        
        ;; Any other character constitutes a violation.
        (T
          (error "Invalid character ~s at position ~d."
            character position)))))
  
  (the Decoder decoder))

;;; -------------------------------------------------------

(defun decoder-decode (decoder
                       &key (destination NIL))
  "Decodes the BERT program stored in the DECODER and writes the thus
   obtained plaintext to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise responding with a fresh
   string containing the result."
  (declare (type Decoder     decoder))
  (declare (type destination destination))
  (the (or null string)
    (cond
      (destination
        (decoder-expect-string decoder "BE")
        (decoder-parse-content decoder
          #'(lambda (bert-code)
              (declare (type real bert-code))
              (write-char (code-char (decode bert-code)) destination)
              (values)))
        (decoder-expect-string      decoder "RT")
        (decoder-expect-end-of-file decoder)
        (decoder-reset              decoder)
        NIL)
      (T
        (with-output-to-string (output)
          (declare (type string-stream output))
          (decoder-decode decoder :destination output))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of public operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type Decoder +DECODER+))

;;; -------------------------------------------------------

(defparameter +DECODER+ (make-decoder)
  "The default BERT decoder.")

;;; -------------------------------------------------------

(defun bert-decode (bert-code &key (destination NIL))
  "Decodes the piece of BERT-CODE and writes the resulting plaintext
   message to the DESTINATION, returning for a non-``NIL'' DESTINATION
   the ``NIL'' value, otherwise responding with a fresh string
   containing the result."
  (declare (type string      bert-code))
  (declare (type destination destination))
  (decoder-set-source +DECODER+ bert-code)
  (the (or null string)
    (decoder-decode +DECODER+ :destination destination)))

;;; -------------------------------------------------------

(defun bert-encode (plaintext
                    &key (minus-sign #\−)
                         (destination NIL))
  "Encodes the PLAINTEXT as a BERT program and writes the same to the
   DESTINATION, returning for a non-``NIL'' DESTINATION the ``NIL''
   value, otherwise responding with a fresh string containing the
   result."
  (declare (type string      plaintext))
  (declare (type character   minus-sign))
  (declare (type destination destination))
  (the (or null string)
    (cond
      (destination
        (flet
            ((write-bert-number-for (bert-number)
              (declare (type real bert-number))
              (if (minusp bert-number)
                (let ((number-as-string
                        (format NIL "~a"
                          (coerce bert-number
                          *read-default-float-format*))))
                  (declare (type string number-as-string))
                  (setf (char number-as-string 0) minus-sign)
                  (format destination "~a" number-as-string))
                (format destination "+~a"
                  (coerce bert-number *read-default-float-format*)))
              (values)))
          (format destination "BE")
          (loop
            for plaintext-character of-type character across plaintext
            do  (write-bert-number-for
                  (encode (char-code plaintext-character))))
          (format destination "RT")))
      (T
        (with-output-to-string (output)
          (declare (type string-stream output))
          (bert-encode plaintext :destination output))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bert-decode
  "BE@This is a 
comment−262.9565217391−368.8695652174−394.4347826087−394.4347826087−405.3913043478−160.6956521739
−116.8695652174−434.6086956522−405.3913043478−416.347826087−394.4347826087−365.2173913043RT")

;;; -------------------------------------------------------

(bert-encode "Hello, world" :destination T)

;;; -------------------------------------------------------

(bert-decode
  (bert-encode "Hello, world"))

;;; -------------------------------------------------------

(bert-decode
  (bert-encode "Hello, world")
  :destination T)
