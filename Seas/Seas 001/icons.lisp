;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file applies itself to the ensconcement of the Unicode character
;; and \"icon\" representations.
;; 
;; The ``Icon'' class incarnates a symbolic aggregate, contingently
;; enumerating more than one Unicode character as a unit invested with
;; both independence and cohesion, the componency's castaldy delegated
;; to an ordered list encompassing the non-negative integer code points.
;; 
;; Siccan patefaction serves, a fortiori, as a vehicle for the
;; expression of emojis, thilk combine tangible characters with
;; combining characters and variation selectors as additaments
;; entalented with an auctive influence on the design. As a forbisen
;; adduced, a decimal digit may be amplected into a visually appealing
;; rectangular plasmature by a set of covenable modifiers.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of Unicode character operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun code-point-designates-a-combining-character-p (code-point)
  "Determines whether the Unicode CODE-POINT designates a \"combining
   character\", returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   The combining characters' Unicode code point range experiences its
   gendrure in a dimidation from two disjunct realms, expressed, in
   concord with the subject's consuetude, in a hexadecimal format as:
     (a) The closed interval [U+0300, U+036F].
     (b) The two-member set  {U+3099, U+309A}."
  (declare (type code-point code-point))
  (the boolean
    (resolve-to-a-boolean-value
      (or (<= #x0300 code-point #x036F)
          (<= #x1AB0 code-point #x1AFF)
          (<= #x1DC0 code-point #x1DFF)
          (<= #x20D0 code-point #x20FF)
          (<= #x2DE0 code-point #x2DFF)
          (<= #xFE20 code-point #xFE2F)
          (=         code-point #x3099)
          (=         code-point #x309A)))))

;;; -------------------------------------------------------

(defun code-point-designates-a-variation-selector-p (code-point)
  "Determines whether the Unicode CODE-POINT designates a \"variation
   selector\", returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   The variation selectors' Unicode code point range constitutes the
   following closed interval's dation, expressed, in concord with the
   subject's consuetude, in a hexadecimal format as: [U+FE00, U+FE0F]."
  (declare (type code-point code-point))
  (the boolean
    (resolve-to-a-boolean-value
      (<= #xFE00 code-point #xFE0F))))

;;; -------------------------------------------------------

(defun code-point-designates-a-modifier-p (code-point)
  "Determines whether the Unicode CODE-POINT designates a character
   modifier, that is, either a member of the species of the \"combining
   characters\" or the class of \"variation selectors\", returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type code-point code-point))
  (the boolean
    (or (code-point-designates-a-combining-character-p code-point)
        (code-point-designates-a-variation-selector-p  code-point))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the class "Icon".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Icon
  (:constructor prepare-an-icon (&rest code-points)))
  "The ``Icon'' class serves in the encapsulation of the Unicode code
   points whose coefficiency, molded into an ordered list, serves in a
   contingently composite Unicode character's replication.
   ---
   Siccan character representation complects in its diorism both the
   basic character and a potential catena of \"combining characters\"
   and \"variation selectors\", whose compound specifications serve as
   a unity's hypostasis.
   ---
   A forbisen's adduction, the Unicode character \"0️⃣\", norned in its
   formal diction \"Keycap Digit Zero\", actually wists of a tripartite
   conformation as the acquainted digit zero \"0\" (Unicode code point
   48) succeeded by the \"Variation Selector 16\" (code point 65039),
   and concluded with the \"Combining Enclosing Keycap\" (code point
   8419). In the ``Icon'' class' parlance, this compound's
   representation ensues in an ordered list of its code points:
   
     48, 65039, 8419
   
   thilk encompasses the homologation for recognition, retrieval, and
   equiparation at any later stage."
  (code-points NIL :type (list-of code-point) :read-only NIL))

;;; -------------------------------------------------------

(defun add-a-code-point-to-the-icon (receiving-icon new-code-point)
  "Appends the NEW-CODE-POINT, representative of a Unicode code point,
   to the RECEIVING-ICON's constitution and returns no value."
  (declare (type Icon       receiving-icon))
  (declare (type code-point new-code-point))
  (setf (icon-code-points receiving-icon)
    (append (icon-code-points receiving-icon)
      (list new-code-point)))
  (values))

;;; -------------------------------------------------------

(defun count-the-icon-code-points (icon)
  "Returns the tally of Unicode code points comprising the ICON."
  (declare (type Icon icon))
  (the (integer 0 *)
    (length
      (icon-code-points icon))))

;;; -------------------------------------------------------

(defun icon-contains-a-non-modifier-p (icon)
  "Determines whether the ICON contains at least one non-modifier code
   point, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Icon icon))
  (the boolean
    (resolve-to-a-boolean-value
      (find-if-not #'code-point-designates-a-modifier-p
        (icon-code-points icon)))))

;;; -------------------------------------------------------

(defun null-icon-p (icon)
  "Determines whether the ICON represents the special \"null icon\", an
   empty complex, deprived of any code points, and serving, in the
   common case, as a source exhaustion marker, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Icon icon))
  (the boolean
    (null
      (icon-code-points icon))))

;;; -------------------------------------------------------

(defun icons-match-p (first-icon second-icon)
  "Determines whether the FIRST-ICON and SECOND-ICON designate the
   same, contingently composite, Unicode character, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Icon first-icon))
  (declare (type Icon second-icon))
  (the boolean
    (resolve-to-a-boolean-value
      (and
        (= (count-the-icon-code-points first-icon)
           (count-the-icon-code-points second-icon))
        (every #'=
          (icon-code-points first-icon)
          (icon-code-points second-icon))))))

;;; -------------------------------------------------------

(defun print-the-icon-to (icon destination)
  "Prints a representative form of the ICON to the DESTINATION stream
   and returns no value."
  (declare (type Icon   icon))
  (declare (type stream destination))
  (format destination "~c"
    (if (icon-code-points icon)
      (code-char
        (first
          (icon-code-points icon)))
      #\Space))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the recognized Unicode icons.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-a-docstring-for-the-icon (name code-points)
  "Generates and returns a documentation string for the global ``Icon''
   variable amenable to the NAME, involving its constituent CODE-POINTS'
   enumeration."
  (declare (type symbol               name))
  (declare (type (list-of code-point) code-points))
  (the string
    (let ((number-of-code-points (length code-points)))
      (declare (type fixnum number-of-code-points))
      (format NIL "The global variable ``~:@(~a~)'' represents the ~
                   ~:[composite~;atomic~] icon composed of the ~
                   Unicode code point~p ~{~d~^, ~}."
        name
        (<= number-of-code-points 1)
        number-of-code-points
        code-points))))

;;; -------------------------------------------------------

(defmacro define-a-standard-icon (name &rest code-points)
  "Creates a fresh ``Icon'' composed of the CODE-POINTS, assigns the
   same to a newly defined global constant amenable to the NAME, and
   returns no value.
   ---
   As an act of supererogation, a ``declaim'' declaration applies itself
   to a prevenience's agency, thilk defines the resulting instance as a
   specimen subsumed into the ``Icon'' type."
  `(progn
     (declaim (type Icon ,name))
     (defparameter ,name
       (prepare-an-icon ,@code-points)
       ,(generate-a-docstring-for-the-icon name code-points))
     (values)))

;;; -------------------------------------------------------

(define-a-standard-icon +WATER-WAVE+                    127754)
(define-a-standard-icon +LARGE-BLUE-SQUARE+             128998)
(define-a-standard-icon +KEYCAP-DIGIT-ZERO+             48 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-ONE+              49 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-TWO+              50 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-THREE+            51 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-FOUR+             52 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-FIVE+             53 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-SIX+              54 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-SEVEN+            55 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-EIGHT+            56 65039 8419)
(define-a-standard-icon +KEYCAP-DIGIT-NINE+             57 65039 8419)
(define-a-standard-icon +NEGATIVE-SQUARED-LATIN-CAPITAL-LETTER-P+
                                                        127359 65039)
(define-a-standard-icon +RECEIPT+                       129534)
(define-a-standard-icon +THERMOMETER+                   127777)
(define-a-standard-icon +KEY+                           128273)
(define-a-standard-icon +LOWER-LEFT-CRAYON+             128397)
(define-a-standard-icon +GREEN-BOOK+                    128215)
(define-a-standard-icon +ORANGE-BOOK+                   128217)
(define-a-standard-icon +LOWER-LEFT-FOUNTAIN-PEN+       128395)
(define-a-standard-icon +BLACK-NIB+                     10002 65039)
(define-a-standard-icon +ROUND-PUSHPIN+                 128205)
(define-a-standard-icon +STRAIGHT-RULER+                128207)
(define-a-standard-icon +HEAVY-PLUS-SIGN+               10133)
(define-a-standard-icon +HEAVY-MINUS-SIGN+              10134)
(define-a-standard-icon +HEAVY-MULTIPLICATION-X+        10006 65039)
(define-a-standard-icon +HEAVY-DIVISION-SIGN+           10135)
(define-a-standard-icon +UP-POINTING-SMALL-RED-TRIANGLE+   128316)
(define-a-standard-icon +DOWN-POINTING-SMALL-RED-TRIANGLE+ 128317)
(define-a-standard-icon +BLACK-LEFT-POINTING-TRIANGLE+  9664 65039)
(define-a-standard-icon +BLACK-RIGHT-POINTING-TRIANGLE+ 9654 65039)
(define-a-standard-icon +ELECTRIC-LIGHT-BULB+           128161)
(define-a-standard-icon +OPEN-BOOK+                     128214)
(define-a-standard-icon +BALLOT-BOX-WITH-BALLOT+        128499)
(define-a-standard-icon +CALENDAR+                      128197)
(define-a-standard-icon +ENVELOPE+                      9993 65039)
(define-a-standard-icon +BLACK-SCISSORS+                9986 65039)
(define-a-standard-icon +ROLL-OF-PAPER+                 129531)
(define-a-standard-icon +CUP-WITH-STRAW+                129380)

(define-a-standard-icon +NULL-ICON+)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the random operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *random-state* (make-random-state T))

;;; -------------------------------------------------------

(defun select-a-random-integer-from-the-range (minimum maximum)
  "Returns in an aleatory fashion an integer number from the closed
   interval [MINIMUM, MAXIMUM]."
  (declare (type integer minimum))
  (declare (type integer maximum))
  (the integer
    (+ minimum
       (random (1+ (- maximum minimum))))))

;;; -------------------------------------------------------

(defun randomly-select-a-printable-ascii-character-code ()
  "Returns in an aleatory fashion an ASCII character code from the
   printable range defined by the closed interval [32, 126]; where the
   space entity, appropriating the ASCII code 32, accompts for an 80%
   percentage's parcery, while the codes occupying the sub-interval
   [33, 126], constitute a 20% probability's pernancy."
  (the fixnum
    (if (< (random 1.0) 0.8)
      32
      (select-a-random-integer-from-the-range 33 126))))

;;; -------------------------------------------------------

(defun randomly-select-a-printable-icon ()
  "Returns in an aleatory fashion an ``Icon'' in representation of an
   ASCII character from the printable range defined by the closed code
   interval [32, 126]."
  (the Icon
    (prepare-an-icon
      (randomly-select-a-printable-ascii-character-code))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the icon reading operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-an-icon-from-the-string (source &key (start 0))
  "Consumes an atomic or compound Unicode character from the SOURCE
   stream and returns a fresh ``Icon'' representation thereof.
   ---
   Upon the SOURCE's exhaustion, this operations responds with the
   ``+NULL-ICON+'' sentinel"
  (declare (type string source))
  (declare (type fixnum start))
  (the (values Icon fixnum)
    (let ((icon NIL))
      (declare (type (or null Icon) icon))
      (loop
        with end-position
          of-type fixnum
          =       start
        
        for current-position
          of-type fixnum
          from    start
          below   (length source)
        for consumed-character
          of-type character
          =       (char source current-position)
        
        do
          (let ((code-point (char-code consumed-character)))
            (declare (type code-point code-point))
            (cond
              ((and icon
                    (icon-contains-a-non-modifier-p icon)
                    (not (code-point-designates-a-modifier-p
                           code-point)))
                (loop-finish))
              (T
                (unless icon
                  (setf icon
                    (prepare-an-icon)))
                (add-a-code-point-to-the-icon icon code-point)
                (incf end-position))))
        
        finally
          (return
            (values
              (or icon +NULL-ICON+)
              end-position))))))

;;; -------------------------------------------------------

(defmacro with-the-icon-iterator ((driver-name source) &body body)
  "Evaluates the SOURCE string, defines a local functions whose
   agnomination constitutes the DRIVER-NAME's dation, and which upon
   each invocation, expecting no arguments, supplies the next ``Icon''
   from the SOURCE, evaluates the BODY forms, and returns the desinent
   form's results."
  (let ((evaluated-source (gensym))
        (current-icon     (gensym))
        (current-position (gensym)))
    (declare (type symbol evaluated-source))
    (declare (type symbol current-icon))
    (declare (type symbol current-position))
    `(let ((,evaluated-source ,source)
           (,current-icon     +NULL-ICON+)
           (,current-position 0))
       (declare (type string ,evaluated-source))
       (declare (type Icon   ,current-icon))
       (declare (type fixnum ,current-position))
       (flet ((,driver-name ()
               "Reads the next icon from the SOURCE and returns thilk.
                ---
                Upon the SOURCE's exhaustion, each request to this
                operation engenders a shared \"null icon\" instance's
                delivery."
               (multiple-value-setq (,current-icon ,current-position)
                 (read-an-icon-from-the-string ,evaluated-source
                   :start ,current-position))
               (the Icon ,current-icon)))
         ,@body))))
