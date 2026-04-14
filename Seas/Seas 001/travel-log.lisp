;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the Seas interpreter, its agnomination's
;; assignment a ludibund exercise peracted as the \"travel log\",
;; intended to pursue the conceptual visualization of a program's
;; execution as the submarine's (instruction pointer, IP) geste
;; manifested into the geste's report.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the travel log.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type protocol-measure +DEFAULT-PROTOCOL-MEASURE+))

;;; -------------------------------------------------------

(defclass Travel-Log ()
  ((sea
    :initarg       :sea
    :initform      (error "No sea to navigate has been communicated.")
    :type          Sea
    :documentation "The sea inwith which the SUBMARINE travels.")
   (submarine
    :initform      (hire-a-submarine)
    :type          Submarine
    :documentation "The submarine acting as the instruction pointer (IP)
                    into the program and concomitant protagonist of our
                    narration during its voyage under the SEA.")
   (action-protocol
    :initform      NIL
    :type          action-protocol
    :documentation "Associates the operative icons with functions
                    dedicated to the convenable actions' exertion.")
   (accumulator
    :initform      0
    :type          integer
    :documentation "The scalar signed integer register.")
   (stack
    :initform      (prepare-an-empty-integer-stack)
    :type          Integer-Stack
    :documentation "A stack concredited with the castaldy of zero or
                    more signed integer numbers."))
  (:documentation
    "The ``Travel-Log'' class applies itself to a Seas interpreter's
     realization, its agnomination a ludibund exercise peracted on the
     premise of a submarine's sojourn inwith a water area's viscerals,
     for whom a species of ephemeris shall be conducted."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the travel log property access operations. -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-the-travel-log-in-our-hands ((log) &body body)
  "Evaluates the travel LOG, binds its slot ``sea'' to the local symbol
   macro ``$sea'', its ``submarine'' to ``$submarine'', the
   ``action-protocol'' to ``$action-protocol'', the ``accumulator'' to
   ``$accumulator'', and homologates an adit to the ``stack'' under the
   agnomination's agency as ``$stack'', executes the BODY forms, and
   returns the desinent form's results."
  (let ((evaluated-log (gensym)))
    (declare (type symbol evaluated-log))
    `(let ((,evaluated-log ,log))
       (declare (type Travel-Log ,evaluated-log)
                (ignorable       ,evaluated-log))
       (with-slots (($sea             sea)
                    ($submarine       submarine)
                    ($action-protocol action-protocol)
                    ($accumulator     accumulator)
                    ($stack           stack))
           ,evaluated-log
         (declare (type Sea           $sea)
                  (ignorable          $sea))
         (declare (type Submarine     $submarine)
                  (ignorable          $submarine))
         (declare (type list          $action-protocol)
                  (ignorable          $action-protocol))
         (declare (type integer       $accumulator)
                  (ignorable          $accumulator))
         (declare (type Integer-Stack $stack)
                  (ignorable          $stack))
         ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the travel log constructors.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-a-fresh-travel-log (sea)
  "Creates and returns a fresh ``Travel-Log'' narrating about a
   submarine's geste under the SEA."
  (declare (type Sea sea))
  (the Travel-Log
    (make-instance 'Travel-Log :sea sea)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the navigational operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-submarine (log)
  "Returs the current position of the submarine narrated upon by the
   travel LOG."
  (declare (type Travel-Log log))
  (the location
    (with-the-travel-log-in-our-hands (log)
      (submarine-position $submarine))))

;;; -------------------------------------------------------

(defun locate-the-submarine-depth (log)
  "Returs the current depth, or y-coordinate, of the submarine narrated
   upon by the travel LOG."
  (declare (type Travel-Log log))
  (the fixnum
    (get-the-location-y-coordinate
      (locate-the-submarine log))))

;;; -------------------------------------------------------

(defun submarine-has-reached-the-surface-p (log)
  "Determines whether the submarine forming the travel LOG's subject
   has reached the sea's surface, as a consectary instigating the
   travel's desition, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Travel-Log log))
  (resolve-to-a-boolean-value
    (= (locate-the-submarine-depth log)
       -1)))

;;; -------------------------------------------------------

(defun keep-on-travelling (log)
  "Moves the submarine whose emprise is narrated in the travel LOG one
   step into its current direction and returns no value."
  (declare (type Travel-Log log))
  (with-the-travel-log-in-our-hands (log)
    (move-the-submarine $submarine))
  (values))

;;; -------------------------------------------------------

(defun inspect-the-current-parcel (log)
  "Returns the sea parcel concurrent with the position of the submarine
   constituting the travel log's cynosure."
  (declare (type Travel-Log log))
  (the Icon
    (with-the-travel-log-in-our-hands (log)
      (request-the-sea-parcel-at $sea
        (locate-the-submarine log)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the arithmetic operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perform-a-binary-operation (log operator)
  "Pops the two top elements from the travel LOG's stack, applies the
   binary OPERATOR to the same, with the erstwhile top element serving
   as the first operand and its erstwhile next lower neighbor as the
   right argument, pushes the adhibition's result onto the stack, and
   returns no value."
  (declare (type Travel-Log                           log))
  (declare (type (function (integer integer) integer) operator))
  (with-the-travel-log-in-our-hands (log)
    (multiple-value-bind (left-operand right-operand)
        (pop-a-twissel-from-the-integer-stack $stack)
      (declare (type integer left-operand))
      (declare (type integer right-operand))
      (push-onto-the-integer-stack $stack
        (funcall operator left-operand right-operand))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the action protocol operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +DEFAULT-PROTOCOL-MEASURE+
  #'(lambda (icon log)
      (declare (type Icon       icon)
               (ignore          icon))
      (declare (type Travel-Log log)
               (ignore          log))
      (values))
  "The default action protocol measure, its deployment intended for such
   cases where a travel log does not imposed a bespoke handler, and
   exhausted in its contribution to enker ineffectuousness.")

;;; -------------------------------------------------------

(defun add-a-protocol-measure (log icon measure)
  "Associates the action protocol MEASURE with the ICON in the travel
   LOG and returns no value."
  (declare (type Travel-Log       log))
  (declare (type Icon             icon))
  (declare (type protocol-measure measure))
  (with-the-travel-log-in-our-hands (log)
    (setf $action-protocol
      (acons icon measure $action-protocol)))
  (values))

;;; -------------------------------------------------------

(defun look-up-the-protocol-measure-for (log icon)
  "Returns the functional object acting as the action protocol measure
   for the ICON, as established in the travel LOG's definitions.
   ---
   If no dioristic measure could be detected, the
   ``+DEFAULT-PROTOCOL-MEASURE+ is produced."
  (declare (type Travel-Log log))
  (declare (type Icon       icon))
  (the function
    (with-the-travel-log-in-our-hands (log)
      (or
        (cdr (assoc icon $action-protocol :test #'icons-match-p))
        +DEFAULT-PROTOCOL-MEASURE+))))

;;; -------------------------------------------------------

(defmacro define-a-protocol-measure ((icon log) &body protocol-measure)
  "Associates with the ICON a new nomothesia, persisting thilk in the
   LOG, its statements constituting the PROTOCOL-MEASURE form's dation,
   the implicitly defined function's first parameter being the pernor
   of the fixed name ``$icon'', its second the ``$log'', involving as a
   parergon a wrapping into the ``with-the-travel-log-in-our-hands''
   macro."
  `(add-a-protocol-measure ,log ,icon
     #'(lambda ($icon $log)
         (declare (type Icon       $icon)
                  (ignorable       $icon))
         (declare (type Travel-Log $log)
                  (ignorable       $log))
         (with-the-travel-log-in-our-hands ($log)
           ,@protocol-measure)
         (values))))

;;; -------------------------------------------------------

(defun initialize-the-action-protocol (log)
  "Registers the action protocol measures for the operative icons at the
   travel LOG and returns no value."
  (declare (type Travel-Log log))
  
  ;; Control flow duction measures:
  (define-a-protocol-measure (+WATER-WAVE+ log))
  
  (define-a-protocol-measure (+LARGE-BLUE-SQUARE+ log))
  
  
  ;; Navigational measures:
  (define-a-protocol-measure (+UP-POINTING-SMALL-RED-TRIANGLE+ log)
    (turn-the-submarine-to $submarine :up))
  
  (define-a-protocol-measure (+DOWN-POINTING-SMALL-RED-TRIANGLE+ log)
    (turn-the-submarine-to $submarine :down))
  
  (define-a-protocol-measure (+BLACK-LEFT-POINTING-TRIANGLE+ log)
    (turn-the-submarine-to $submarine :left))
  
  (define-a-protocol-measure (+BLACK-RIGHT-POINTING-TRIANGLE+ log)
    (turn-the-submarine-to $submarine :right))
  
  (define-a-protocol-measure (+LOWER-LEFT-FOUNTAIN-PEN+ log)
    (redirect-the-submarine-on-an-ascending-mirror $submarine))
  
  (define-a-protocol-measure (+BLACK-NIB+ log)
    (redirect-the-submarine-on-a-descending-mirror $submarine))
  
  (define-a-protocol-measure (+ROUND-PUSHPIN+ log)
    (turn-the-submarine-to $submarine
      (if (zerop (pop-from-the-integer-stack $stack))
        :up
        :down)))
  
  (define-a-protocol-measure (+STRAIGHT-RULER+ log)
    (turn-the-submarine-to $submarine
      (if (zerop (pop-from-the-integer-stack $stack))
        :left
        :right)))
  
  
  ;; Numerical measures:
  (define-a-protocol-measure (+KEYCAP-DIGIT-ZERO+ log)
    (push-onto-the-integer-stack $stack 0))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-ONE+ log)
    (push-onto-the-integer-stack $stack 1))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-TWO+ log)
    (push-onto-the-integer-stack $stack 2))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-THREE+ log)
    (push-onto-the-integer-stack $stack 3))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-FOUR+ log)
    (push-onto-the-integer-stack $stack 4))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-FIVE+ log)
    (push-onto-the-integer-stack $stack 5))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-SIX+ log)
    (push-onto-the-integer-stack $stack 6))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-SEVEN+ log)
    (push-onto-the-integer-stack $stack 7))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-EIGHT+ log)
    (push-onto-the-integer-stack $stack 8))
  
  (define-a-protocol-measure (+KEYCAP-DIGIT-NINE+ log)
    (push-onto-the-integer-stack $stack 9))
  
  
  ;; Arithmetical measures:
  (define-a-protocol-measure (+HEAVY-PLUS-SIGN+ log)
    (perform-a-binary-operation $log #'+))
  
  (define-a-protocol-measure (+HEAVY-MINUS-SIGN+ log)
    (perform-a-binary-operation $log #'-))
  
  (define-a-protocol-measure (+HEAVY-MULTIPLICATION-X+ log)
    (perform-a-binary-operation $log #'*))
  
  (define-a-protocol-measure (+HEAVY-DIVISION-SIGN+ log)
    (perform-a-binary-operation $log #'truncate))
  
  
  ;; Accumulator measures:
  (define-a-protocol-measure (+NEGATIVE-SQUARED-LATIN-CAPITAL-LETTER-P+
                              log)
    (multiple-value-bind (x y)
        (pop-a-twissel-from-the-integer-stack $stack)
      (declare (type integer x))
      (declare (type integer y))
      (let* ((parcel-position (specify-a-location x y))
             (parcel-icon     (request-the-sea-parcel-at
                                $sea
                                parcel-position)))
        (declare (type location parcel-position))
        (declare (type Icon     parcel-icon))
        (set-the-sea-parcel-at $sea parcel-position
          (prepare-an-icon $accumulator))
        (setf $accumulator
          (first
            (icon-code-points parcel-icon))))))
  
  (define-a-protocol-measure (+RECEIPT+ log)
    (let ((erstwhile-accumulator $accumulator))
      (declare (type integer erstwhile-accumulator))
      (setf $accumulator
        (pop-from-the-integer-stack $stack))
      (push-onto-the-integer-stack $stack erstwhile-accumulator)))
  
  (define-a-protocol-measure (+THERMOMETER+ log)
    (setf $accumulator 0))
  
  
  ;; Stack measures:
  (define-a-protocol-measure (+KEY+ log)
    (clear-the-integer-stack $stack))
  
  (define-a-protocol-measure (+LOWER-LEFT-CRAYON+ log)
    (reverse-the-integer-stack $stack))
  
  (define-a-protocol-measure (+GREEN-BOOK+ log)
    (raise-the-integer-stack-bottom $stack))
  
  (define-a-protocol-measure (+ORANGE-BOOK+ log)
    (lower-the-integer-stack-top $stack))
  
  (define-a-protocol-measure (+ENVELOPE+ log)
    (discard-the-integer-stack-top $stack))
  
  (define-a-protocol-measure (+BLACK-SCISSORS+ log)
    (swap-the-integer-stack-top-elements $stack))
  
  (define-a-protocol-measure (+ROLL-OF-PAPER+ log)
    (duplicate-the-integer-stack-top $stack))
  
  (define-a-protocol-measure (+CUP-WITH-STRAW+ log)
    (push-onto-the-integer-stack $stack
      (if (zerop (pop-from-the-integer-stack $stack))
        1
        0)))
  
  
  ;; Input and output measures:
  (define-a-protocol-measure (+ELECTRIC-LIGHT-BULB+ log)
    (format T "~&Please enter an integer number: ")
    (finish-output)
    (push-onto-the-integer-stack $stack
      (parse-integer
        (read-line NIL NIL "0")))
    (clear-input))
  
  (define-a-protocol-measure (+OPEN-BOOK+ log)
    (format T "~&Please enter a character: ")
    (finish-output)
    (push-onto-the-integer-stack $stack
      (char-code
        (read-char NIL NIL #\Null)))
    (clear-input))
  
  (define-a-protocol-measure (+BALLOT-BOX-WITH-BALLOT+ log)
    (format T "~&~d~%"
      (pop-from-the-integer-stack $stack))
    (finish-output))
  
  (define-a-protocol-measure (+CALENDAR+ log)
    (format T "~c"
      (code-char
        (pop-from-the-integer-stack $stack)))
    (finish-output))
  
  (values))

;;; -------------------------------------------------------

(defun apply-the-nait-protocol (log)
  "Applies the action protocol measure associated with the icon
   concurrent with the travel LOG's submarine position and returns no
   value."
  (declare (type Travel-Log log))
  (let ((current-icon (inspect-the-current-parcel log)))
    (declare (type Icon current-icon))
    (funcall
      (look-up-the-protocol-measure-for log current-icon)
      current-icon
      log))
  (values))

;;; -------------------------------------------------------

(defun start-our-journey (log)
  "Starts the journey whose geste constitutes the travel LOG's hyle,
   founded upon the Seas grid persitent therein, and returns no value."
  (declare (type Travel-Log log))
  (initialize-the-action-protocol log)
  (loop until (submarine-has-reached-the-surface-p log) do
    (apply-the-nait-protocol log)
    (keep-on-travelling      log))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpretation operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-Seas-code (code)
  "Interprets the piece of Seas source CODE and returns no value."
  (declare (type string code))
  (let ((log
          (prepare-a-fresh-travel-log
            (read-a-sea-from-the-string code))))
    (declare (type Travel-Log log))
    (initialize-the-action-protocol log)
    (loop until (submarine-has-reached-the-surface-p log) do
      (apply-the-nait-protocol log)
      (keep-on-travelling      log)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-Seas-lines (&rest lines)
  "Concatenates the LINES into a single string, each line twissel being
   alligated by one newline character, interprets the resulting source
   code string, and returns no value."
  (declare (type (list-of string) lines))
  (interpret-the-Seas-code
    (apply #'stack-the-lines lines))
  (values))
