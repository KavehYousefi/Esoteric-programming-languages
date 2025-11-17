;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements an interpreter for the "5" programming language,
;; an entity to whom the bailiwick's assignment concerning such a
;; program's execution constitutes its parcery's nature.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of 5 mode processor interface.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric prepare-the-5-code-for-its-execution (mode interpreter)
  (:documentation
    "Adhibits the requisite parasceve to the INTERPRETER's internally
     managed \"5\" program, assuming the specified MODE as the language
     variant's designation, and returns no value.
     ---
     The causata accompassed by this operation shall be construed as a
     single modification ere the INTERPRETER's actual execution step."))

;;; -------------------------------------------------------

(defgeneric process-the-5-symbol (mode symbol interpreter)
  (:documentation
    "Processes the SYMBOL in the \"5\" INTERPRETER's context, assuming
     the specified MODE as the language variant's designation, and
     returns no value."))

;;; -------------------------------------------------------

(defgeneric prepare-for-the-next-5-symbol (mode interpreter)
  (:documentation
    "As a sequela to a processed symbol's conclusion, administers the
     parasceuastic measures to the \"5\" INTERPRETER's internal state,
     assuming the specified MODE as the language variant's designation,
     and returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of 5 mode processor commodity operations.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun object-designates-the-generic-sentinel-p (object)
  "Determines whether the OBJECT represents the generic sentinel symbol
   ``*'', returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''.
   ---
   Its encheson such of a high crebritude, the ``*'' sentinel applies
   itself, among other this, to the designation of a formal parameter
   whose type and/or value do not partake of any significance."
  (declare (type T object))
  (the boolean
    (resolve-to-a-boolean-value
      (and (symbolp object)
           (eq      object '*)))))

;;; -------------------------------------------------------

(defun build-the-5-processor-mode-specializer (mode)
  "Creates and returns a method specializer covenable for the \"5\"
   MODE's representation in a ``defmethod'' definition's formal
   parameter."
  (declare (type (or symbol 5-mode) mode))
  (the (method-specializer T integer)
    (if (object-designates-the-generic-sentinel-p mode)
      'integer
      `(eql ,mode))))

;;; -------------------------------------------------------

(defun build-the-5-processor-symbol-specializer (symbol)
  "Creates and returns a method specializer covenable for a \"5\"
   SYMBOL's representation in a ``defmethod'' definition's formal
   parameter."
  (declare (type (or symbol character) symbol))
  (the (method-specializer character)
    (if (object-designates-the-generic-sentinel-p symbol)
      'character
      `(eql ,symbol))))

;;; -------------------------------------------------------

(defmacro define-a-5-symbol-processor (mode symbol &body body)
  "Defines an implementation of the generic function
   ``process-the-5-symbol'', its first formal argument's agnomination
   selected automatically, specializing on the resolved MODE's
   specifications, the second argument's cleping such fixated as
   ``$symbol'', specializing on the resolved SYMBOL, the third and
   desinent input nevened as ``$interpreter'', dispatching on the
   ``5-Interpreter'' class, evaluates the BODY forms, the same are
   ensconced in an implicit ``with-5-interpreter'' form, and returns
   no value.
   ---
   Please heed that the first argument, the mode, is disencumbered from
   an immediately accessible binding's furnishment, forecause the adit
   derives from interpreter itself in its ``with-5-interpreter'' form's
   symbol macro ``$mode''."
  (let ((mode-name
          (gensym))
        (mode-specializer
          (build-the-5-processor-mode-specializer mode))
        (symbol-specializer
          (build-the-5-processor-symbol-specializer symbol)))
    (declare (type symbol                           mode-name))
    (declare (type (method-specializer T integer)   mode-specializer))
    (declare (type (method-specializer T character) symbol-specializer))
    `(defmethod process-the-5-symbol
         ((,mode-name   ,mode-specializer)
          ($symbol      ,symbol-specializer)
          ($interpreter 5-Interpreter))
       (declare (type 5-mode        ,mode-name))
       (declare (ignorable          ,mode-name))
       (declare (type character     $symbol))
       (declare (ignorable          $symbol))
       (declare (type 5-Interpreter $interpreter))
       (declare (ignorable          $interpreter))
       (with-5-interpreter ($interpreter)
         ,@body)
       (values))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "5-Interpreter".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass 5-Interpreter ()
  ((source
    :initarg       :source
    :initform      ""
    :type          simple-string
    :documentation "The piece of 5, 15, or 35 source code to
                    evaluate and execute.")
   (mode
    :initarg       :mode
    :initform      5
    :type          5-mode
    :documentation "The variant, or \"mode\", of the \"5\" programming
                    language.")
   (ip
    :initform      0
    :type          fixnum
    :documentation "The current, zero-based instruction pointer (IP)
                    position into the SOURCE.")
   (shall-advance-the-ip-p
    :initform      T
    :type          boolean
    :documentation "A Boolean flag thilk determines whether the
                    instruction pointer (IP) shall be incremented at
                    the conclusion of the current symbol's processing.")
   (jump-table
    :type          Jump-Table
    :documentation "Connects the jump start points (\"[\"] with their
                    matching terminal tokens (\"]\").")
   (accumulator
    :initform      0
    :type          integer
    :documentation "The scalar integer accumulator.")
   (queue
    :initform      (make-an-empty-queue)
    :type          Queue
    :documentation "The memory queue dedicated to the data castaldy.")
   (call-stack
    :initform      (make-an-empty-call-stack)
    :type          Call-Stack
    :documentation "The call stack dedicated to the instruction pointer
                    (IP) castaldy."))
  (:documentation
    "The ``5-Interpreter'' class furnishes an entity entalented with the
     capacitation of a \"5\" program's evaluation in order to accompass
     actual efficacy."))

;;; -------------------------------------------------------

(defmacro with-5-interpreter ((interpreter) &body body)
  "Evaluates the \"5\" language INTERPRETER, binds its slots to local
   symbol macros, evaluates the BODY forms, and returns the desinent
   form's results."
  (let ((evaluated-interpreter (gensym)))
    (declare (type symbol evaluated-interpreter))
    `(let ((,evaluated-interpreter ,interpreter))
       (declare (type 5-Interpreter ,evaluated-interpreter)
                (ignorable          ,evaluated-interpreter))
       (with-slots (($source                 source)
                    ($mode                   mode)
                    ($ip                     ip)
                    ($shall-advance-the-ip-p shall-advance-the-ip-p)
                    ($jump-table             jump-table)
                    ($accumulator            accumulator)
                    ($queue                  queue)
                    ($call-stack             call-stack))
           ,evaluated-interpreter
         (declare (type simple-string $source)
                  (ignorable          $source))
         (declare (type 5-mode        $mode)
                  (ignorable          $mode))
         (declare (type fixnum        $ip)
                  (ignorable          $ip))
         (declare (type boolean       $shall-advance-the-ip-p)
                  (ignorable          $shall-advance-the-ip-p))
         (declare (type Jump-Table    $jump-table)
                  (ignorable          $jump-table))
         (declare (type integer       $accumulator)
                  (ignorable          $accumulator))
         (declare (type Queue         $queue)
                  (ignorable          $queue))
         (declare (type Call-Stack    $call-stack)
                  (ignorable          $call-stack))
         ,@body))))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((interpreter 5-Interpreter) &key)
  "Builds the jump table for the \"5\" program consigned to the
   INTERPRETER's castaldy and returns no value."
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (prepare-the-5-code-for-its-execution $mode interpreter)
    (setf $jump-table
      (supputate-the-jump-table-for $source)))
  (values))

;;; -------------------------------------------------------

(defun make-a-5-interpreter (source mode)
  "Creates and returns a fresh ``5-Interpreter'', dedicated to the piece
   of \"5\" SOURCE code, the same is construed in the MODE variant of
   the programming language."
  (declare (type simple-string source))
  (declare (type 5-mode        mode))
  (the 5-Interpreter
    (make-instance '5-Interpreter :source source :mode mode)))

;;; -------------------------------------------------------

(defun advance-to-the-next-5-symbol (interpreter)
  "Based upon the \"5\" INTERPRETER's internal configurations,
   contingently advances its instruction pointer (IP) to the next
   character in its underlying source code."
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (when $shall-advance-the-ip-p
      (incf $ip))
    (setf $shall-advance-the-ip-p T))
  (values))

;;; -------------------------------------------------------

(defmethod prepare-the-5-code-for-its-execution
    ((mode        (eql 5))
     (interpreter 5-Interpreter))
  (declare (type 5-mode        mode)
           (ignore             mode))
  (declare (type 5-Interpreter interpreter)
           (ignore             interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod prepare-the-5-code-for-its-execution
    ((mode        (eql 15))
     (interpreter 5-Interpreter))
  (declare (type 5-mode        mode)
           (ignore             mode))
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (setf $source
      (excise-comments-from-the-5-code $source)))
  (values))

;;; -------------------------------------------------------

(defmethod prepare-the-5-code-for-its-execution
    ((mode        (eql 35))
     (interpreter 5-Interpreter))
  (declare (type 5-mode        mode)
           (ignore             mode))
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (setf $source
      (excise-comments-from-the-5-code $source)))
  (values))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\«
  (setf $accumulator
    (* $accumulator 10)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\»
  (setf $accumulator
    (floor $accumulator 10)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\+
  (incf $accumulator))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\-
  (decf $accumulator))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\²
  (setf $accumulator
    (* $accumulator $accumulator)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\³
  (setf $accumulator
    (* $accumulator $accumulator $accumulator)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\?
  (add-to-the-queue-rear $queue $accumulator))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\{
  (push-onto-the-call-stack $call-stack $ip))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\}
  (psetf
    $ip                     (pop-from-the-call-stack $call-stack)
    $shall-advance-the-ip-p NIL))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\|
  (pop-from-the-call-stack $call-stack))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\~
  (setf $accumulator
    (lognot $accumulator)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\*
  (move-the-queue-front-to-its-rear $queue))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\^
  (move-the-queue-rear-to-its-front $queue))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\,
  (setf $accumulator
    (count-the-number-of-fives $accumulator)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\;
  (incf $ip))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\!
  (when (zerop $accumulator)
    (incf $ip)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\[
  (setf $ip
    (locate-the-jump-end-point $jump-table $ip)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\]
  (setf $ip
    (locate-the-jump-end-point $jump-table $ip)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * *
  (when (digit-char-p $symbol)
    (multiple-value-bind (extracted-number new-ip)
        (extract-a-number-from-the-5-code $source $ip)
      (declare (type (integer 0 *) extracted-number))
      (declare (type fixnum        new-ip))
      (psetf
        $accumulator            extracted-number
        $ip                     new-ip
        $shall-advance-the-ip-p NIL))))

;;; -------------------------------------------------------

;; The basic "5" variation does not recognize string literals.
(define-a-5-symbol-processor 5 #\`)

;;; -------------------------------------------------------

;; The basic "5" variation does not recognize numeric output.
(define-a-5-symbol-processor 5 #\#)

;;; -------------------------------------------------------

;; The basic "5" variation does not recognize character output.
(define-a-5-symbol-processor 5 #\$)

;;; -------------------------------------------------------

;; The basic "5" variation does not recognize input.
(define-a-5-symbol-processor 5 #\@)

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\`
  (multiple-value-bind (string-literal new-ip)
      (extract-a-string-literal-from-the-5-code $source $ip)
    (declare (type simple-string string-literal))
    (declare (type fixnum        new-ip))
    (format T "~a~%" string-literal)
    (finish-output)
    (psetf
      $ip                     new-ip
      $shall-advance-the-ip-p NIL)))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\#
  (format T "~a~%" $accumulator)
  (finish-output))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\$
  (format T "~c"
    (code-char $accumulator))
  (finish-output))

;;; -------------------------------------------------------

(define-a-5-symbol-processor * #\@
  (setf $accumulator
    (parse-the-user-input
      (query-for-a-line-of-input))))

;;; -------------------------------------------------------

(define-a-5-symbol-processor 35 #\\
  (setf $accumulator
    (replace-all-ones-by-fives $accumulator)))

;;; -------------------------------------------------------

(defmethod prepare-for-the-next-5-symbol ((mode        integer)
                                          (interpreter 5-Interpreter))
  (declare (type 5-mode        mode)
           (ignore             mode))
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (psetf
      $ip          (replace-all-ones-by-fives $ip)
      $accumulator (replace-all-ones-by-fives $accumulator)))
  (values))

;;; -------------------------------------------------------

(defmethod prepare-for-the-next-5-symbol ((mode        (eql 35))
                                          (interpreter 5-Interpreter))
  (declare (type 5-mode        mode)
           (ignore             mode))
  (declare (type 5-Interpreter interpreter)
           (ignore             interpreter))
  (values))

;;; -------------------------------------------------------

(defun 5-program-is-exhausted-p (interpreter)
  "Determines whether the \"5\" program consigned to the INTERPRETER's
   castaldy has been processed in its entirety, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type 5-Interpreter interpreter))
  (the boolean
    (with-5-interpreter (interpreter)
      (not (array-in-bounds-p $source $ip)))))

;;; -------------------------------------------------------

(defun request-the-current-5-symbol (interpreter)
  "Returns the symbol in the \"5\" program consigned to the
   INTERPRETER's castaldy and located under the current instruction
   pointer (IP) position."
  (declare (type 5-Interpreter interpreter))
  (the character
    (with-5-interpreter (interpreter)
      (schar $source $ip))))

;;; -------------------------------------------------------

(defun process-the-current-5-symbol (interpreter)
  "Processes the currently selected symbol in the \"5\" program
   consigned to the INTERPRETER's castaldy and returns no value."
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (process-the-5-symbol
      $mode
      (request-the-current-5-symbol interpreter)
      interpreter))
  (values))

;;; -------------------------------------------------------

(defun conclude-the-current-5-cycle (interpreter)
  "Engages in the adhibition of the concluding operations for the \"5\"
   INTERPRETER's current symbol's successful processing, the same
   concomitantly represents the parasceuastic measures for a contingent
   subsequent cycle, and returns no value."
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (prepare-for-the-next-5-symbol $mode interpreter))
  (values))

;;; -------------------------------------------------------

(defun handle-a-negative-accumulator (interpreter)
  "Determines whether the \"5\" INTERPRETER's accumulator contains a
   negative value, on confirmation dequeuing from the INTERPRETER's
   queue the front element and transferring thilk to the accumulator,
   otherwise accompassing no epiphenomenon, and in any case returning
   no value."
  (declare (type 5-Interpreter interpreter))
  (with-5-interpreter (interpreter)
    (when (minusp $accumulator)
      (setf $accumulator
        (remove-from-the-queue-front $queue))))
  (values))

;;; -------------------------------------------------------

(defun execute-the-5-program (interpreter)
  "Executes the \"5\" program consigned to the INTERPRETER's castaldy
   and returns no value."
  (declare (type 5-Interpreter interpreter))
  (loop until (5-program-is-exhausted-p interpreter) do
    (process-the-current-5-symbol  interpreter)
    (advance-to-the-next-5-symbol  interpreter)
    (conclude-the-current-5-cycle  interpreter)
    (handle-a-negative-accumulator interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-5-code (source &key (mode 5))
  "Interprets the piece of \"5\" source CODE, its concrete language
   variant communicated via the MODE option, and returns no value."
  (declare (type string source))
  (declare (type 5-mode mode))
  (execute-the-5-program
    (make-instance '5-Interpreter
      :source (convert-into-a-simple-string source)
      :mode   mode))
  (values))
