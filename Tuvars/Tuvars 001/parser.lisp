;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the parser, the dever of which encompasses an
;; executable Tuvars program's assemblage from the lexer's services.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-variable-argument (line start)
  "Proceeding from the START position into the LINE, reads a variable
   name as a command argument, probes its occurrency as the desinent
   effective content on the LINE, and returns the thus detected
   identifier character."
  (declare (type string line))
  (declare (type fixnum start))
  (let ((position (skip-spaces line start)))
    (declare (type fixnum position))
    (multiple-value-bind (variable new-position)
        (read-variable-name line position)
      (declare (type character variable))
      (declare (type fixnum    new-position))
      (setf position new-position)
      (expect-end-of-line line position)
      (the character variable))))

;;; -------------------------------------------------------

(defun parse-integer-argument (line start)
  "Proceeding from the START position into the LINE, reads an integer
   literal as a command argument, probes its occurrency as the desinent
   effective content on the LINE, and returns the detected number."
  (declare (type string line))
  (declare (type fixnum start))
  (let ((integer-literal 0)
        (position        (skip-spaces line start)))
    (declare (type integer integer-literal))
    (declare (type fixnum  position))
    (multiple-value-setq (integer-literal position)
      (read-integer line position))
    (expect-end-of-line line position)
    (the integer integer-literal)))

;;; -------------------------------------------------------

(defun parse-print-string-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"print\" identifier, parses a
   string print command invocation and returns a
   ``Print-String-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the Print-String-Command
    (make-print-string-command :message
      (subseq line
        (skip-spaces line start)))))

;;; -------------------------------------------------------

(defun parse-print-character-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"char\" identifier, parses a
   character print command invocation and returns a
   ``Print-Character-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the Print-Character-Command
    (make-print-character-command :variable
      (parse-variable-argument line start))))

;;; -------------------------------------------------------

(defun parse-print-number-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"number\" identifier, parses a
   number print command invocation and returns a
   ``Print-Number-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the Print-Number-Command
    (make-print-number-command :variable
      (parse-variable-argument line start))))

;;; -------------------------------------------------------

(defun parse-print-linebreak-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"linebreak\" identifier, parses a
   linebreak print command invocation and returns a
   ``Print-Linebreak-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (expect-end-of-line line start)
  (the Print-Linebreak-Command
    (make-print-linebreak-command)))

;;; -------------------------------------------------------

(defun parse-read-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"read\" identifier, parses a read
   command invocation and returns a ``Read-Command'' representation
   thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the Read-Command
    (make-read-command :variable
      (parse-variable-argument line start))))

;;; -------------------------------------------------------

(defun parse-title-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"title\" identifier, parses a
   console title setting command invocation and returns a
   ``Title-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the Title-Command
    (make-title-command :text
      (subseq line
        (skip-spaces line start)))))

;;; -------------------------------------------------------

(defun parse-clear-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"clear\" identifier, parses a
   console clearance command invocation and returns a ``Clear-Command''
   representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (expect-end-of-line line start)
  (the Clear-Command
    (make-clear-command)))

;;; -------------------------------------------------------

(defun parse-close-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"close\" identifier, parses a
   console closure command invocation and returns a ``Close-Command''
   representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (expect-end-of-line line start)
  (the Close-Command
    (make-close-command)))

;;; -------------------------------------------------------

(defun parse-goto-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"goto\" identifier, parses a goto
   command invocation and returns a ``Goto-Command'' representation
   thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (let ((position (skip-spaces line start)))
    (declare (type fixnum position))
    (the Goto-Command
      (make-goto-command :line-number
        (parse-integer-argument line position)))))

;;; -------------------------------------------------------

(defun parse-if-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"if\" identifier, parses an
   affirmative conditional command invocation and returns an
   ``If-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the If-Command
    (make-if-command :variable
      (parse-variable-argument line start))))

;;; -------------------------------------------------------

(defun parse-if-not-command (line start)
  "Proceeding from the START position into the LINE, empight immediately
   in succession to the identifying \"ifnot\" identifier, parses a
   negative conditional command invocation and returns an
   ``If-Not-Command'' representation thereof."
  (declare (type string line))
  (declare (type fixnum start))
  (the If-Not-Command
    (make-if-not-command :variable
      (parse-variable-argument line start))))

;;; -------------------------------------------------------

(defun parse-line (line)
  "Parses the LINE and either returns a command representation of its
   content or, if the same does not comprehend a single instruction,
   responds with the ``NIL'' value."
  (declare (type string line))
  (let ((position 0))
    (declare (type fixnum position))
    (setf position (skip-spaces  line 0))
    (setf position (skip-comment line position))
    (the (or null Command)
      (when (< position (length line))
        (multiple-value-bind (command-name new-position)
            (read-word line position)
          (declare (type string command-name))
          (declare (type fixnum new-position))
          (string-case command-name
            ("print"
              (parse-print-string-command    line new-position))
            ("char"
              (parse-print-character-command line new-position))
            ("number"
              (parse-print-number-command    line new-position))
            ("linebreak"
              (parse-print-linebreak-command line new-position))
            ("read"
              (parse-read-command            line new-position))
            ("title"
              (parse-title-command           line new-position))
            ("clear"
              (parse-clear-command           line new-position))
            ("close"
              (parse-close-command           line new-position))
            ("goto"
              (parse-goto-command            line new-position))
            ("if"
              (parse-if-command              line new-position))
            ("ifnot"
              (parse-if-not-command          line new-position))
            (otherwise
              NIL)))))))

;;; -------------------------------------------------------

(defun parse-program (code)
  "Parses the piece of Tuvars source CODE and returns a one-dimensional
   simple array comprehending its commands."
  (declare (type string code))
  (with-input-from-string (input-stream code)
    (declare (type string-stream input-stream))
    (the tuvars-program
      (coerce
        (loop
          for current-line
            of-type (or null string)
            =       (read-line input-stream NIL NIL)
          
          while current-line
          
          for command
            of-type (or null Command)
            =       (parse-line current-line)
          
          when command
            collect (parse-line current-line))
        '(simple-array Command (*))))))
