;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file encompasses the definitions and implementations of the
;; console, the paravaunt entity for the input and output intercourse
;; actuation during a Tuvars program.
;; 
;; A necessity issuing from Common Lisp's lacuna regarding a graphical
;; user interface library, a textual succedaneum, in conjunction with
;; its appertaining interface for contingent supersessions, partakes of
;; a furnishment.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Console".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Console ()
  ()
  (:documentation
    "The ``Console'' interface establishes a diorism for an interactive
     console's provision."))

;;; -------------------------------------------------------

(defgeneric start-console (console)
  (:documentation
    "Opens, activates, and ostends the CONSOLE, and returns no value."))

;;; -------------------------------------------------------

(defgeneric insert-string (console content)
  (:documentation
    "Appends the string CONTENT immediately to the CONSOLE's output and
     returns no value."))

;;; -------------------------------------------------------

(defgeneric insert-character (console content)
  (:documentation
    "Appends the character CONTENT immediately to the CONSOLE's output
     and returns no value."))

;;; -------------------------------------------------------

(defgeneric insert-number (console content)
  (:documentation
    "Appends the integral or floating-point numeric CONTENT immediately
     to the CONSOLE's output and returns no value."))

;;; -------------------------------------------------------

(defgeneric insert-linebreak (console)
  (:documentation
    "Appends a single linebreak immediately to the CONSOLE's output and
     returns no value."))

;;; -------------------------------------------------------

(defgeneric clear-console (console)
  (:documentation
    "Purges the CONSOLE's output and returns no value."))

;;; -------------------------------------------------------

(defgeneric set-title (console new-title)
  (:documentation
    "Changes the CONSOLE title text to the NEW-TITLE and returns no
     value."))

;;; -------------------------------------------------------

(defgeneric close-console (console)
  (:documentation
    "Closes the CONSOLE and returns no value.
     ---
     If the CONSOLE status ascertains an already assumed closure, this
     operation does not accompass any modifications, nor does it respond
     in an anomalous manner."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Text-Console".                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum +DEFAULT-VIEWPORT-WIDTH+))
(declaim (type fixnum +DEFAULT-VIEWPORT-HEIGHT+))
(declaim (type fixnum +DEFAULT-SCREEN-HEIGHT+))

;;; -------------------------------------------------------

(defparameter +DEFAULT-VIEWPORT-WIDTH+ 30
  "The default number of columns comprising the console output area.")

(defparameter +DEFAULT-VIEWPORT-HEIGHT+ 15
  "The default number of lines comprising the console output area.")

(defparameter +DEFAULT-SCREEN-HEIGHT+ 35
  "The default tally of lines comprising the physical or virtual
   screen.")

;;; -------------------------------------------------------

(defclass Text-Console (Console)
  ((viewport-width
    :initarg       :viewport-width
    :initform      +DEFAULT-VIEWPORT-WIDTH+
    :type          fixnum
    :documentation "The number of columns comprising the output area.")
   (viewport-height
    :initarg       :viewport-height
    :initform      +DEFAULT-VIEWPORT-HEIGHT+
    :type          fixnum
    :documentation "The number of rows comprising the output area.")
   (total-width
    :initform      0
    :type          fixnum
    :documentation "The complete width of the console.")
   (total-height
    :initform      0
    :type          fixnum
    :documentation "The complete height of the console.")
   (title
    :initform      "Tuvars"
    :type          string
    :documentation "The console window title text.")
   (complete-content
    :initform      (make-array 0
                     :element-type    'character
                     :initial-element #\Null
                     :adjustable      T
                     :fill-pointer    0)
    :type          string
    :documentation "The entire string inserted into the console's
                    output buffer.")
   (visible-content
    :initform      NIL
    :type          (list-of string)
    :documentation "A rearrangement of the COMPLETE-CONTENT as a line
                    of strings, its horizontal dispansion in concord
                    with the VIEWPORT-WIDTH as a maximum length, and, in
                    its vertical aspect, adjusted to the VIEWPORT-HEIGHT
                    in its imposition of a maximum tally of admitted
                    rows.")
   (screen-height
    :initarg       :screen-height
    :initform      +DEFAULT-SCREEN-HEIGHT+
    :type          fixnum
    :documentation "The tally of lines comprising the physical or
                    virtual display screen's height, utilized for
                    repaint operations of the console.")
   (open-p
    :initform      T
    :type          boolean
    :documentation "Determines whether the console is open, that is,
                    visible and responsive."))
  (:documentation
    "The ``Text-Console'' class furnishes a ``Console'' implementation
     based upon a textual interface, its intercourse proceeding by means
     of the system's standard input and output conduits.
     ---
     The following diagram shall adhibit a visual supputation's dation
     considering the roles of the various configurations and, in
     particular, expose the relationships betwixt the total dimensions
     and the viewport measurement:
     
                 totalWidth
          |~~~~~~~~~~~~~~~~~~~~~~~|
          
                viewportWidth
            |~~~~~~~~~~~~~~~~~~~~|
               viewportWidth = totalWidth - (leftMargin + rightMargin)
                             = totalWidth - (2          + 2)
                             = totalWidth - 4
      
      1   +-----------------------+  = 1
      2   |                       |  | 2
      3   +-----------------------+  | 3
      4   |                       |  | 4
     ...  |                       |  | viewportHeight = totalHeight - 4
     ...  |                       |  |
     tH   +-----------------------+  = totalHeight"))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((console Text-Console) &key)
  (declare (type Text-Console console))
  (with-slots (total-width total-height viewport-width viewport-height)
      console
    (declare (type fixnum total-width))
    (declare (type fixnum total-height))
    (declare (type fixnum viewport-width))
    (declare (type fixnum viewport-height))
    (setf total-width  (+ viewport-width  4))
    (setf total-height (+ viewport-height 4)))
  (values))

;;; -------------------------------------------------------

(defun make-text-console (viewport-width viewport-height
                          &key (screen-height +DEFAULT-SCREEN-HEIGHT+))
  "Creates and returns a new ``Text-Console'' whose dimensions are
   derived from the VIEWPORT-WIDTH and VIEWPORT-HEIGHT, and which
   employs the optional SCREEN-HEIGHT standard for its measurements."
  (declare (type fixnum viewport-width))
  (declare (type fixnum viewport-height))
  (declare (type fixnum screen-height))
  (the Text-Console
    (make-instance 'Text-Console
      :viewport-width  viewport-width
      :viewport-height viewport-height
      :screen-height   screen-height)))

;;; -------------------------------------------------------

(defun make-default-text-console ()
  "Creates and returns a ``Text-Console'' utilizing the default
   configurations."
  (the Text-Console
    (make-text-console
      +DEFAULT-VIEWPORT-WIDTH+
      +DEFAULT-VIEWPORT-HEIGHT+)))

;;; -------------------------------------------------------

(defun update-visible-content (console)
  "Supputates the CONSOLE's visible lines based upon its complete
   content and its viewport dimensions, updates the CONSOLE, and returns
   no value."
  (declare (type Console console))
  (with-slots (visible-content) console
    (declare (type (list-of string) visible-content))
    (with-slots (complete-content viewport-width) console
      (declare (type string complete-content))
      (declare (type fixnum viewport-width))
      (setf visible-content
        (collect-text-lines
          (arrange-layouts-in-lines
            (generate-text-layouts complete-content viewport-width)))))
    (with-slots (viewport-height) console
      (declare (type fixnum viewport-height))
      (loop while (> (length visible-content) viewport-height) do
        (pop visible-content))))
  (values))

;;; -------------------------------------------------------

(defun draw-horizontal-line (console)
  "Draws a horizontal line spanning the CONSOLE window's width to the
   standard output and returns no value."
  (declare (type Text-Console console))
  (format T "~&+")
  (format T "~v@{~a~:*~}"
    (- (slot-value console 'total-width) 2)
    #\-)
  (format T "+")
  (format T "~%")
  (values))

;;; -------------------------------------------------------

(defun draw-left-bourne (console)
  "Draws the CONSOLE window's sinistral border for the current line to
   the standard output and returns no value."
  (declare (type Text-Console console))
  (declare (ignore            console))
  (format T "~&|")
  (values))

;;; -------------------------------------------------------

(defun draw-right-bourne (console)
  "Draws the CONSOLE window's dextral border for the current line to the
   standard output and returns no value."
  (declare (type Text-Console console))
  (format T "~vt|"
    (1- (slot-value console 'total-width)))
  (values))

;;; -------------------------------------------------------

(defun draw-title-bar (console)
  "Prints the CONSOLE's title bar to the standard output and returns no
   value."
  (declare (type Text-Console console))
  (draw-left-bourne console)
  (format T " ~a"
    (curtail-text
      (slot-value console 'title)
      (slot-value console 'viewport-width)))
  (draw-right-bourne console)
  (values))

;;; -------------------------------------------------------

(defun draw-output-content (console)
  "Prints the CONSOLE's output content to the standard output and
   returns no value."
  (declare (type Console console))
  (with-slots (visible-content viewport-height) console
    (declare (type (list-of string) visible-content))
    (declare (type fixnum           viewport-height))
    (loop
      repeat viewport-height
      for output-lines
        of-type (list-of string)
        =       visible-content
        then    (rest output-lines)
      do
        (draw-left-bourne console)
        (format T "~v@{~a~:*~}" 1 #\Space)
        (when output-lines
          (format T "~a"
            (first output-lines)))
        (draw-right-bourne console)))
  (values))

;;; -------------------------------------------------------

(defun shift-console-into-view (console)
  "Inserts a specified tally of linebreaks in order to conceal the
   CONSOLE's previous representation and emphasize its current one, and
   returns no value."
  (declare (type Text-Console console))
  (format T "~v%"
    (slot-value console 'screen-height))
  (values))

;;; -------------------------------------------------------

(defun update-view (console)
  "Updates the CONSOLE's visual representation on the standard output by
   printing its current state, if still open, to the same, otherwise
   accompassing no causatum, and in any case returns no value."
  (declare (type Text-Console console))
  (when (slot-value console 'open-p)
    ;; Shift old console image out of view and the new one into it.
    (shift-console-into-view console)
    (update-visible-content console)
    ;; Top window line.
    (draw-horizontal-line console)
    ;; Draw title bar.
    (draw-title-bar console)
    ;; Bottom title bar line.
    (draw-horizontal-line console)
    ;; Print content.
    (draw-output-content console)
    ;; Bottom window line.
    (draw-horizontal-line console))
  (values))

;;; -------------------------------------------------------

(defmethod start-console ((console Text-Console))
  (declare (type Text-Console console))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod insert-string ((console Text-Console) (content string))
  (declare (type Text-Console console))
  (declare (type string       content))
  (with-slots (complete-content) console
    (declare (type string complete-content))
    (format complete-content "~a" content))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod insert-character ((console Text-Console) (content character))
  (declare (type Text-Console console))
  (declare (type character    content))
  (with-slots (complete-content) console
    (declare (type string complete-content))
    (format complete-content "~c" content))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod insert-number ((console Text-Console) (content real))
  (declare (type Text-Console console))
  (declare (type real         content))
  (with-slots (complete-content) console
    (declare (type string complete-content))
    (format complete-content "~a" content))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod insert-linebreak ((console Text-Console))
  (declare (type Text-Console console))
  (with-slots (complete-content) console
    (declare (type string complete-content))
    (format complete-content "~%"))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod clear-console ((console Text-Console))
  (declare (type Text-Console console))
  (with-slots (complete-content) console
    (declare (type string complete-content))
    (setf (fill-pointer complete-content) 0))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod set-title ((console Text-Console) (new-title string))
  (declare (type Text-Console console))
  (declare (type string       new-title))
  (with-slots (title) console
    (declare (type string title))
    (setf title new-title))
  (update-view console)
  (values))

;;; -------------------------------------------------------

(defmethod close-console ((console Text-Console))
  (declare (type Text-Console console))
  (with-slots (open-p) console
    (declare (type boolean open-p))
    (when open-p
      (shift-console-into-view console)
      (setf open-p NIL)))
  (values))
