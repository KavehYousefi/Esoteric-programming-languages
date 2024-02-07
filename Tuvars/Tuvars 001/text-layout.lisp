;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the text layout operations, to whom the dever
;; is apportioned to format a string in a two-dimensional arrangement,
;; respecting a spatial horizontal limitation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Fragment".                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Fragment
  (:constructor make-fragment
    (type source start end
     &optional (effective-length (- end start)))))
  "The ``Fragment'' class serves in the representation of a demarcated
   parcel extracted from a source string, abutting the disjoint segments
   which in collaboration replicate the provenance."
  (type             (error "Missing fragment type.")
                    :type      fragment-type
                    :read-only T)
  (source           (error "Missing source.")
                    :type      string
                    :read-only T)
  (start            (error "Missing start position.")
                    :type      fixnum
                    :read-only T)
  (end              (error "Missing end position.")
                    :type      fixnum
                    :read-only T)
  (effective-length (error "Missing effective length.")
                    :type      fixnum
                    :read-only T))

;;; -------------------------------------------------------

(defun fragment-length (fragment)
  "Returns the number of characters constituting the FRAGMENT."
  (declare (type Fragment fragment))
  (the fixnum
    (- (fragment-end   fragment)
       (fragment-start fragment))))

;;; -------------------------------------------------------

(defun fragment-content (fragment)
  "Returns the characters of the FRAGMENT's source demarcated by its
   bournes."
  (declare (type Fragment fragment))
  (the string
    (subseq
      (fragment-source fragment)
      (fragment-start  fragment)
      (fragment-end    fragment))))

;;; -------------------------------------------------------

(defun split-fragment (fragment left-extent)
  "Splits the FRAGMENT into a sinistral and a dextral partition, the
   former of which comprehends the LEFT-EXTANT tally of characters,
   while the remnants contribute to the latter moiety, and returns a
   cons whose first item represents the left parcel and whose second
   component entails the right morsel.
   ---
   The FRAGMENT itself will not be altered as a consequent of which
   operation."
  (declare (type Fragment fragment))
  (declare (type fixnum   left-extent))
  (let ((type         (fragment-type fragment))
        (source       (fragment-source fragment))
        (left-start   (fragment-start fragment))
        (splice-point (+ (fragment-start fragment) left-extent))
        (right-end    (fragment-end fragment)))
    (declare (type string source))
    (declare (type fixnum left-start))
    (declare (type fixnum splice-point))
    (declare (type fixnum right-end))
    (the (cons Fragment Fragment)
      (cons
        (make-fragment type source left-start   splice-point)
        (make-fragment type source splice-point right-end)))))

;;; -------------------------------------------------------

(defmethod print-object ((fragment Fragment) (stream T))
  (declare (type Fragment    fragment))
  (declare (type destination stream))
  (format stream "(start=~d end=~d length=~d content=~s)"
    (fragment-start   fragment)
    (fragment-end     fragment)
    (fragment-length  fragment)
    (fragment-content fragment)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text splitter.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-text-split-points (text)
  "Divides the TEXT into an ordered list of its disjunct fragments,
   cleft at the convenable word sepiments or jontures inside of a word
   itself, and returns the same."
  (declare (type string text))
  (let ((position 0))
    (declare (type fixnum position))
    (the (list-of Fragment)
      (loop while (< position (length text)) collect
        (cond
          ((linebreak-character-p (char text position))
            (let ((start position))
              (incf position)
              (make-fragment :linebreak text start position 0)))
          ((word-separator-p (char text position))
            (let ((start position))
              (declare (type fixnum start))
              (incf position)
              (make-fragment :space text start position)))
          (T
            (let ((start position))
              (declare (type fixnum start))
              (setf position
                (or (position-if #'word-separator-p text :start start)
                    (length text)))
              ;; Include a trailing hyphen ("-") or underscore ("_") in
              ;; the fragment.
              (when (word-jointure-follows-p text position)
                (incf position))
              (make-fragment :word text start position))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Text-Layout".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Text-Layout
  (:constructor make-text-layout (fragment column row)))
  "The ``Text-Layout'' class associates with a textual fragment, being
   a compound of a source text entirety's section and the information
   requisite to its location in the enveloping provenance, a spatial
   context whose capacitation encompasses the arrangement of these
   fragment inside of a specified horizontal boundary, the conceptual
   viewport width."
  (fragment (error "Missing fragment.") :type Fragment :read-only T)
  (column   (error "Missing column.")   :type fixnum   :read-only T)
  (row      (error "Missing row.")      :type fixnum   :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text writer.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-text-layouts (text viewport-width
                              &key (start-column 0)
                                   (start-row    0))
  "Generates and returns for the TEXT an ordered list of its layouts,
   each such attributes the textual fragments with two-dimensional
   spatial information for its replication inside of a horizontal
   expanse tantamount to the VIEWPORT-WIDTH, optionally starting the
   first line, indexed by the START-ROW, at the specified START-COLUMN."
  (declare (type string text))
  (declare (type fixnum viewport-width))
  (declare (type fixnum start-column))
  (declare (type fixnum start-row))
  
  (let ((splits (get-text-split-points text)))
    (declare (type (list-of Fragment) splits))
    (the (list-of Text-Layout)
      (loop
        with column-index of-type fixnum = start-column
        with row-index    of-type fixnum = start-row
        
        while splits
        
        ;; Line has been filled entirely?
        ;; => Descend into next line without insertion.
        if (>= column-index viewport-width) do
          (setf column-index  0)
          (incf row-index)
        
        else collect
          (let* ((current-fragment
                  (pop splits))
                 (current-fragment-length
                  (fragment-length current-fragment)))
            (declare (type Fragment current-fragment))
            (declare (type fixnum   current-fragment-length))
            (cond
              ;; Linebreak?
              ((eq (fragment-type current-fragment) :linebreak)
                (prog1
                  (make-text-layout
                    current-fragment column-index row-index)
                  (setf column-index 0)
                  (incf row-index)))
              
              ;; Complete CURRENT-FRAGMENT matches into current line?
              ;; => Insert it at the current row's COLUMN-INDEX.
              ((<= (+ column-index current-fragment-length)
                   viewport-width)
                (prog1
                  (make-text-layout
                    current-fragment column-index row-index)
                  (incf column-index current-fragment-length)))
              
              ;; CURRENT-FRAGMENT would match into next empty line?
              ;; => Descend into next row and insert the fragment at its
              ;;    incipiency.
              ((<= current-fragment-length viewport-width)
                (incf row-index)
                (prog1
                  (make-text-layout current-fragment 0 row-index)
                  (setf column-index current-fragment-length)))
              
              ;; CURRENT-FRAGMENT too long for any line?
              ;; => Forcefully fill the current line with one moeity of
              ;;    the CURRENT-FRAGMENT, and push the dextral remnant
              ;;    as a new fragment unto the SPLITS stack.
              (T
                (destructuring-bind (left-split . right-split)
                    (split-fragment current-fragment
                      (- viewport-width column-index))
                  (declare (type Fragment right-split))
                  (declare (type Fragment left-split))
                  (push right-split splits)
                  (prog1
                    (make-text-layout left-split column-index row-index)
                    (incf column-index current-fragment-length))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of printing operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-indentation (left-margin destination)
  "Simulates a left margin by printing an indentation comprehending the
   LEFT-MARGIN tally of spaces to the DESTINATION and returns no value."
  (declare (type fixnum      left-margin))
  (declare (type destination destination))
  (format destination "~&~v@{~a~:*~}" left-margin #\Space)
  (values))

;;; -------------------------------------------------------

(defun print-vertical-spacing (previous-row-index current-row-index
                               destination)
  "Prints to the DESTINATION the tally of newlines requisite for
   reaching the CURRENT-ROW-INDEX from the PREVIOUS-ROW-INDEX and
   returns no value."
  (declare (type fixnum      previous-row-index))
  (declare (type fixnum      current-row-index))
  (declare (type destination destination))
  (format destination "~v%"
    (- current-row-index previous-row-index))
  (values))

;;; -------------------------------------------------------

(defun print-layout-content (layout destination)
  "Prints the text LAYOUT's content to the DESTINATION and returns no
   value."
  (declare (type Text-Layout layout))
  (declare (type destination destination))
  (unless (eq (fragment-type (text-layout-fragment layout)) :linebreak)
    (format destination "~a"
      (fragment-content
        (text-layout-fragment layout))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of "Text-Line" class.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Text-Line
  (:constructor make-text-line (&optional (layouts))))
  "The ``Text-Line'' class serves in the encapsulation of zero or more
   ``Text-Layout'' objects which are physically expected to compound
   into a single line."
  (layouts NIL :type (list-of Text-Layout) :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of text line assembler.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arrange-layouts-in-lines (layouts)
  "Creates and returns for the text LAYOUTS a list of text lines which
   comprehend in each member a line's representative layouts."
  (declare (type (list-of Text-Layout) layouts))
  (let ((lines       NIL)
        (line-buffer NIL)
        (row         0))
    (declare (type (list-of Text-Line)              lines))
    (declare (type (or null (vector Text-Layout *)) line-buffer))
    (declare (type fixnum                           row))
    
    (flet
        ((ensure-line-buffer ()
          "Ascertains the existency of a LINE-BUFFER by creating such
           upon its absence, otherwise accompassing no effect, and in
           any case returns no value."
          (unless line-buffer
            (setf line-buffer
              (make-array 0
                :element-type    '(or null Text-Layout)
                :initial-element NIL
                :adjustable      T
                :fill-pointer    0)))
          (values))
         
         (add-to-text-line (new-layout)
          "Appends the NEW-LAYOUT to the LINE-BUFFER's desinence and
           returns no value."
          (declare (type Text-Layout new-layout))
          (vector-push-extend new-layout line-buffer)
          (values))
         
         (conclude-current-line ()
          "If a LINE-BUFFER exists at the instant, pushes the same
           unto the LINES stack, and sets the LINE-BUFFER to ``NIL'',
           in any case returning no value."
          (when line-buffer
            (push
              (make-text-line
                (coerce line-buffer 'list))
              lines)
            (setf line-buffer NIL))
          (values)))
      
      (the (list-of Text-Line)
        (loop
          for layout of-type Text-Layout in layouts
          do
            ;; New line started?
            (when (> (text-layout-row layout) row)
              (ensure-line-buffer)
              (conclude-current-line)
              (incf row)
              (loop while (> (text-layout-row layout) row) do
                (push (make-text-line) lines)
                (incf row)))
            
            (ensure-line-buffer)
            (add-to-text-line layout)
          
          finally
            (conclude-current-line)
            (return
              (nreverse lines)))))))

;;; -------------------------------------------------------

(defun collect-text-lines (lines)
  "Returns a list comprehending the text LINES' contents.
   ---
   Empty lines are represented by empty strings."
  (declare (type (list-of Text-Line) lines))
  (the (list-of string)
    (loop for current-line of-type Text-Line in lines collect
      (with-output-to-string (line-content)
        (declare (type string-stream line-content))
        (dolist (current-layout (text-line-layouts current-line))
          (declare (type Text-Layout current-layout))
          (print-layout-content current-layout line-content))))))
