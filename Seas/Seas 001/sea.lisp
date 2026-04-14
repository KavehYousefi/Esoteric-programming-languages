;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the operations whose capacitation entails the
;; creation of a Seas program's executable plasmature, molded into a
;; veridicous two-dimensional reticulation of ``Icon''-valued cells;
;; proffered in conjunction with the routines permitting such a grid's
;; conspection and manipulation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the sea grid.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Sea ()
  ((width
    :initform      0
    :type          fixnum
    :documentation "The tally of columns imposed by the water surface,
                    limned via a horizontal catena of \"🌊\" symbols.")
   (depth
    :initform      0
    :type          fixnum
    :documentation "The tally of rows partaking in this sea.")
   (parcels
    :initform      (make-hash-table :test #'eql)
    :type          icon-matrix
    :documentation "Associates with each two-dimensional location in
                    the sea an ``Icon''."))
  (:documentation
    "The ``Sea'' class furnishes a two-dimensional Cartesian grid of
     icons representative of a Seas program."))

;;; -------------------------------------------------------

(defun prepare-a-pristine-sea ()
  "Creates and returns an initially vacant ``Sea''."
  (the Sea
    (make-instance 'Sea)))

;;; -------------------------------------------------------

(defmacro with-the-sea ((sea) &body body)
  "Evaluates the SEA, binds its slot ``width'' to the local symbol macro
   ``$width'', the ``height'' to ``$height'', and the ``parcels'' to
   the name ``$parcels'', evaluates the BODY forms, and returns the
   desinent form's results."
  (let ((evaluated-sea (gensym)))
    (declare (type symbol evaluated-sea))
    `(let ((,evaluated-sea ,sea))
       (declare (type Sea  ,evaluated-sea)
                (ignorable ,evaluated-sea))
       (with-slots (($width width) ($depth depth) ($parcels parcels))
           ,evaluated-sea
         (declare (type fixnum      $width)
                  (ignorable        $width))
         (declare (type fixnum      $depth)
                  (ignorable        $depth))
         (declare (type icon-matrix $parcels)
                  (ignorable        $parcels))
         ,@body))))

;;; -------------------------------------------------------

(defun request-the-sea-parcel-at (sea location)
  "Returns the icon empight at the LOCATION into the SEA."
  (declare (type Sea      sea))
  (declare (type location location))
  (the Icon
    (with-the-sea (sea)
      (multiple-value-bind (icon icon-exists-p)
          (gethash location $parcels)
        (declare (type (or null Icon) icon))
        (declare (type T              icon-exists-p))
        (unless icon-exists-p
          (setf icon
            (randomly-select-a-printable-icon)))
        icon))))

;;; -------------------------------------------------------

(defun set-the-sea-parcel-at (sea location new-icon)
  "Stores the NEW-ICON in the SEA parcel whose ponibility is
   accommodated by the LOCATION and returns no value."
  (declare (type Sea      sea))
  (declare (type location location))
  (declare (type Icon     new-icon))
  (with-the-sea (sea)
    (setf (gethash location $parcels) new-icon))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((sea Sea) (stream T))
  (declare (type Sea    sea))
  (declare (type stream stream))
  (with-the-sea (sea)
    (format stream "Sea (width = ~d, depth = ~d):" $width $depth)
    (loop for row of-type fixnum from -1 below $depth do
      (format stream "~&")
      (loop for column of-type fixnum from 0 below $width do
        (print-the-icon-to
          (request-the-sea-parcel-at sea
            (specify-a-location column row))
          stream))))
  (the Sea sea))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program loading operations.            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-the-first-program-line (sea source)
  "Generates the first Seas line, amenable to the special row index -1,
   from the SOURCE, stores the thus received content into the SEA, and
   returns no value."
  (declare (type Sea    sea))
  (declare (type string source))
  (with-the-sea (sea)
    (with-the-icon-iterator (request-the-next-icon source)
      (loop
        for current-column of-type fixnum from 0 by 1
        and next-icon      of-type Icon   = (request-the-next-icon)
        do
          (cond
            ((null-icon-p next-icon)
              (loop-finish))
            ((icons-match-p next-icon +WATER-WAVE+)
              (set-the-sea-parcel-at sea
                (specify-a-location current-column -1)
                next-icon)
              (incf $width))
            (T
              (error 'Invalid-Surface-Error
                :position  current-column
                :character next-icon))))))
  (values))

;;; -------------------------------------------------------

(defun read-a-program-line (sea source line-number)
  "Generates a Seas program line aboon the surface level, its ponibility
   vouchsafed by the zero-based LINE-NUMBER, stores thilk in the SEA,
   and returns no value."
  (declare (type Sea    sea))
  (declare (type string source))
  (declare (type fixnum line-number))
  (with-the-sea (sea)
    (with-the-icon-iterator (request-the-next-icon source)
      (loop
        for current-column of-type fixnum from 0 by 1
        and next-icon      of-type Icon   = (request-the-next-icon)
        do
          (cond
            ((null-icon-p next-icon)
              (loop-finish))
            ((icons-match-p next-icon +WATER-WAVE+)
              (error 'Invalid-Character-Error
                :position
                  (specify-a-location current-column line-number)
                :character
                  next-icon))
            ((> current-column (1- $width))
              (error 'Invalid-Line-Length-Error
                :expected-length $width
                :actual-length   (1+ current-column)
                :line-number     (1+ line-number)))
            (T
              (set-the-sea-parcel-at
                sea
                (specify-a-location current-column line-number)
                next-icon)
              (setf $depth (1+ line-number))))
        finally
          (when (/= current-column $width)
            (error 'Invalid-Line-Length-Error
              :expected-length $width
              :actual-length   (1+ current-column)
              :line-number     (1+ line-number))))))
  (values))

;;; -------------------------------------------------------

(defun read-a-sea-from-the-string (source)
  "Creates and returns a fresh ``Sea'' whose content's fons et origi is
   realized in the SOURCE string."
  (declare (type string source))
  (let ((the-deep-blue-sea (prepare-a-pristine-sea)))
    (declare (type Sea the-deep-blue-sea))
    (with-input-from-string (source-stream source)
      (declare (type string-stream source-stream))
      (loop
        for current-line
          of-type (or null string)
          =       (read-line source-stream NIL NIL)
        and line-number
          from    -1
          by       1
        and first-line-p
          of-type boolean
          =       T
          then    NIL
        while current-line do
          (if first-line-p
            (read-the-first-program-line
              the-deep-blue-sea
              current-line)
            (read-a-program-line
              the-deep-blue-sea
              current-line
              line-number))))
    (the Sea the-deep-blue-sea)))
