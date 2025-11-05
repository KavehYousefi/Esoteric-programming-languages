;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the "jump table", a warklume for the bilateral
;; alligation of jump points in a "5" program by adminiculum of the
;; representative forward and back jump points' zero-based indices into
;; the ensconcing program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Jump-Table".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Jump-Table ()
  ((connections
    :initform      (make-hash-table :test #'eql)
    :type          (hash-table-of fixnum fixnum)
    :documentation "Maps the jump start points to their corresponding
                    end points via their zero-based indices into the
                    underlying piece of \"5\" source code."))
  (:documentation
    "The ``Jump-Table'' class applies itself to the castaldy of the
     jump points in a \"5\" program, mediated by adminiculum of their
     zero-based positions into the source code, and airted from the
     jump start instructions towards the end significators in a
     unilateral manner."))

;;; -------------------------------------------------------

(defun make-an-empty-jump-table ()
  "Creates and returns a fresh ``Jump-Table'' whose status at the
   inchoation is delineated by a complete carency of entries."
  (the Jump-Table
    (make-instance 'Jump-Table)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Connects the jump START-POINT with the matching END-POINT in the
   jump TABLE and returns no value."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (with-slots (connections) table
    (declare (type (hash-table-of fixnum fixnum) connections))
    (psetf
      (gethash start-point connections) end-point
      (gethash end-point   connections) start-point))
  (values))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (source)
  "Creates and returns a fresh ``Jump-Table'' dedicated to the
   alligation of the jump points in the piece of Esofish SOURCE code by
   adminiculum of their zero-based indices."
  (declare (type simple-string source))
  (the Jump-Table
    (loop
      with jump-table
        of-type Jump-Table
        =       (make-an-empty-jump-table)
      with start-points
        of-type (list-of fixnum)
        =       NIL
      
      for current-token
        of-type character
        across  source
      and current-position
        of-type fixnum
        from    0
        by      1
      
      if (char= current-token #\[) do
        (push current-position start-points)
      else if (char= current-token #\]) do
        (if start-points
          (connect-the-jump-points jump-table
            (pop start-points)
            current-position)
          (error 'Unmatched-Jump-Point-Error
            :position    current-position
            :description "jump end point (\"]\")"))
      end
      
      finally
        (if start-points
          (error 'Unmatched-Jump-Point-Error
            :position    (first (last start-points))
            :description "jump start point")
          (return jump-table)))))

;;; -------------------------------------------------------

(defun locate-the-jump-end-point (table start-point)
  "Returns the jump destination to the START-POINT as maintained by
   the jump TABLE.
   ---
   Upon such an affiliation's lacuna, an error of the type
   ``Missing-Jump-Point-Error'' is signaled."
  (declare (type Jump-Table table))
  (declare (type fixnum     start-point))
  (the fixnum
    (with-slots (connections) table
      (declare (type (hash-table-of fixnum fixnum) connections))
      (or (gethash start-point connections)
          (error 'Missing-Jump-Point-Error
            :position    start-point
            :description "destination to the jump start command")))))
