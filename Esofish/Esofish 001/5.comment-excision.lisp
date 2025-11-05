;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the operations whose bailiwick's entelchia
;; accompts for such directed at a "5" program's excision of commentary
;; tmemata, these being demarcated by an ensconcement in a jumelle of
;; parentheses, "(" and ")".
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of comment excision operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-start-of-the-next-comment (source start)
  "Proceeding from the inclusive START position into the SOURCE,
   searches for the next comment instigator symbol, \"(\", on
   confirmation returning its zero-based position into the SOURCE;
   otherwise responds with the SOURCE's length."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position #\( source :start start :test #'char=)
        (length source))))

;;; -------------------------------------------------------

(defun locate-the-end-of-the-next-comment (source start)
  "Proceeding from the inclusive START position into the SOURCE,
   searches for the next comment terminator symbol, \"(\", on
   confirmation returning its zero-based position into the SOURCE;
   otherwise signals an error of the type
   ``Unterminated-Comment-Error''."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (the fixnum
    (or (position #\) source :start start :test #'char=)
        (error 'Unterminated-Comment-Error :start-point start))))

;;; -------------------------------------------------------

(defun locate-the-next-comment (source start)
  "Proceeding from the START position into the SOURCE, searches for the
   next comment, instigated by a \"(\" token and terminating in a \")\"
   symbol, and returns two values:
     (1) If a comment could be detected, the zero-based position of its
         initializing character \"(\" in the SOURCE; otherwise the
         SOURCE's length.
     (2) If a comment could be detected, the zero-based position
         immediately succeeding its terminating character \")\" in the
         SOURCE; otherwise the SOURCE's length.
   ---
   If a comment's inchoation could be attested, yet in the carency of
   its desinence, an error of the type ``Unterminated-Comment-Error'' is
   signaled."
  (declare (type simple-string source))
  (declare (type fixnum        start))
  (let ((comment-start-point
          (locate-the-start-of-the-next-comment source start)))
    (declare (type fixnum comment-start-point))
    (the (values fixnum fixnum)
      (values
        comment-start-point
        (if (< comment-start-point (length source))
          (1+
            (locate-the-end-of-the-next-comment
              source
              comment-start-point))
          comment-start-point)))))

;;; -------------------------------------------------------

(defun excise-comments-from-the-5-code (source)
  "Creates and returns a fresh simple string whose content represents
   the piece of \"5\" SOURCE code purged of all commentary tmemata."
  (declare (type simple-string source))
  (the simple-string
    (convert-into-a-simple-string
      (let ((copy-start-point 0))
        (declare (type fixnum copy-start-point))
        (with-output-to-string (purged-string)
          (declare (type string-stream purged-string))
          (loop do
            (multiple-value-bind (comment-start-point
                                  comment-end-point)
                (locate-the-next-comment source copy-start-point)
              (declare (type fixnum comment-start-point))
              (declare (type fixnum comment-end-point))
              (format purged-string "~a"
                (subseq source copy-start-point comment-start-point))
              (setf copy-start-point comment-end-point))
            until
              (>= copy-start-point
                  (length source))))))))
