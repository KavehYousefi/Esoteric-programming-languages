;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the definition of the conditions appertaining to
;; a Celum program's interpretation, in particular the expected errors.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Missing-Prefix-Line-Error (simple-error)
  ((offended-program
    :initarg       :offended-program
    :initform      (error "Missing program.")
    :type          Program
    :documentation "The Celum program during whose execution this
                    anomaly's transpiration has been registered.")
   (expected-prefix
    :initarg       :expected-prefix
    :initform      (error "Missing expected prefix bit.")
    :type          bit
    :documentation "The prefix bit which could not be detected in any
                    line's inchoation."))
  (:documentation
    "The ``Missing-Prefix-Line-Error'' serves to signal an anomalous
     situation whose etiology issues from the failure to find a line
     designated by a certain prefix bit."))

;;; -------------------------------------------------------

(defun signal-missing-prefix-line-error (offended-program
                                         expected-prefix)
  "Signals an error of the type ``Missng-Prefix-Line-Error'', apprizing
   about the OFFENDED-PROGRAM being destitute of a line bearing the
   EXPECTED-PREFIX bit."
  (declare (type Program offended-program))
  (declare (type bit     expected-prefix))
  (error 'Missing-Prefix-Line-Error
    :offended-program offended-program
    :expected-prefix  expected-prefix
    :format-control   "No line bearing the prefix bit ~d could ~
                       be found."
    :format-arguments (list expected-prefix)))
