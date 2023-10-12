;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the provision of the line search operations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of search facilities.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun search-upwards-for-bit (program expected-prefix)
  "Proceeding from the position above the PROGRAM's current line, moves
   upwards while searching for a line whose prefix bit matches the
   EXPECTED-PREFIX, on success returning the line nearest to the start
   point, otherwise responding with ``NIL''."
  (declare (type Program program))
  (declare (type bit     expected-prefix))
  (the (or null Line)
    (when (program-current-line program)
      (loop
        for probed-line
          of-type (or null Line)
          =       (program-previous-line program)
          then    (line-predecessor      probed-line)
        while
          probed-line
        when (line-prefix-matches-p probed-line expected-prefix) do
          (return probed-line)
        finally
          (return NIL)))))

;;; -------------------------------------------------------

(defun search-downwards-for-bit (program expected-prefix)
  "Proceeding from the position below the PROGRAM's current line, moves
   downwards while searching for a line whose prefix bit matches the
   EXPECTED-PREFIX, on success returning the line nearest to the start
   point, otherwise responding with ``NIL''."
  (declare (type Program program))
  (declare (type bit     expected-prefix))
  (the (or null Line)
    (when (program-current-line program)
      (loop
        for probed-line
          of-type (or null Line)
          =       (program-next-line program)
          then    (line-successor    probed-line)
        while
          probed-line
        when (line-prefix-matches-p probed-line expected-prefix) do
          (return probed-line)
        finally
          (return NIL)))))

;;; -------------------------------------------------------

(defun find-label (program expected-label)
  "Proceeding from the line immediately below the PROGRAM's current one,
   searches for a line designated by the EXPECTED-LABEL, contingently
   wrapping around if having reached the PROGRAM's, and probing in its
   desinent step the current line, on success returning the first line
   matching the predicate, otherwise responding with ``NIL''."
  (declare (type Program program))
  (declare (type string  expected-label))
  (the (or null Line)
    (when (program-current-line program)
      (let ((initial-line (program-current-line program)))
        (declare (type Line initial-line))
        (loop
          for probed-line
            of-type Line
            =       (program-next-line  program :wrap-around-p T)
            then    (program-line-after program probed-line
                                        :wrap-around-p T)
          ;; Match?
          if (line-label-matches-p probed-line expected-label) do
            (return probed-line)
          ;; No match, and returned to start?
          else if (eq probed-line initial-line) do
            (return NIL))))))
