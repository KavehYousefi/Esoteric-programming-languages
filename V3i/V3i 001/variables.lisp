;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the variable castaldy entity, its devers
;; enumerating the validation, indagation, and modulation of the treble
;; variable objects commorant in a V3i program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition types.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition V3i-Error (error)
  ()
  (:documentation
    "The ``V3i-Error'' condition type accoutres a common substratum for
     all conditions nuncupated to a V3i program's processing."))

;;; -------------------------------------------------------

(define-condition Undefined-Variable-Error (V3i-Error simple-error)
  ((variable
    :initarg       :variable
    :initform      (error "Missing undefined variable name.")
    :reader        undefined-variable-error-variable
    :type          variable-name
    :documentation "The name of the variable which, having not yet been
                    defined, was pursued for an indagation of
                    modulation."))
  (:documentation
    "The ``Undefined-Variable-Error'' condition type serves in the
     communication of an anomalous situation whose etiology ensues from
     the attempt to perquire or modify a varaible not yet defined with
     a value."))

;;; -------------------------------------------------------

(define-condition Invalid-Variable-Name-Error (V3i-Error simple-error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending identifier.")
    :reader        invalid-variable-name-error-offending-name
    :type          string
    :documentation "The invalid variable name tried for an action."))
  (:documentation
    "The ``Invalid-Variable-Name-Error'' condition type serves in the
     communication of an anomalous situation whose etiology ensues from
     the attempt to define, query, or modify a variable of an invalid
     name."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of condition operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun signal-undefined-variable-error (variable)
  "Signals an error of the type ``Undefined-Variable-Error'' which
   communicates the VARIABLE as the culprit."
  (declare (type variable-name variable))
  (error 'Undefined-Variable-Error
    :variable         variable
    :format-control   "The variable \"~a\" is not yet defined."
    :format-arguments (list variable)))

;;; -------------------------------------------------------

(defun signal-invalid-variable-name-error (offending-name)
  "Signals an error of the type ``Invalid-Variable-Name-Error'' which
   communicates the OFFENDING-NAME as the instigating encheson."
  (declare (type string offending-name))
  (error 'Invalid-Variable-Name-Error
    :offending-name   offending-name
    :format-control   "The name \"~a\" does not identify a variable."
    :format-arguments (list offending-name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable class.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (V3iVariable
  (:constructor make-v3ivariable (name)))
  "The ``V3iVariable'' class serves in the encapsulation of a
   succedaneum's notion."
  (name      (error "Missing variable name.")
             :type      variable-name
             :read-only T)
  (value     0
             :type      v3i-object
             :read-only NIL)
  (defined-p NIL
             :type      boolean
             :read-only NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of foundational variable operations.          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-initial-variables ()
  "Creates and returns a fresh variable set populated by the default
   configurations."
  (the variable-set-entries
    (list
      (cons "x" (make-v3ivariable "x"))
      (cons "y" (make-v3ivariable "y"))
      (cons "z" (make-v3ivariable "z")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of variable set.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Variable-Set ()
  ((entries
    :initform      (build-initial-variables)
    :reader        variable-set-entries
    :type          variable-set-entries
    :documentation "Maps the recognized variable names to representative
                    ``V3iVariable'' objects."))
  (:documentation
    "The ``Variable-Set'' class is apportioned the dever of the three
     V3i variable's castaldy, its bailiwick extending over their
     definition, indagation, and modulation."))

;;; -------------------------------------------------------

(defun make-variable-set ()
  "Creates and returns a ``Variable-Set'' whose entries are yet
   undefined."
  (the Variable-Set
    (make-instance 'Variable-Set)))

;;; -------------------------------------------------------

(defun get-entry-for-variable (variables name)
  "Returns the entry amenable to the NAME in the VARIABLES table, or
   signals an error of an unspecified type upon its disrespondency."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (the (or null (cons variable-name V3iVariable))
    (assoc name
      (variable-set-entries variables)
      :test #'string=)))

;;; -------------------------------------------------------

(defun valid-variable-name-p (variables name)
  "Determines whether the NAME identifes a recognized variable in the
   VARIABLES table, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (the boolean
    (get-boolean-value-of
      (get-entry-for-variable variables name))))

;;; -------------------------------------------------------

(defun get-variable (variables name)
  "Returns the variable amenable to the NAME in the VARIABLES table, or
   signals an error of the type ``Invalid-Variable-Name-Error'' upon its
   disrespondency."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (the V3iVariable
    (or (cdr (get-entry-for-variable variables name))
        (signal-invalid-variable-name-error name))))

;;; -------------------------------------------------------

(defun define-variable (variables name initial-value)
  "Marks the variable amenable to the NAME as defined, if not yet done,
   stores the INITIAL VALUE in the same, and returns no value."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (declare (type v3i-object   initial-value))
  (let ((variable (get-variable variables name)))
    (declare (type V3iVariable variable))
    (setf (v3ivariable-defined-p variable) T)
    (setf (v3ivariable-value     variable) initial-value))
  (values))

;;; -------------------------------------------------------

(defun determine-if-variable-is-defined (variables name)
  "Determines whether a variable amenable to the NAME exists in the
   VARIABLES table and, if ascertained, is defined, returning on
   confirmation the respective ``V3iVariable'' instance; otherwise an
   error of the type ``Undefined-Variable-Error'' is signaled."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (let ((variable (get-variable variables name)))
    (declare (type V3iVariable variable))
    (the V3iVariable
      (if (v3ivariable-defined-p variable)
        variable
        (signal-undefined-variable-error name)))))

;;; -------------------------------------------------------

(defun variable-value (variables name)
  "Returns the value stored in the variable amenable to the NAME in the
   VARIABLES table, if the same constitutes a valid identifier and
   concomitantly is defined; otherwise signals an error of an
   unspecified type."
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (the v3i-object
    (v3ivariable-value
      (determine-if-variable-is-defined variables name))))

;;; -------------------------------------------------------

(defun (setf variable-value) (new-value variables name)
  "Stores the NEW-VALUE in the variable amenable to the NAME in the
   VARIABLES table and returns no value."
  (declare (type v3i-object   new-value))
  (declare (type Variable-Set variables))
  (declare (type string       name))
  (setf (v3ivariable-value
          (determine-if-variable-is-defined variables name))
        new-value)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((variables Variable-Set) (stream T))
  (declare (type Variable-Set variables))
  (declare (type destination  stream))
  (format stream "~&Variable-Set:")
  (loop
    for (variable-name . variable)
      of-type (variable-name . V3iVariable)
      in      (variable-set-entries variables)
    do
      (format stream "~&  \"~a\" => ~a" variable-name
        (if (v3ivariable-defined-p variable)
          (v3ivariable-value variable)
          "<undefined>"))))
