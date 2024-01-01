;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the conditions appertaining to the lexical
;; analyzation, parsing, and interpretation of a NeverGonna program.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of condition type "NeverGonna-Error".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition NeverGonna-Error (error)
  ()
  (:documentation
    "The ``NeverGonna-Error'' condition type furnishes a common foundry
     for all error appertaining especially to the lexical analyzation,
     parsing, or interpretation of a NeverGonna program."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of condition type "Invalid-Variable-Error".       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Variable-Error (NeverGonna-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        invalid-variable-error-offending-name
    :type          string
    :documentation "The variable name whose ineligibility has instigated
                    this erroneous condition."))
  (:report
    (lambda (condition destination)
      (declare (type Invalid-Variable-Error condition))
      (declare (type destination            destination))
      (format destination "The identifier ~s constitutes an invalid ~
                           name for a variable."
        (invalid-variable-error-offending-name condition))))
  (:documentation
    "The ``Invalid-Variable-Error'' condition type serves to signal an
     anomalous situation arising by an attempt to declare a variable
     with a name whose assessment fails a covenableness."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of condition type "Unknown-Variable-Error".       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Unknown-Variable-Error (NeverGonna-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        unknown-variable-error-offending-name
    :type          string
    :documentation "The variable name whose attempt at an assignment,
                    indagation or manipulation, because of a prevenient
                    declaration's lacuna, has instigated this
                    predicament."))
  (:report
    (lambda (condition destination)
      (declare (type Unknown-Variable-Error condition))
      (declare (type destination            destination))
      (format destination "Cannot access or modify the variable ~s,
                           forecause it has not been declared yet."
        (unknown-variable-error-offending-name condition))))
  (:documentation
    "The ``Unknown-Variable-Error'' condition type serves to signal an
     anomalous situation arising by an attempt to assign, query, or
     modify a variable which has not yet been declared."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of condition type "Incomplete-Variable-Error".    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Incomplete-Variable-Error (NeverGonna-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        incomplete-variable-error-offending-name
    :type          string
    :documentation "The variable name whose attempt at a perquisition
                    has instigated this error, as it may have been
                    declared, but not yet assigned a value."))
  (:report
    (lambda (condition destination)
      (declare (type Incomplete-Variable-Error condition))
      (declare (type destination               destination))
      (format destination "Cannot query the value of the variable ~s ~
                           as it has not yet been assigned a value."
        (incomplete-variable-error-offending-name condition))))
  (:documentation
    "The ``Incomplete-Variable-Error'' condition type serves to signal
     an anomalous situation arising by an attempt to query a variable
     which may or may not have been declared, but, a fortiori, has not
     been assigned a value yet."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of condition type "Duplicate-Variable-Error".     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Duplicate-Variable-Error (NeverGonna-Error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending name.")
    :reader        duplicate-variable-error-offending-name
    :type          string
    :documentation "The variable name whose attempt at an iterum
                    declaration has instigated this predicament."))
  (:report
    (lambda (condition destination)
      (declare (type Duplicate-Variable-Error condition))
      (declare (type destination              destination))
      (format destination "Cannot declare a variable with the name ~s ~
                           as such has already been defined."
        (duplicate-variable-error-offending-name condition))))
  (:documentation
    "The ``Duplicate-Variable-Error'' condition type serves to signal an
     anomalous situation arising by an attempt to declare a variable
     whose name has already been registered, that is, either declared or
     both declared and assigned."))
