;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the bespoke condition types assigned a certain
;; mete of involvement in a Seas's program's reception, parsing, and
;; execution.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the bespoke conditions.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Seas-Error (error)
  ()
  (:documentation
    "The ``Seas-Error'' condition type establishes a common foundry for
     all error representations dedicated to anomalous situations in the
     context of a Seas program's lexical analyzation, parsing, and
     interpretation."))

;;; -------------------------------------------------------

(define-condition Invalid-Character-Error (Seas-Error)
  ((position
    :initarg       :position
    :initform      (error "Missing offending position.")
    :reader        invalid-character-error-position
    :type          location
    :documentation "The position at which the offending CHARACTER has
                    been empight.")
   (character
    :initarg       :character
    :initform      (error "Missing offending character.")
    :reader        invalid-character-error-character
    :type          Icon
    :documentation "The invalid character which instigated this
                    anomalous circumstance."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Character-Error condition))
      (declare (type stream                  stream))
      (multiple-value-bind (position-x position-y)
          (disassemble-the-location
            (invalid-character-error-position condition))
        (declare (type integer position-x))
        (declare (type integer position-x))
        (format stream "The icon \"~a\" in column ~d of line ~d ~
                        is invalid."
          (invalid-character-error-character condition)
          position-x position-y))))
  (:documentation
    "The ``Invalid-Character-Error'' condition type signals an anomalous
     situation whose provenance issues from the occurrency of an invalid
     character in a Seas program, or of a valid character at a location
     inadmissible to a certain content."))

;;; -------------------------------------------------------

(define-condition Invalid-Surface-Error (Seas-Error)
  ((position
    :initarg       :position
    :initform      (error "Missing offending position.")
    :reader        invalid-surface-error-position
    :type          fixnum
    :documentation "The zero-based column of the top line into the
                    source code grid at which the offending, non-🌊
                    character wones.")
   (character
    :initarg       :character
    :initform      (error "Missing offending character.")
    :reader        invalid-surface-error-character
    :type          Icon
    :documentation "The invalid, non-🌊 character at the zero-based
                    POSITION into the source code grid's top line."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Surface-Error condition))
      (declare (type stream                stream))
      (format stream "The icon \"~a\" at the position ~d into ~
                      the top line does not represent the sole valid ~
                      entity \"🌊\" for the surface designment."
        (invalid-surface-error-character condition)
        (invalid-surface-error-position  condition))))
  (:documentation
    "The ``Invalid-Surface-Error'' condition type signals an anomalous
     situation whose provenance issues from an invalid symbol's presence
     in the topmost Seas program line, a etiology thilk lays its
     amplection around any character except for the \"🌊\" program
     terminator."))

;;; -------------------------------------------------------

(define-condition Invalid-Line-Length-Error (Seas-Error)
  ((expected-length
    :initarg       :expected-length
    :initform      (error "Missing expected line length.")
    :reader        invalid-line-length-error-expected-length
    :type          fixnum
    :documentation "The tally of characters expected for each grid line
                    to be attended to.")
   (actual-length
    :initarg       :actual-length
    :initform      (error "Missing actual line length.")
    :reader        invalid-line-length-error-actual-length
    :type          fixnum
    :documentation "The offending line's actual, inadmissible length.")
   (line-number
    :initarg       :line-number
    :initform      (error "Missing offending line number.")
    :reader        invalid-line-length-error-line-number
    :type          fixnum
    :documentation "The one-based index of the offending line."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Line-Length-Error condition))
      (declare (type stream                    stream))
      (format stream "The length of the line number ~d, measuring ~
                      ~d characters, does not match the expected ~
                      width of ~d."
        (invalid-line-length-error-line-number     condition)
        (invalid-line-length-error-actual-length   condition)
        (invalid-line-length-error-expected-length condition))))
  (:documentation
    "The ``Invalid-Line-Length-Error'' condition type signals an
     anomalous situation whose provenance issues from a specification of
     a Seas source line deviating in its length from the topmost row's
     imposed width."))

;;; -------------------------------------------------------

(define-condition Empty-Stack-Error (Seas-Error)
  ((offended-stack
    :initarg       :offended-stack
    :initform      (error "Missing offended stack.")
    :reader        empty-stack-error-offended-stack
    :type          Integer-Stack
    :documentation "The stack whose vacancy during an aspired indagation
                    or removal has instigated this error."))
  (:report
    (lambda (condition stream)
      (declare (type Empty-Stack-Error condition))
      (declare (ignore                 condition))
      (declare (type stream            stream))
      (format stream "Cannot pop from an empty stack.")
      (values)))
  (:documentation
    "The ``Empty-Stack-Error'' condition type signals an anomalous
     situation whose provenance issues from an attempt to indagate or
     remove a vacant stack's top element."))
