;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the dedicated object encapsulations proceeding
;; from the interface "NGObject", their rationalization is accompassed
;; by the necessity of their attendance and manipulation during the
;; interpreter's operations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of NGObjects.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct NGObject
  "The ``NGObject'' interface accommodates a common foundry for all
   entities intended to represent NeverGonna objects."
  (type (error "Missing object type.") :type ngobject-type))

;;; -------------------------------------------------------

(defstruct (NGBoolean
  (:include     NGObject)
  (:constructor make-ngboolean (value &aux (type :boolean))))
  "The ``NGBoolean'' class implements the ``NGObject'' interface in its
   pursuit to represent a Boolean truth value."
  (value (error "Missing Boolean value.") :type boolean))

;;; -------------------------------------------------------

(defstruct (NGInteger
  (:include     NGObject)
  (:constructor make-nginteger (value &aux (type :integer))))
  "The ``NGInteger'' class implements the ``NGObject'' interface in its
   pursuit to represent a signed integer value of any magnitude."
  (value (error "Missing integer value.") :type integer))

;;; -------------------------------------------------------

(defstruct (NGString
  (:include     NGObject)
  (:constructor make-ngstring (value &aux (type :string))))
  "The ``NGString'' class implements the ``NGObject'' interface in its
   pursuit to represent a string value of arbitrary extent."
  (value (error "Missing string value.") :type string))

;;; -------------------------------------------------------

(defgeneric ngobject-value (ngobject)
  (:documentation
    "Returns the object autochthonous to Common Lisp's infrastructure
     for the NGOBJECT.")
  
  (:method ((ngobject NGBoolean))
    (declare (type NGBoolean ngobject))
    (the boolean
      (ngboolean-value ngobject)))
  
  (:method ((ngobject NGInteger))
    (declare (type NGInteger ngobject))
    (the integer
      (nginteger-value ngobject)))
  
  (:method ((ngobject NGString))
    (declare (type NGString ngobject))
    (the string
      (ngstring-value ngobject))))

;;; -------------------------------------------------------

(defgeneric ngobject-get-boolean-truth-value (ngobject)
  (:documentation
    "Returns the ``boolean'' truth value convenable to the NGOBJECT's
     nature, or signals an error of an unspecified type upon its
     disqualification for such a purpose.")
  
  (:method ((ngobject NGBoolean))
    "Returns the ``boolean'' truth value stored in the NGOBJECT."
    (declare (type NGBoolean ngobject))
    (the boolean
      (ngboolean-value ngobject)))
  
  (:method ((ngobject NGObject))
    (declare (type NGObject ngobject))
    (error "The object ~s, of the type ~s, cannot be transliterated ~
            into a Boolean truth value."
      ngobject
      (class-name (class-of ngobject)))))
