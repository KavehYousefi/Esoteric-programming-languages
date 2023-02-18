;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the definition of the "SOAPSet" interface, the
;; implementing "Exclusive-SOAPSet" and "Inclusive-SOAPSet" classes, and
;; the appertaining operations, serving in the representation of the
;; set data structure.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "SOAPSet".                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (SOAPSet)
  "The ``SOAPSet'' interface represents the foundation of all set
   implementations, bivious in their nature betwixt the inclusive and
   the exclusive variant, with the former subtype pertaining to finite
   sets, while the latter accommodates infinite expanses.
   ---
   Two subtypes of the ``SOAPSet'' class exist, each a complementary
   whole's representative by advocating a particular principle. The
   ``Inclusive-SOAPSet'' entity is ultimately founded upon an empty set,
   appropriating a finite tally of members from the ensconcing universe
   U. Athwart to this perspective, the ``Exclusive-SOAPSet'' inherits
   the complete universe U, thus being infinite in its haecceity, but
   excludes a finite tally of members from its cleronomy. In simple
   terms:
     inclusive SOAPSet = empty set  + finite elements of universe U
     exclusive SOAPSet = universe U - finite elements of universe U")

;;; -------------------------------------------------------

(defgeneric soapset-flip-membership (set element)
  (:documentation
    "Flips the membership of the ELEMENT in the SET, that is, removes it
     if it is present in the SET, otherwise adds it to the same, in any
     case returning no value."))

;;; -------------------------------------------------------

(defgeneric soapset-union (left-set right-set)
  (:documentation
    "Determines the union of the LEFT-SET and the RIGHT-SET and returns
     a new ``SOAPSet'' representative of the result.
     ---
     Neither the LEFT-SET nor the RIGHT-SET are modified in the
     process; additionally, the output instance does not share any
     properties with its progenitors."))

;;; -------------------------------------------------------

(defgeneric soapset-intersection (left-set right-set)
  (:documentation
    "Determines the intersection of the LEFT-SET and the RIGHT-SET and
     returns a new ``SOAPSet'' representative of the result.
     ---
     Neither the LEFT-SET nor the RIGHT-SET are modified in the
     process; additionally, the output instance does not share any
     properties with its progenitors."))

;;; -------------------------------------------------------

(defgeneric soapset-left-difference (left-set right-set)
  (:documentation
    "Determines the difference of the LEFT-SET reduced by the RIGHT-SET
     and returns a new ``SOAPSet'' representative of the result.
     ---
     Neither the LEFT-SET nor the RIGHT-SET are modified in the
     process; additionally, the output instance does not share any
     properties with its progenitors."))

;;; -------------------------------------------------------

(defgeneric soapset-right-difference (left-set right-set)
  (:documentation
    "Determines the difference of the RIGHT-SET reduced by the LEFT-SET
     and returns a new ``SOAPSet'' representative of the result.
     ---
     Neither the LEFT-SET nor the RIGHT-SET are modified in the
     process; additionally, the output instance does not share any
     properties with its progenitors."))

;;; -------------------------------------------------------

(defgeneric soapset-complement (set)
  (:documentation
    "Determines the complement of the SET and returns a new ``SOAPSet''
     representative of the result.
     ---
     The input SET is not modified in the process; additionally, the
     output instance does not share any properties with its
     progenitor."))

;;; -------------------------------------------------------

(defgeneric soapset-subset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes a subset of the RIGHT-SET,
     returning on confirmation a ``boolean'' value of ``T'', otherwise
     ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-proper-subset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes a proper subset of the
     RIGHT-SET, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-not-subset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes no subset of the RIGHT-SET,
     returning on confirmation a ``boolean'' value of ``T'', otherwise
     ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-superset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes a superset of the
     RIGHT-SET, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-proper-superset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes a proper superset of the
     RIGHT-SET, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-not-superset-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET constitutes no superset of the
     RIGHT-SET, returning on confirmation a ``boolean'' value of ``T'',
     otherwise ``NIL''."))

;;; -------------------------------------------------------

(defgeneric soapset-equal-p (left-set right-set)
  (:documentation
    "Checks whether the LEFT-SET is equal to the RIGHT-SET, returning on
     confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Exclusive-SOAPSet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Exclusive-SOAPSet
  (:include     SOAPSet)
  (:constructor make-exclusive-soapset
    (&optional (blacklist NIL)
     &aux      (blacklist (copy-list blacklist)))))
  "The ``Exclusive-SOAPSet'' class represents an infinite set of natural
   numbers defined by those elements from the universe U *not* present
   in the particular set instance.
   ---
   The universe elements absent from an exclusive set are represented by
   a \"blacklist\", a simple list acting in the agency of a set. In
   corollary, adding to such a blacklist effectively removes an element
   from the set; while removing a member from the list declares it an
   actual set member."
  (blacklist NIL :type number-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Inclusive-SOAPSet".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Inclusive-SOAPSet
  (:include     SOAPSet)
  (:constructor make-inclusive-soapset
    (&optional (elements NIL)
     &aux      (elements (copy-list elements)))))
  "The ``Inclusive-SOAPSet'' class represents a finite set of natural
   numbers defined by the elements present in the same."
  (elements NIL :type number-list))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-flip-membership".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-flip-membership ((set     Exclusive-SOAPSet)
                                    (element integer))
  (declare (type Exclusive-SOAPSet set))
  (declare (type integer           element))
  (if (member element (exclusive-soapset-blacklist set) :test #'=)
    (setf (exclusive-soapset-blacklist set)
      (delete element
        (exclusive-soapset-blacklist set)
        :test #'=))
    (push element (exclusive-soapset-blacklist set)))
  (values))

;;; -------------------------------------------------------

(defmethod soapset-flip-membership ((set     Inclusive-SOAPSet)
                                    (element integer))
  (declare (type Inclusive-SOAPSet set))
  (declare (type integer           element))
  (if (member element (inclusive-soapset-elements set) :test #'=)
    (setf (inclusive-soapset-elements set)
      (delete element
        (inclusive-soapset-elements set)
        :test #'=))
    (push element (inclusive-soapset-elements set)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-union".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-union ((left-set  Exclusive-SoapSet)
                          (right-set Exclusive-SoapSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (intersection
        (exclusive-soapset-blacklist left-set)
        (exclusive-soapset-blacklist right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-union ((left-set  Exclusive-SOAPSet)
                          (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (set-difference
        (exclusive-soapset-blacklist left-set)
        (inclusive-soapset-elements  right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-union ((left-set  Inclusive-SOAPSet)
                          (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (set-difference
        (exclusive-soapset-blacklist right-set)
        (inclusive-soapset-elements  left-set)))))

;;; -------------------------------------------------------

(defmethod soapset-union ((left-set  Inclusive-SOAPSet)
                          (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (union
        (inclusive-soapset-elements left-set)
        (inclusive-soapset-elements right-set)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-intersection".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-intersection ((left-set  Exclusive-SOAPSet)
                                 (right-set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (union
        (exclusive-soapset-blacklist left-set)
        (exclusive-soapset-blacklist right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-intersection ((left-set  Exclusive-SOAPSet)
                                 (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (set-difference
        (inclusive-soapset-elements  right-set)
        (exclusive-soapset-blacklist left-set)))))

;;; -------------------------------------------------------

(defmethod soapset-intersection ((left-set  Inclusive-SOAPSet)
                                 (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (set-difference
        (inclusive-soapset-elements  left-set)
        (exclusive-soapset-blacklist right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-intersection ((left-set  Inclusive-SOAPSet)
                                 (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (intersection
        (inclusive-soapset-elements left-set)
        (inclusive-soapset-elements right-set)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-left-difference".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-left-difference ((left-set  Exclusive-SOAPSet)
                                    (right-set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (set-difference
        (exclusive-soapset-blacklist right-set)
        (exclusive-soapset-blacklist left-set)))))

;;; -------------------------------------------------------

(defmethod soapset-left-difference ((left-set  Exclusive-SOAPSet)
                                    (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (union
        (exclusive-soapset-blacklist left-set)
        (inclusive-soapset-elements  right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-left-difference ((left-set  Inclusive-SOAPSet)
                                    (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (intersection
        (inclusive-soapset-elements  left-set)
        (exclusive-soapset-blacklist right-set)))))

;;; -------------------------------------------------------

(defmethod soapset-left-difference ((left-set  Inclusive-SOAPSet)
                                    (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (set-difference
        (inclusive-soapset-elements left-set)
        (inclusive-soapset-elements right-set)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-right-difference".         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-right-difference ((left-set  SOAPSet)
                                     (right-set SOAPSet))
  (declare (type SOAPSet left-set))
  (declare (type SOAPSet right-set))
  (the SOAPSet
    (soapset-left-difference right-set left-set)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-complement".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-complement ((set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet set))
  (the Inclusive-SOAPSet
    (make-inclusive-soapset
      (exclusive-soapset-blacklist set))))

;;; -------------------------------------------------------

(defmethod soapset-complement ((set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet set))
  (the Exclusive-SOAPSet
    (make-exclusive-soapset
      (inclusive-soapset-elements set))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-subset-p".                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-subset-p ((left-set  Exclusive-SOAPSet)
                             (right-set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the boolean
    (not (null
      (every
        #'(lambda (left-element)
            (declare (type integer left-element))
            (member left-element
              (exclusive-soapset-blacklist right-set)
              :test #'=))
        (exclusive-soapset-blacklist left-set))))))

;;; -------------------------------------------------------

(defmethod soapset-subset-p ((left-set  Exclusive-SOAPSet)
                             (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (ignore                 left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (declare (ignore                 right-set))
  (the boolean NIL))

;;; -------------------------------------------------------

(defmethod soapset-subset-p ((left-set  Inclusive-SOAPSet)
                             (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the boolean
    (the boolean
      (not (null
        (notany
          #'(lambda (left-element)
              (declare (type integer left-element))
              (member left-element
                (exclusive-soapset-blacklist right-set)
                :test #'=))
          (inclusive-soapset-elements left-set)))))))

;;; -------------------------------------------------------

(defmethod soapset-subset-p ((left-set  Inclusive-SOAPSet)
                             (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the boolean
    (the boolean
      (not (null
        (every
          #'(lambda (left-element)
              (declare (type integer left-element))
              (member left-element
                (inclusive-soapset-elements right-set)
                :test #'=))
          (inclusive-soapset-elements left-set)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-proper-subset-p".          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-proper-subset-p ((left-set  Exclusive-SOAPSet)
                                    (right-set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the boolean
    (not (null
      (and (soapset-subset-p left-set right-set)
           (< (length (exclusive-soapset-blacklist left-set))
              (length (exclusive-soapset-blacklist right-set))))))))

;;; -------------------------------------------------------

(defmethod soapset-proper-subset-p ((left-set  Exclusive-SOAPSet)
                                    (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (ignore                 left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (declare (ignore                 right-set))
  (the boolean NIL))

;;; -------------------------------------------------------

(defmethod soapset-proper-subset-p ((left-set  Inclusive-SOAPSet)
                                    (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the boolean
    (soapset-subset-p left-set right-set)))

;;; -------------------------------------------------------

(defmethod soapset-proper-subset-p ((left-set  Inclusive-SOAPSet)
                                    (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the boolean
    (not (null
      (and (soapset-subset-p left-set right-set)
           (< (length (inclusive-soapset-elements left-set))
              (length (inclusive-soapset-elements right-set))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-not-subset-p".             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-not-subset-p ((left-set  SOAPSet)
                                 (right-set SOAPSet))
  (declare (type SOAPSet left-set))
  (declare (type SOAPSet right-set))
  (the boolean
    (not (soapset-subset-p left-set right-set))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-superset-p".               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-superset-p ((left-set  SOAPSet)
                               (right-set SOAPSet))
  (declare (type SOAPSet left-set))
  (declare (type SOAPSet right-set))
  (the boolean
    (soapset-subset-p right-set left-set)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-proper-superset-p".        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-proper-superset-p ((left-set  SOAPSet)
                                      (right-set SOAPSet))
  (declare (type SOAPSet left-set))
  (declare (type SOAPSet right-set))
  (the boolean
    (soapset-proper-subset-p right-set left-set)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-not-superset-p".           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-not-superset-p ((left-set  SOAPSet)
                                   (right-set SOAPSet))
  (declare (type SOAPSet left-set))
  (declare (type SOAPSet right-set))
  (the boolean
    (soapset-not-subset-p right-set left-set)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of method "soapset-equal-p".                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod soapset-equal-p ((left-set  Exclusive-SOAPSet)
                            (right-set Exclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (the boolean
    (not (null
      (and (soapset-subset-p left-set right-set)
           (= (length (exclusive-soapset-blacklist left-set))
              (length (exclusive-soapset-blacklist right-set))))))))

;;; -------------------------------------------------------

(defmethod soapset-equal-p ((left-set  Exclusive-SOAPSet)
                            (right-set Inclusive-SOAPSet))
  (declare (type Exclusive-SOAPSet left-set))
  (declare (ignore                 left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (declare (ignore                 right-set))
  (the boolean NIL))

;;; -------------------------------------------------------

(defmethod soapset-equal-p ((left-set  Inclusive-SOAPSet)
                            (right-set Exclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (ignore                 left-set))
  (declare (type Exclusive-SOAPSet right-set))
  (declare (ignore                 right-set))
  (the boolean NIL))

;;; -------------------------------------------------------

(defmethod soapset-equal-p ((left-set  Inclusive-SOAPSet)
                            (right-set Inclusive-SOAPSet))
  (declare (type Inclusive-SOAPSet left-set))
  (declare (type Inclusive-SOAPSet right-set))
  (the boolean
    (not (null
      (and (soapset-subset-p left-set right-set)
           (= (length (inclusive-soapset-elements left-set))
              (length (inclusive-soapset-elements right-set))))))))
