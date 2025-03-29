;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the type operations and definitions, their
;; character as an annotational warklume propagating through the entire
;; project's infrastructure.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-derived-type
    (type-name (candidate-variable &rest lambda-list)
     &body body)
  "Defines a derived type norned by the TYPE-NAME, its formal parameters
   being appropriated ipsissima verba from the LAMBDA-LIST, and which
   probes the quesited object by the agnomination provided through the
   CANDIDATE-VARIABLE symbol, evaluating the BODY forms, and construing
   the desinent form's primary return value as a
   \"generalized boolean\", siccan ought to respond with a \"true\"
   sentinel for the candidate's eligiblity, otherwise with \"false\".
   ---
   The first BODY form, upon its establishment of a string object, is
   subjected to an interpretation as the derived type's documentation
   string, and, as a corollary, reappropriated for this sole purpose."
  (let ((predicate-variable (gensym)))
    (declare (type symbol predicate-variable))
    `(deftype ,type-name (,@lambda-list)
       ,(or (and (stringp (first body))
                 (pop body))
            "")
       (let ((,predicate-variable (gensym)))
         (declare (type symbol ,predicate-variable))
         (setf (symbol-function ,predicate-variable)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-variable)))))

;;; -------------------------------------------------------

(defun accepts-any-object-p (object)
  "Determines whether the OBJECT represents the generic type sentinel
   ``*'', the same homologates any object's participation, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null
      (and (symbolp object)
           (eq      object '*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the generic sentinel
   ``*''."
  (and
    (listp candidate)
    (or
      (accepts-any-object-p element-type)
      (loop
        for    element of-type T in (the list candidate)
        always (typep element element-type)))))

;;; -------------------------------------------------------

(define-derived-type hash-table-of (candidate
                                    &optional (key-type   '*)
                                              (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, the keys of which comply to the KEY-TYPE, the values to
   the VALUE-TYPE, both defaulting to the generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and
          (or (accepts-any-object-p key)
              (typep                key key-type))
          (or (accepts-any-object-p value)
              (typep                value value-type))))))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list compact of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype address-mode ()
  "The ``address-mode'' type enumerates the admissive variations on a
   memory access request's "
  '(member :immediate :referential))

;;; -------------------------------------------------------

(deftype program ()
  "The ``program'' type defines an executable ``` program as a
   one-dimensional simple array composed of zero or more ``Instruction''
   objects."
  '(simple-array Instruction (*)))

;;; -------------------------------------------------------

(deftype cell-vector ()
  "The ``cell-vector'' type defines a sparse vector of integer-valued
   cells, amenable to signed integer indices, and realized by
   adminiculum of a hash table, the keys of which correspond to the
   subscripts, while the values answer to the cell values."
  '(hash-table-of integer integer))

;;; -------------------------------------------------------

(deftype io-mode ()
  "The ``io-mode'' type enumerates the recognized input/output modes."
  '(member :none :input :output))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   diorism's amplectation enumerates, without an exhaustion's
   infliction, the functions ``format'' and ``write-char''."
  '(or null (eql T) stream string))
