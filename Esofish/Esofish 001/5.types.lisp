;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file's compass is ordained to amplect the implementation of
;; operations concerning the handling of types, concomitant to their
;; definitions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :5-programming-language)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-derived-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type with an agnomination desumed from the
   TYPE-NAME, compernage to the formal parameters' ipsissima verba
   appropriation from the LAMBDA-LIST, clepes the object of the docimasy
   via the CANDIDATE-NAME, evaluates the BODY forms, and construes the
   desinent form's primary value as the assessment's conclusion, where
   a \"generalized boolean\" value of \"true\" corresponds to the type
   predicate's satisfaction by the candidate, and \"false\" as the
   incompatibility's signification.
   ---
   If the first BODY form resolves to a string, its purpose is assumed
   to appertain to a documentation string for the derived type; as a
   consectary, the element's reappropration for this cheason is
   actuated."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   perimeter of which amplects, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(define-a-derived-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, everichon among these subsumes into the ELEMENT-TYPE, for
   thilk is imposed the generic sentinel ``*'' as the default."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(define-a-derived-type tuple-of (candidate &rest element-types)
  "The ``tuple-of'' type defines a list-based tuple of elements, the
   members of which subsume, in the specified order, into the imposed
   ELEMENT-TYPES.
   ---
   In a concrete diction, given
     T = (t_1, t_2, ..., t_N) as the ordered list of the ELEMENT-TYPES
     E = (e_1, e_2, ..., e_M) as the candidate, assumed to be a list,
   it must hold:
     M = N
     and
     e_i is of the type t_i, for each i in 1 to N."
  (and
    (listp candidate)
    (= (length (the list candidate))
       (length element-types))
    (loop
      for    current-element of-type T in (the list candidate)
      for    expected-type   of-type T in element-types
      always (typep current-element expected-type))))

;;; -------------------------------------------------------

(deftype method-specializer (&optional (specific-object  '*)
                                       (alternative-type '*))
  "The ``method-specializer'' type defines a dispatching mechanism for
   a formal argument in a  ``defmethod'' construct in a twifaced mode;
   imprimis, as an ``eql''-specialization on the SPECIFIC-OBJECT; and,
   secondly, parhedral in its logical consideration, as the
   ALTERNATIVE-TYPE."
  `(or (tuple-of (eql eql) ,specific-object)
       (eql ,alternative-type)))

;;; -------------------------------------------------------

(define-a-derived-type hash-table-of
    (candidate
     &optional (key-type   '*)
               (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose constitution
   is founded upon zero or more entries, each key of which subsumes into
   the KEY-TYPE and associates with a value compliant with the
   VALUE-TYPE, both governed by the default configuration of the generic
   sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(deftype 5-mode ()
  "The ``5-mode'' type enumerates the recognized variations on the
   \"5\" programming language, also nevened their \"modes\"."
  '(member 5 15 35))
