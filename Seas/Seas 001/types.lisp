;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file entails the bespoke type definitions, utible athwart the
;; project's entire gamut.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type utilizing the ``deftype'' infrastructure in
   conjunction with the ``satisfies'' type specifier, the agnomination
   of which issues from the TYPE-NAME, relying on the LAMBDA-LIST for
   its formal parameters, the probed object being nevened by the
   CANDIDATE-NAME, and evaluates the BODY forms, interpreting the
   predicate as satisfied if the desinent form returns a generalized
   Boolean value of a \"true\" nature, otherwise, if responding with
   ``NIL'', this docimasy denies its covenableness."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(if (stringp (first body))
          (pop body)
          (format NIL "Defines the type ~a." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-name)
               (declare (type T    ,candidate-name))
               (declare (ignorable ,candidate-name))
               ,@body))
         `(satisfies ,,predicate-name)))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which assumes the KEY-TYPE and answers to
   a value of the VALUE-TYPE, for both holds the default value of the
   generic sentinel ``*''."
  (and
    (hash-table-p candidate)
    (loop
      for key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value value)
      always
        (and (typep key   key-type)
             (typep value value-type)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, for
   which governs the generic sentinel of ``*'' the default's
   stipulation."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional Cartesian coordinates
   pair of integral resolution as either a complex number, the real part
   of which communicates the x-coordinate, while the imaginary moeity
   contributes the y-coordinate, or, imposed by the Common Lisp
   standard's mandatory coercion applied to a complex compound with a
   rational imaginary part of zero (0) to a scalar rational real part,
   a simple fixnum representative of the x-coordinate, the same tacitly
   carries the zero-valued y-coordinate.
   ---
   The various combinations of components and their causatum shall be
   explicated in a compendious tabular apercu:
     --------------------------------------------------------
     Input combination | Actual representation in Common Lisp
     ------------------+-------------------------------------
     (complex x y)     | Complex number of (x, y).
     ........................................................
     (complex x 0)     | Fixum   number of  x.
     ........................................................
     (complex 0 y)     | Complex number of (x, y).
     ........................................................
     (complex 0 0)     | Fixnum  number of 0.
     --------------------------------------------------------"
  '(or (complex fixnum) fixnum))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized variations on airts
   along thilk the instruction pointer (IP), or, in a diction endowed
   with metaphorical puissance, the \"submarine\", in a Seas program
   may engage in its perambulation."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype icon-matrix ()
  "The ``icon-matrix'' type defines a sparse two-dimensional array of
   program cells, realized as a hash table whose keys are supplied by
   ``location'' objects, and whose values kithe in the form of ``Icon''
   objects."
  '(hash-table-of location Icon))

;;; -------------------------------------------------------

(deftype code-point ()
  "The ``code-point'' type defines a Unicode code point as an integral
   objects whose inclusive lower mear is empight at the value zero (0),
   but whose mickleness along the positive axis does not thole any
   imposition."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype protocol-measure ()
  "The ``protocal-measure'' type defines a function responsible for an
   ``Icon'' object's evaluation in a ``Travel-Log'''s context, realized
   as a functional object of a dyadic nature, the ``Icon'' constituting
   its first argument, the ``Travel-Log'' the second moeity, and which
   produces no value."
  '(function (Icon Travel-Log) (values)))

;;; -------------------------------------------------------

(deftype action-protocol ()
  "The ``action-protocol'' type defines a registry whose telos wones in
   the association of Seas instruction identifiers with the connable
   actions, its plasmature chosen as an association list (alist) thilk
   alligates ``Icon'' objects, representative of instruction names, with
   ``protocol-measure''s, dyadic functions which, for an ``Icon'' and a
   ``Travel-Log'', the latter accoutres the execution context, exercise
   the nait epiphenomena, while returning no value."
  '(list-of (cons Icon protocol-measure)))
