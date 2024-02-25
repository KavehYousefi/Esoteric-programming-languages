;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file establishes the definitions of the types employed
;; throughout the project.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (or (eq element-type '*)
                         (typep element element-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the recognized airts for the
   movement inside of an elevator of the stair room."
  '(member :down :up))

;;; -------------------------------------------------------

(deftype non-negative-integer ()
  "The ``non-negative-integer'' type defines an integer number greater
   than or equal to zero, that is, a commorant in the range
   [0, +infinity]."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype non-positive-integer ()
  "The ``non-negative-integer'' type defines an integer number less than
   or equal to zero, that is, a commorant in the range [-infinity, 0]."
  '(integer * 0))

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integral number greater than
   zero, but bourneless along the upper axis, and thus a commorant of
   the interval [1, +infinity]."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (indicator-type '*)
                                        (value-type     '*))
  "The ``association-list-of'' type defines an association list, or
   alist, compact of zero or more entries, these being conses, where
   each indicator, or key, of which assumes the INDICATOR-TYPE and
   associates with a value of the VALUE-TYPE, for both holding the
   default specification of the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for element of-type T in (the list candidate)
              always
                (and (typep element 'cons)
                     (or (eq indicator-type '*)
                         (typep (car element) indicator-type))
                     (or (eq value-type '*)
                         (typep (cdr element) value-type)))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' enumerates the recognized variation on
   \"Thief, Police and the Building\" operations classes."
  '(member
    :enter-elevator
    :operate-elevator
    :enter-stair-room
    :climb-stair-room
    :leave
    :rob-room
    :encounter-police))

;;; -------------------------------------------------------

(deftype command-list ()
  "The ``command-list'' type defines a list of zero or more
   \"Thief, Police and the Building\" instructions, represented by a
   list of ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype transition-table ()
  "The ``transition-table'' type defines a mapping from commands to
   program states, realized as an association list, or alist, whose key
   assume ``command-type'' instances and map to ``Program-State''s."
  '(association-list-of command-type Program-State))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, its
   diorism incorporating siccan functions as ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))
