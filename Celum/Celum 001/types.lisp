;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This files serves in the declaration of the globally significant
;; types.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   exemplary of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which assumes the KEY-TYPE, affiliated with a
   value of the VALUE-TYPE, both to default to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (hash-table-p object)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table object)
              using
                (hash-value value)
              always
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype command-list ()
  "The ``command-list'' type defines a list composed of zero or more
   ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte composed of eight bits."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype automaton-rule ()
  "The ``automaton-rule'' type defines an elementary cellular automaton
   rule by its Wolfram code, an integral value in the closed interval
   [0, 255], the 8-bit binary representation of which specifies the
   unique bit pattern commorant in any specimen from this enumerated
   set."
  '(integer 0 255))

;;; -------------------------------------------------------

(deftype cell-neighborhood ()
  "The ``cell-neighborhood'' type defines a bit key, a triad of
   consecutive bits which encode the index of a bit in an automaton
   rule's binary pattern, and thus elicit an output bit response from
   the same."
  '(unsigned-byte 3))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   species of which comprehends, among others, the functions ``format''
   and ``write-char''."
  '(or null (eql T) stream string))
