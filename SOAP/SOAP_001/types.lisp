;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the declaration of the globally significant
;; types.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   member of which conforms to the ELEMENT-TYPE, defaulting to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to the comprehensive
   ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value value)
              always
                (and (typep key key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype attribute-map ()
  "The ``attribute-map'' type defines a collection of node attributes in
   the form of a hash table mapping which associated keyword symbol
   attribute names to arbitrary values."
  '(hash-table-of keyword T))

;;; -------------------------------------------------------

(deftype attribute-list ()
  "The ``attribute-list'' type defines a list of node attributes in
   terms of a property list, or plist, with each attribute name (key or
   indicator) immediately succeeded by its associated attribute value
   (property value), the former of which must be a keyword symbol,
   whereas the latter may assume the generic type ``T''."
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (evenp (length (the list candidate)))
            (loop
              for (indicator value)
                of-type (T T)
                on      (the list candidate)
                by      #'cddr
              always
                (and (typep indicator 'keyword)
                     (typep value     T))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of Node))

;;; -------------------------------------------------------

(deftype set-operator ()
  "The ``set-operator'' type enumerates the recognized binary set
   operations."
  '(member
    :union
    :intersection
    :left-difference
    :right-difference))

;;; -------------------------------------------------------

(deftype set-relationship ()
  "The ``set-relationship'' type enumerates the recognized relationship
   betwixt two sets, most commonly employed in the indagation of a
   loop's continuation predicate."
  '(member
    :subset
    :proper-subset
    :not-subset
    :superset
    :proper-superset
    :not-superset
    :equal))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   enumerating, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype natural-number ()
  "The ``natural-number'' type defines a positive integer with no upper
   bourne, that is, a commorant of the range [1, +infinity], most
   commonly employed in the context of set members."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype number-list ()
  "The ``number-list'' type defines a list of zero or more natural
   numbers, that is, positive integers."
  '(list-of natural-number))
