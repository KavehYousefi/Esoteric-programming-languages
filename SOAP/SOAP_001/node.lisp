;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This class comprehends the definition of "Node" class and its
;; appertaining operations, serving to represent an abstract syntax tree
;; (AST) node.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Node".                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Node
  (:constructor initialize-node (type)))
  "The ``Node'' class encapsulates the requisite information for
   representing an abstract syntax tree (AST) node.
   ---
   In lieu of subclassing, the different language aspects express their
   diorisms by adminiculum of a type property, operating in champarty
   with a hash table of attribute name-value twains where additional
   specifications impose such."
  (type       (error "Missing node type.") :type keyword)
  (attributes (make-hash-table :test #'eq) :type attribute-map))

;;; -------------------------------------------------------

(defun make-node (type &rest initial-attributes)
  "Creates and returns a new ``Node'' subsumed into the TYPE, and
   optionally endowed with the INITIAL-ATTRIBUTES, provided as a
   property list (plist) of zero or more attribute name-value pairs."
  (declare (type keyword        type))
  (declare (type attribute-list initial-attributes))
  (let ((node (initialize-node type)))
    (declare (type Node node))
    (loop
      for (attribute-name attribute-value)
        of-type (keyword T)
        on      initial-attributes
        by      #'cddr
      do
        (setf (gethash attribute-name (node-attributes node))
              attribute-value))
    (the Node node)))

;;; -------------------------------------------------------

(defun node-attribute (node attribute-name)
  "Returns the value of the attribute registered with the ATTRIBUTE-NAME
   at the NODE, or signals an error of an unspecified type upon the
   designator's absence."
  (declare (type Node node))
  (declare (type keyword attribute-name))
  (multiple-value-bind (attribute-value contains-name-p)
      (gethash attribute-name (node-attributes node))
    (declare (type T attribute-value))
    (declare (type T contains-name-p))
    (the T
      (if contains-name-p
        attribute-value
        (error "Unrecognized attribute name: ~s." attribute-name)))))

;;; -------------------------------------------------------

(defmethod print-object ((node Node) stream)
  (declare (type Node        node))
  (declare (type destination stream))
  (format stream "(NODE :TYPE ~s :ATTRIBUTES ["
    (node-type node))
  (loop
    for attribute-name
      of-type keyword
      being the hash-keys in (node-attributes node)
    using
      (hash-value attribute-value)
    for first-attribute-p
      of-type boolean
      =       T
      then    NIL
    do
      (unless first-attribute-p
        (format stream ", "))
      (format stream "~s => ~s" attribute-name attribute-value))
  (format stream "])"))
