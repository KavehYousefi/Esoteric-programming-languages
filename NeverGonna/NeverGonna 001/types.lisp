
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more elements
   of the ELEMENT-TYPE, the same defaults to the generic ``*''
   sentinel."
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

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, defaulting to the generic
   ``*'' sentinel."
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
                (and (typep key   key-type)
                     (typep value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype parser-processor ()
  "The ``parser-processor'' type defines a function utible for the
   evaluation of a ``Parse-State'' in order to respond with a
   ``Parse-Result'', and thus conformant to the signature
     lambda (Parse-State) => Parse-Result"
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser'' objects."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype binary-operator ()
  "The ``binary-operator'' type enumerates the recognized binary
   operators."
  '(member
    :plus
    :minus
    :times
    :divided
    :remainder
    :power
    :equal
    :not-equal
    :greater-or-equal
    :greater-than
    :less-or-equal
    :less-than))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized unary
   operators."
  '(member :plus :minus :logical-not))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines a list of zero or more ``Node''
   objects."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype led-processor ()
  "The ``led-processor'' type defines a functional unit responsible for
   the transformation of led token and an appertaining left expression
   into a new production, realized in terms of a function that adheres
   to the signature
     lambda (led-token left-expression) => led-result"
  '(function (Token AST-Node) Parser))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the possible operator
   associativity policies that govern the precedence among operators of
   paregal binding power."
  '(member :none :left :right))

;;; -------------------------------------------------------

(deftype ngobject-type ()
  "The ``ngobject-type'' enumerates the valid species of objects
   admitted to participate in a NeverGonna program."
  '(member :boolean :integer :string))

;;; -------------------------------------------------------

(deftype variable-map ()
  "The ``variable-map'' type defines a mapping of variable names to
   their representative variable objects, realized as a hash table that
   associates string identifiers to ``NGVariable'' encapsulations."
  '(hash-table-of string NGVariable))
