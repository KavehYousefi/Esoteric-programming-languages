;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file accommodates the operations and diorisms appertaining to
;; the commonly employed types in the V3i interpreter implementation
;; process.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-custom-type
    (type-name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type stevened by the TYPE-NAME, its formal
   parameters constituting a verbatim appropriation from the
   LAMBDA-LIST, the docimasy's cynosure being agnominated by the
   CANDIDATE-NAME, executes the BODY forms, granted access to both the
   LAMBDA-LIST and the CANDIDATE-NAME, and construes the desinent form's
   primary return value as an adjudgment of the probed entity's
   eligibility, imputing for a \"generalized boolean\" value of \"true\"
   a compatible nature, otherwise, for a \"false\" response, its lack
   of appropriateness.
   ---
   The first BODY form, upon its resolution to a string object, will be
   attended to the assumption of its role as a documentation string to
   the derived type, resulting in the appropriation for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name ,lambda-list
       ,(or (and (stringp (first body))
                 (pop body))
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

(define-custom-type hash-table-of
    (candidate &optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table composed of zero or
   more entries, each key of which conforms to the KEY-TYPE and answers
   to a value of the VALUE-TYPE, for both holds the default of ``T''."
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

(define-custom-type list-of (candidate &optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE, the same
   defaults to the comprehensive ``T''."
  (and
    (listp candidate)
    (loop
      for    element of-type T in (the list candidate)
      always (typep element element-type))))

;;; -------------------------------------------------------

(deftype associativity ()
  "The ``associativity'' type enumerates the recognized configurations
   of the associativity, which governs the triage to be imputed betwixt
   a twissel of operators, entalented with equipollence in their
   ligation's fortitude, when engaged in the agon for an operand's
   obtention."
  '(member :left :right :none))

;;; -------------------------------------------------------

(deftype nud-transformer ()
  "The ``nud-transformer'' type defines a callback function responsible
   for the transformation of a nud token into an abstract syntax tree
   (AST) node, accepting the token in question and a token stream for
   contingent further tokens' obention in order to deliver the
   desideratum.
   ---
   In this agency, the function ought to comply with the signature:
     lambda (Token Token-Stream) => AST-Node"
  '(function (Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype led-transformer ()
  "The ``led-transformer'' type defines a callback function responsible
   for the transformation of a led token into an abstract syntax tree
   (AST) node, accepting the token in question, a token stream for
   contingent further tokens' obention, and the already present
   prevenient expression, itself an AST node, in order to deliver the
   desideratum.
   ---
   In this agency, the function ought to comply with the signature:
     lambda (Token Token-Stream AST-Node) => AST-Node"
  '(function (Token Token-Stream AST-Node) AST-Node))

;;; -------------------------------------------------------

(deftype std-transformer ()
  "The ``std-transformer'' type defines a callback function responsible
   for the transformation of a std token into an abstract syntax tree
   (AST) node, accepting the token in question and a token stream for
   contingent further tokens' obention in order to deliver the
   desideratum.
   ---
   In this agency, the function ought to comply with the signature:
     lambda (Token Token-Stream) => AST-Node"
  '(function (Token Token-Stream) AST-Node))

;;; -------------------------------------------------------

(deftype node-list ()
  "The ``node-list'' type defines an ordered list composed of zero or
   more abstract syntax tree (AST) nodes."
  '(list-of AST-Node))

;;; -------------------------------------------------------

(deftype unary-operator ()
  "The ``unary-operator'' type enumerates the recognized binary
   operators."
  '(member :plus :minus))

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
    :equal-to
    :less-than
    :greater-than))

;;; -------------------------------------------------------

(deftype unsigned-integer ()
  "The ``unsigned-integer'' type defines a non-negative integer number
   greater than or equal to zero (0), but without a natural march's
   imposition along the positive axis."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype variable-name ()
  "The ``variable-name'' type defines a variable identifier as a
   singleton string."
  '(simple-string 1))

;;; -------------------------------------------------------

(deftype association-list-of (&optional (key-type T) (value-type T))
  "The ``association-list-of'' type defines an association list, or
   alist, composed of zero or more entries, each such embracing in its
   diorism a cons cell whose first element, the key, complies with the
   KEY-TYPE, and whose second element, imposing the value, admits the
   VALUE-TYPE, for both holding the comprehensive default of ``T''."
  `(list-of (cons ,key-type ,value-type)))

;;; -------------------------------------------------------

(deftype variable-set-entries ()
  "The ``variable-set-entries'' type defines unilateral association
   betwixt a variable's agnomination and its representative
   encapsulation, its manifestation chosen as an association list
   (alist), the keys of which bear ``variable-name''s and respond with
   ``V3iVariable'' constructs."
  '(association-list-of variable-name V3iVariable))

;;; -------------------------------------------------------

(deftype v3i-object ()
  "The ``v3i-object'' type defines a datum conable to be admitted to a
   participation in a V3i program."
  '(or integer string))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations, the
   circumference of which includes, among others, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))
