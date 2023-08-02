;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "TCC", invented by the Esolang user "A" and presented on 25
;; October, 2018, the design and concept of which is desumed from the
;; database query language "SQL", pursuing the definition of tables and
;; their columns' printing.
;; 
;; 
;; Concept
;; =======
;; TCC applies itself to the creation of tables and their columns'
;; printing, akin to the database query language SQL.
;; 
;; == TCC: [T]ABLE [C]HANGING [C]ODE ==
;; The agnomination TCC alludes in its extended form, "Table Changing
;; Code", to the language's wike of creating and manipulating tables
;; during the prosecution of its telos.
;; 
;; == TABLES OF INTEGERS AND STRING ==
;; A throughout parvipotent dation assesses TCC's competences, as the
;; language's mimicry of the database query language "Structured Query
;; Language" (SQL) serves to adhibit merely the castaldy of tables.
;; 
;; An arbitrary tally of such two-dimensional entities finds its adit
;; in a program, identified and distinguished by a name's adminiculum,
;; and capable of being created, but escaping an expulsion mechanism.
;; 
;; Each table's diorism embraces any tally of rows and columns, the
;; ensuing cells accommodating an aefauld signed integer or string's
;; salvatory.
;; 
;; Both columns and rows exhibit an amenability to positive integer
;; indices, commencing with the location one (1).
;; 
;; 
;; Architecture
;; ============
;; The architecture of TCC conflates with its paravant object of
;; haecceity, the table comprehending signed integers and strings as
;; the tokens of currency in a cell.
;; 
;; All tables are registered with a unique name at the system, acting as
;; a handle for references, and initially amounting to a blank state.
;; The rows and columns are amenable to positive integer indices,
;; commencing with the first identifier one (1).
;; 
;; Rows may be inserted by mediation of their values, as well as be
;; subjected to deletion by their indices.
;; 
;; Columns, also answering to indices, are entalented with the capacity
;; of their printing only, lacking a removal mechanism.
;; 
;; 
;; Data Types
;; ==========
;; TCC's type system bifurcates into a twain of members: signed integer
;; numbers of any magnitude, and strings of an arbitrary extent.
;; 
;; 
;; Syntax
;; ======
;; A TCC program contains a composition of zero or more statements, each
;; such introduced by a unique identifier and alternating constituents
;; representing arguments and keywords. Both the command names and
;; keywords are case-insensitve.
;; 
;; == GRAMMAR ==
;; A formulation of TCC's syntaxis in the Extended Backus-Naur Form
;; (EBNF) shall be adduced:
;; 
;;   program       := optSpaces
;;                 ,  [ command , { spaces , command } ]
;;                 ,  optSpaces
;;                 ;
;;   
;;   command       := create | in | insert | delete ;
;;   create        := "CREATE" , spaces , tableName ;
;;   in            := "IN"     , spaces , tableName , spaces
;;                 ,  "CHOOSE" , spaces , index ;
;;   insert        := "INSERT" , spaces , tableName , spaces
;;                 ,  "VALUES" , spaces , cellValueList
;;                 ;
;;   delete        := "DELETE" , spaces , tableName , spaces
;;                 ,  "WHERE" , spaces , index
;;                 ;
;;   
;;   tableName     := letter , { letter | digit | "_" } ;
;;   index         := digit , { digit } ;
;;   cellValueList := "(" , optSpaces
;;                 ,   [ cellValue , cellValueRest ]
;;                 ,   optSpaces , ")"
;;                 ;
;;   cellValueRest := { optSpaces , "," , optSpaces , cellValue } ;
;;   cellValue     := integer | string ;
;;   string        := quote , { character - quote } , quote ;
;;   quote         := '"' ;
;;   integer       := [ "+" | "-" ] , digit , { digit } ;
;;   optSpaces     := { space } ;
;;   spaces        := space , { space } ;
;;   space         := " " | "\t " ;
;; 
;; 
;; Instructions
;; ============
;; The language's instruction set tallies four members in its mimicry
;; of database handling, the competences of which amplect the creation
;; of tables, insertion and deletions of rows, and the printing of
;; columns.
;; 
;; == OVERVIEW ==
;; The quadruple instruction set of TCC's shall now be subjected to a
;; cursory introduction, ere more detailed expositions follow alow.
;; 
;;   ------------------------------------------------------------------
;;   Command keyword | Effect
;;   ----------------+-------------------------------------------------
;;   CREATE          | Creates a new table.
;;                   | -> See TABLE CREATION.
;;   ..................................................................
;;   IN              | Prints a column from a table.
;;                   | -> See COLUMN SELECTION.
;;   ..................................................................
;;   INSERT          | Inserts a row (record) into a table.
;;                   | -> See ROW INSERTION.
;;   ..................................................................
;;   DELETE          | Deletes a row (record) from a table.
;;                   | -> See ROW DELETION.
;;   ------------------------------------------------------------------
;; 
;; == COMMAND LISTING ==
;; An enumeration of the commands mentioned in the parasceve, endowed
;; with enhanced nimiety in their specification, shall now be adduced.
;; 
;; Please note the following points:
;; 
;;   (1) Placeholder sections are underlined with asterisks ("*"), the
;;       same must be substituted by actual code in the program.
;;   (2) All command names are case-insensitive, where as the
;;       discrepancy betwixt minuscles and majuscles acquires
;;       significance for all other entities.
;; 
;; The commands are thus established:
;; 
;;      ________________
;;    _/ TABLE CREATION \______________________________________________
;;   |-----------------------------------------------------------------
;;   | Command | CREATE table_name
;;   |         |        **********
;;   |---------+-------------------------------------------------------
;;   | Effect  | Creates and registers a new empty table denoted by the
;;   |         | {table_name} if the same does not exist yet. Otherwise
;;   |         | no effect transpires.
;;   |         |-------------------------------------------------------
;;   |         | The {table_name} must be a valid table identifier.
;;    -----------------------------------------------------------------
;;   
;;      __________________
;;    _/ COLUMN SELECTION \____________________________________________
;;   |-----------------------------------------------------------------
;;   | Command | IN table_name CHOOSE column_index
;;   |         |    **********        ************
;;   |---------+-------------------------------------------------------
;;   | Effect  | Prints to the standard output the values in the
;;   |         | column designated by the one-based {column_index} in
;;   |         | the table specified by the {table_name}.
;;   |         |-------------------------------------------------------
;;   |         | The {table_name} must be a valid table identifier.
;;   |         |-------------------------------------------------------
;;   |         | The {column_index} must be a positive integer number.
;;    -----------------------------------------------------------------
;;   
;;      _______________
;;    _/ ROW INSERTION \_______________________________________________
;;   |-----------------------------------------------------------------
;;   | Command | INSERT table_name VALUES (value_1, ..., value_N)
;;   |         |        **********         *******  ***  *******
;;   |---------+-------------------------------------------------------
;;   | Effect  | Inserts a new row (record) composed of the values
;;   |         | {value_1} through {value_N} into the table designated
;;   |         | by the {table_name}.
;;   |         |-------------------------------------------------------
;;   |         | The {table_name} must be a valid table identifier.
;;   |         |-------------------------------------------------------
;;   |         | The values {value_1} through {value_N} must be a
;;   |         | comma-separated sequence of zero or more signed
;;   |         | integer numbers or strings of arbitrary length.
;;   |         |-------------------------------------------------------
;;   |         | An error of the type "InvalidTableNameError" is
;;   |         | signaled if no table with the {table_name} exists.
;;    -----------------------------------------------------------------
;;   
;;      ______________
;;    _/ ROW DELETION \________________________________________________
;;   |-----------------------------------------------------------------
;;   | Command | DELETE table_name WHERE row_index
;;   |         |        **********       *********
;;   |---------+-------------------------------------------------------
;;   | Effect  | Deletes from the table designated by the {table_name}
;;   |         | the row at the one-based index {row_index}.
;;   |         |-------------------------------------------------------
;;   |         | The {table_name} must be a valid table identifier.
;;   |         |-------------------------------------------------------
;;   |         | The {row_index} must be a positive integer number.
;;   |         |-------------------------------------------------------
;;   |         | An error of the type "InvalidTableNameError" is
;;   |         | signaled if no table with the {table_name} exists.
;;   |         |-------------------------------------------------------
;;   |         | No causatum transpires if the {row_index} designates
;;   |         | an invalid location inside of a table.
;;    -----------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; The curtailed nature of TCC's protolog admits the inroad of certain
;; ambiguities, a subset of which shall be the coming sections'
;; material:
;; 
;; == WHICH DATA TYPES ARE HOMOLOGATED? ==
;; The specification relates of data castaldy, but is peccant of a
;; lapsus concerning their actual types' diorism.
;; 
;; It has been adjudged to permit signed integers and strings of any
;; length, as these elements, extrapolated from experiences with other
;; esoteric programming languages, occupy a paravant, and frequently
;; exclusive, echolon.
;; 
;; 
;; Implementation
;; ==============
;; This simple implementation has been realized in the programming
;; language Common Lisp, conflating the lexing and parsing stages, but
;; maintaining a distinct interpreter step.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-08-03
;; 
;; Sources:
;;   [esolang2022TCC]
;;   The Esolang contributors, "TCC", 21 May, 2022
;;   URL: "https://esolangs.org/wiki/TCC"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list composed of zero or more elments
   of the ELEMENT-TYPE, the same defaults to the comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (loop
              for    element of-type T in (the list candidate)
              always (typep element element-type)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype cell-value ()
  "The ``cell-value'' type defines the types of objects admissible to
   an amplectation in the program memory."
  '(or null integer string))

;;; -------------------------------------------------------

(deftype value-list ()
  "The ``value-list'' type defines a list composed of zero or more
   ``cell-value'' objects."
  '(list-of cell-value))

;;; -------------------------------------------------------

(deftype command-type ()
  "The ``command-type'' enumerates the recognized vairants of commands."
  '(member :create :in :insert :delete))

;;; -------------------------------------------------------

(deftype character-test ()
  "The ``character-test'' type defines a dyadic function which accepts
   two characters and determines their equality.
   ---
   The function must accept a twain of characters to match against each
   other, responding with a generalized Boolean that, if assuming a
   non-``NIL'' value, avers their equality; a response of ``NIL''
   denotes divergence. The signature, as a corollary, is tantamount to:
     lambda (character character) => generalized-boolean"
  '(function (character character) *))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   comprehensive ``T''."
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

(deftype tcc-program ()
  "The ``tcc-program'' type defines an executable TCC program as a list
   composed of zero or more ``Command'' objects."
  '(list-of Command))

;;; -------------------------------------------------------

(deftype index ()
  "The ``index'' type defines a positive integer nait for the
   designation of a column or row index."
  '(integer 1 *))

;;; -------------------------------------------------------

(deftype row ()
  "The ``row'' type defines a table row as a one-dimensional simple
   array of zero or more ``cell-value'' instances."
  '(simple-array cell-value (*)))

;;; -------------------------------------------------------

(deftype row-list ()
  "The ``row-list'' type defines a vector of zero or more ``row''s."
  '(vector row *))

;;; -------------------------------------------------------

(deftype database ()
  "The ``database'' type defines a registry which associates table names
   with actual table objects, manifesting as a hash table whose string
   keys represents the names, mapping to ``Table'' instances."
  '(hash-table-of string Table))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   encompassing, among others, the functions ``format'' and
   ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of command.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:constructor make-command (type &rest arguments)))
  "The ``Command'' class specifies a TCC command, the diorism of the
   same empights the imperative operation type beside a list of zero or
   more optional arguments."
  (type      (error "Missing command type.") :type command-type)
  (arguments NIL                             :type (list-of T)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace, returning
   on confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=)))))

;;; -------------------------------------------------------

(defun command-character-p (candidate)
  "Determines whether the CANDIDATE represents a command identifier
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (alpha-char-p candidate)))))

;;; -------------------------------------------------------

(defun table-name-start-character-p (candidate)
  "Determines whether the CANDIDATE represents a table name constituent
   eligible for introducing such an identifier, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alpha-char-p candidate)
          (char= candidate #\_))))))

;;; -------------------------------------------------------

(defun table-name-character-p (candidate)
  "Determines whether the CANDIDATE represents a table name constituent,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (not (null
      (or (alphanumericp candidate)
          (char= candidate #\_))))))

;;; -------------------------------------------------------

(defun get-command (identifier)
  "Returns the command type corresponding to the case-insensitive
   IDENTIFIER, or signals an error of an unspecified type upon its
   disrespondency."
  (declare (type string identifier))
  (the command-type
    (cond
      ((string-equal identifier "CREATE") :create)
      ((string-equal identifier "IN")     :in)
      ((string-equal identifier "INSERT") :insert)
      ((string-equal identifier "DELETE") :delete)
      (T (error "Invalid command identifier: ~s." identifier)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of lexer.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type string              *source*))
(declaim (type fixnum              *current-position*))
(declaim (type (or null character) *current-character*))

;;; -------------------------------------------------------

(defparameter *source* ""
  "The piece of TCC code to analyze.")

(defparameter *current-position* 0
  "The current position into the *SOURCE*.")

(defparameter *current-character* NIL
  "The character at the *CURRENT-POSITION* into the *SOURCE*.")

;;; -------------------------------------------------------

(defun advance ()
  "Returns the *CURRENT-CHARACTER*, while concomitantly advancing the
   *CURRENT-POSITION* cursor to the next location in the *SOURCE*,
   updating in the course the *CURRENT-CHARACTER*."
  (the (or null character)
    (prog1 *current-character*
      (setf *current-character*
        (when (array-in-bounds-p *source* (1+ *current-position*))
          (char *source*
            (incf *current-position*)))))))

;;; -------------------------------------------------------

(defun eof-p ()
  "Determines whether the *CURRENT-POSITION* has transcended the
   *SOURCE*'s boundaries, thus signifying the end of the file (EOF)."
  (the boolean
    (null *current-character*)))

;;; -------------------------------------------------------

(defun on-match-p (predicate)
  "Determines whether the *CURRENT-CHARACTER* constitutes a ``NIL''
   value which satisfies the PREDICATE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''.
   ---
   The PREDICATE must be supplied in the form of a function which
   accepts as its aefauld argument a character which always amounts to
   the *CURRENT-CHARACTER*, and returns a generalized Boolean result to
   signify the eligibility of the probed input. Its signature, in
   corollary, must conform to:
     function (character) => generalized-boolean"
  (declare (type (function (character) *) predicate))
  (the boolean
    (not (null
      (and *current-character*
           (funcall predicate *current-character*))))))

;;; -------------------------------------------------------

(defun on-character-p (expected-character)
  "Determines whether the *CURRENT-CHARACTER* equals the
   EXPECTED-CHARACTER, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character expected-character))
  (the boolean
    (and *current-character*
         (char= *current-character* expected-character))))

;;; -------------------------------------------------------

(defun on-digit-p ()
  "Determines whether the *CURRENT-CHARACTER* assumes a decimal digit,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (on-match-p #'digit-char-p)))

;;; -------------------------------------------------------

(defun on-sign-p ()
  "Determines whether the *CURRENT-CHARACTER* constitutes a mathematical
   sign, that is, either plus (\"+\") or minus (\"-\"), returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (the boolean
    (on-match-p
      #'(lambda (character)
          (declare (type character character))
          (find character "+-" :test #'char=)))))

;;; -------------------------------------------------------

(defun on-command-identifier-p ()
  "Determines whether the *CURRENT-CHARACTER* assumes a command
   identifier constituent, returning on confirmation a ``boolean'' value
   of ``T'', otherwise ``NIL''."
  (the boolean
    (on-match-p #'alpha-char-p)))

;;; -------------------------------------------------------

(defun on-table-name-p ()
  "Determines whether the *CURRENT-CHARACTER* assumes a table name
   constituent, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (on-match-p #'table-name-character-p)))

;;; -------------------------------------------------------

(defun on-whitespace-p ()
  "Determines whether the *CURRENT-CHARACTER* assumes a whitespace,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (on-match-p #'whitespace-character-p)))

;;; -------------------------------------------------------

(defun skip-whitespaces ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, skips a
   sequence of zero or more accolent whitespaces, and returns no value."
  (loop while (on-whitespace-p) do
    (advance))
  (values))

;;; -------------------------------------------------------

(defun expect-match (predicate
                     &optional (expected-content ""))
  "Determines whether the *CURRENT-CHARACTER* satisfies the PREDICATE,
   on confirmation returning no value; otherwise an error of an
   unspecified type, optionally accompanied with the EXPECTED-CONTENT as
   a hint to the offence's etiology, is signaled.
   ---
   The PREDICATE must be a function of the arity one, the aefauld
   argument of which constitutes a character, the *CURRENT-CHARACTER*,
   to be probed, responding with a generalized Boolean that responds
   with a non-``NIL'' value for the input's satisfaction, otherwise with
   ``NIL''. The signature, hence, conforms to:
     lambda (character) => generalized-boolean
   ---
   In the EXPECTED-CONTENT an arbitrary object may be contributed which,
   upon the PREDICATE's failure, is embedded in an aesthetically form
   into the elicited error message; the expectency governs that this
   additional piece of information describes the desired, but absent,
   character from the *SOURCE*."
  (declare (type (function (character) *) predicate))
  (declare (type T                        expected-content))
  (cond
    ((eof-p)
      (error "Expected ~a at position ~d, but encountered EOF."
        expected-content *current-position*))
    ((not (funcall predicate *current-character*))
      (error "Expected ~a at position ~d, ~
              but encountered \"~c\"."
        expected-content *current-position* *current-character*))
    (T
      NIL))
  (values))

;;; -------------------------------------------------------

(defun expect-whitespace ()
  "Determines whether the *CURRENT-CHARACTER* represents a whitespace,
   on confirmation skipping all whitespaces accolent to the current one,
   and returning no value; otherwise an error of an unspecified type is
   signaled."
  (expect-match #'whitespace-character-p "a whitespace")
  (skip-whitespaces)
  (values))

;;; -------------------------------------------------------

(defun expect-character (expected-character
                         &key (test #'char=))
  "Determines whether the *CURRENT-CHARACTER* matches the
   EXPECTED-CHARACTER, naiting the TEST, which defaults to ``char='', on
   confirmation advancing to the next position in the *SOURCE* and
   returning no value; otherwise signals an error of an unspecified
   type."
  (declare (type character      expected-character))
  (declare (type character-test test))
  (cond
    ((eof-p)
      (error "Expected the character \"~c\" at position ~d, ~
              but encountered EOF."
        *current-character* *current-position*))
    ((funcall test *current-character* expected-character)
      (advance))
    (T
      (error "Expected the character \"~c\" at position ~d, ~
              but encountered \"~c\"."
        *current-character* *current-position* expected-character)))
  (values))

;;; -------------------------------------------------------

(defun expect-keyword (expected-keyword)
  "Determines whether, proceeding from the *CURRENT-POSITION* into the
   *SOURCE*, the subsequent characters replicate the EXPECTED-KEYWORD
   sequence in a case-insensitive fashion, on confirmation translating
   the position cursor beyond the matching section, while no returning
   value; otherwise an error of an unspecified type is signaled."
  (declare (type string expected-keyword))
  (loop
    for expected-character of-type character across expected-keyword
    do  (expect-character expected-character :test #'char-equal))
  (values))

;;; -------------------------------------------------------

(defun expect-command-coda ()
  "Determines whether, proceeding from the *CURRENT-POSITION* into the
   *SOURCE* either the end of the latter immdiately follows, designating
   the end of the TCC program, or a sequence of one or more whitespaces
   is encountered, which bifurcates into the contingencies for both the
   program termination or the segregation from a subsequent command, on
   confirmation returning no value; otherwise an error of an unspecified
   type is signaled."
  (unless (or (eof-p)
              (on-whitespace-p))
    (error "Command not separated by whitespaces; instead encountered ~
            the character \"~c\" at position ~d."
      *current-character* *current-position*))
  (values))

;;; -------------------------------------------------------

(defun read-command-name ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   command name and returns a ``command-type'' representation thereof."
  (the command-type
    (get-command
      (with-output-to-string (identifier)
        (declare (type string-stream identifier))
        (loop while (on-command-identifier-p) do
          (write-char (advance) identifier))))))

;;; -------------------------------------------------------

(defun read-number ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   signed or unsigned integer number and returns the same."
  (the integer
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (when (on-sign-p)
          (write-char (advance) digits))
        (loop while (on-digit-p) do
          (write-char (advance) digits))))))

;;; -------------------------------------------------------

(defun read-index ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads an
   unsigned positive integer number, nait for the employment as a column
   or row index, and returns the same."
  (the index
    (parse-integer
      (with-output-to-string (digits)
        (declare (type string-stream digits))
        (loop while (on-digit-p) do
          (write-char (advance) digits))))))

;;; -------------------------------------------------------

(defun read-string ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   string ensconced in double quotation marks ('\"') and returns the
   same."
  (expect-character #\")
  (the string
    (with-output-to-string (content)
      (declare (type string-stream content))
      (loop do
        (case *current-character*
          ((NIL)
            (error "Unterminated string literal at position ~d."
              *current-position*))
          (#\"
            (advance)
            (loop-finish))
          (otherwise
            (write-char (advance) content)))))))

;;; -------------------------------------------------------

(defun read-table-name ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   table name and returns a string representation thereof."
  (the string
    (with-output-to-string (table-name)
      (declare (type string-stream table-name))
      (expect-match #'table-name-start-character-p "a table name")
      (loop while (on-table-name-p) do
        (write-char (advance) table-name)))))

;;; -------------------------------------------------------

(defun read-value ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   cell value and returns its parsed form."
  (the cell-value
    (cond
      ((eof-p)
        (error "Expected a value, but encountered EOF at position ~d."
          *current-position*))
      ((on-digit-p)
        (read-number))
      ((on-sign-p)
        (read-number))
      ((on-character-p #\")
        (read-string))
      (T
        (error "Unrecognized value character \"~c\" at positiion ~d."
          *current-character* *current-position*)))))

;;; -------------------------------------------------------

(defun read-values-list ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   value list, ensconcing zero or more cell values in a twain of left
   parenthesis (\"(\") and right parenthesis (\")\"), and returns a list
   of the encountered objects in their encountered order."
  (expect-character #\()
  (let ((values NIL))
    (declare (type (list-of cell-value) values))
    (cond
      ((eof-p)
        (error "Unterminated values list at position ~d."
          *current-position*))
      ((on-character-p #\))
        (advance))
      (T
        (loop
          initially
            (skip-whitespaces)
            (push (read-value) values)
            (skip-whitespaces)
          while (on-character-p #\,)
          do
            (advance)
            (skip-whitespaces)
            (push (read-value) values)
            (skip-whitespaces)
          finally
            (setf values
              (nreverse values)))
        (expect-character #\))))
    (the (list-of cell-value) values)))

;;; -------------------------------------------------------

(defun read-command ()
  "Proceeding from the *CURRENT-POSITION* into the *SOURCE*, reads a
   command and returns a ``Command'' representation thereof."
  (let ((command-type (read-command-name)))
    (declare (type command-type command-type))
    (the Command
      (case command-type
        (:create
          (expect-whitespace)
          (let ((table-name (read-table-name)))
            (declare (type string table-name))
            (make-command :create table-name)))
        
        (:in
          (expect-whitespace)
          (let ((table-name (read-table-name)))
            (declare (type string table-name))
            (expect-whitespace)
            (expect-keyword "CHOOSE")
            (expect-whitespace)
            (let ((column-number (read-index)))
              (declare (type index column-number))
              (make-command :in table-name column-number))))
        
        (:insert
          (expect-whitespace)
          (let ((table-name (read-table-name)))
            (declare (type string table-name))
            (expect-whitespace)
            (expect-keyword "VALUES")
            (skip-whitespaces)
            (let ((values (read-values-list)))
              (declare (type (list-of cell-value) values))
              (make-command :insert table-name values))))
        
        ;; DELETE {table_name} WHERE {row_index}
        (:delete
          (expect-whitespace)
          (let ((table-name (read-table-name)))
            (declare (type string table-name))
            (expect-whitespace)
            (expect-keyword "WHERE")
            (expect-whitespace)
            (let ((row-number (read-index)))
              (declare (type index row-number))
              (make-command :delete table-name row-number))))
        
        (otherwise
          (error "Invalid command type: ~s." command-type))))))

;;; -------------------------------------------------------

(defun extract-commands ()
  "Extracts and returns from the *SOURCE* a list of its commands."
  (let ((commands NIL))
    (declare (type (list-of Command) commands))
    (loop do
      (skip-whitespaces)
      (cond
        ((eof-p)
          (loop-finish))
        ((on-command-identifier-p)
          (push (read-command) commands)
          (expect-command-coda))
        (T
          (error "Unexpected character \"~c\" at position ~d ~
                  during the extractions of commands."
            *current-character* *current-position*))))
    (the tcc-program
      (nreverse commands))))

;;; -------------------------------------------------------

(defun initialize-source (source)
  "Sets the *SOURCE* to the SOURCE, resets the *CURRENT-POSITION* to
   zero (0), sets the *CURRENT-CHARACTER* to the incipient character,
   and returns no value."
  (declare (type string source))
  (setf *source*            source)
  (setf *current-position*  0)
  (setf *current-character*
    (when (array-in-bounds-p *source* *current-position*)
      (char *source* *current-position*)))
  (values))

;;; -------------------------------------------------------

(defun parse-TCC (code)
  "Extracts and returns from the piece of TCC source CODE the
   incorporated commands."
  (declare (type string code))
  (initialize-source code)
  (the tcc-program
    (extract-commands)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Invalid-Table-Name-Error (error)
  ((offending-name
    :initarg       :offending-name
    :initform      (error "Missing offending table name.")
    :reader        invalid-table-name-error-offending-name
    :type          string
    :documentation "The unrecognized table name peccant as the issue's
                    etiology."))
  (:report
    (lambda (condition stream)
      (declare (type Invalid-Table-Name-Error condition))
      (declare (type destination              stream))
      (format stream "There exists no table bearing the name ~s."
        (invalid-table-name-error-offending-name condition))))
  (:documentation
    "The ``Invalid-Table-Name-Error'' serves in the signaling of an
     anomalous situation involving a request for a table by a name not
     contained in the maintaining registry."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of table.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-row (&optional (elements NIL))
  "Creates and returns a new row comprehending the optional ELEMENTS."
  (declare (type (list-of cell-value) elements))
  (the row
    (make-array (length elements)
      :element-type     'cell-value
      :initial-contents elements
      :adjustable       NIL
      :fill-pointer     NIL)))

;;; -------------------------------------------------------

(defun make-row-list ()
  "Creates and returns an initially empty adjustable vector of rows,
   nait for modifications."
  (the row-list
    (make-array 0
      :element-type    'row
      :initial-element (make-row)
      :adjustable      T
      :fill-pointer    0)))

;;; -------------------------------------------------------

(defstruct (Table
  (:constructor make-table (name)))
  "The ``Table'' class represents a table as a vector of rows, admitting
   heterogeneity in their extents, while maintaining the same as
   one-dimensional simple arrays."
  (name (error "Missing table name.") :type string)
  (rows (make-row-list)               :type row-list))

;;; -------------------------------------------------------

(defun table-choose-column (table column-index)
  "Prints the cell values commorant in the file designated in the TABLE
   by the COLUMN-INDEX, skipping those rows not covered by the index,
   and returns no value."
  (declare (type Table table))
  (declare (type index column-index))
  (loop for current-row of-type row across (table-rows table) do
    (when (array-in-bounds-p current-row (1- column-index))
      (format T "~a"
        (aref current-row (1- column-index)))))
  (values))

;;; -------------------------------------------------------

(defun table-insert-row (table values)
  "Appends a new row comprehending the VALUES' elemens to the end of the
   TABLE and returns no value."
  (declare (type Table      table))
  (declare (type value-list values))
  (vector-push-extend
    (make-row values)
    (table-rows table))
  (values))

;;; -------------------------------------------------------

(defun table-delete-row (table row-index)
  "Removes the row located at the one-based ROW-INDEX in the TABLE and
   returns no value.
   ---
   If the ROW-INDEX transcends the range of valid indices in the TABLE,
   this operation exerts no effect."
  (declare (type Table table))
  (declare (type index row-index))
  (when (< row-index (length (table-rows table)))
    (setf (table-rows table)
      (delete-if
        (constantly T)
        (table-rows table)
        :start (1- row-index)
        :end   row-index)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter (program)))
  "The ``Interpreter'' contributes an instance responsible for the
   application of actual effect to a sequence of TCC commands, while
   concomitantly entrusted with the castaldy of the database storing the
   tables."
  (program  (error "Missing program.")      :type tcc-program)
  (database (make-hash-table :test #'equal) :type database))

;;; -------------------------------------------------------

(defun interpreter-create-table (interpreter table-name)
  "Registers a new empty table designated by the TABLE-NAME at the
   INTERPRETER and returns no value."
  (declare (type Interpreter interpreter))
  (declare (type string      table-name))
  (setf (gethash table-name
          (interpreter-database interpreter))
    (make-table table-name))
  (values))

;;; -------------------------------------------------------

(defun interpreter-get-table (interpreter table-name)
  "Returns the table registered with the TABLE-NAME at the INTERPRETER,
   or signals an error of an unspecified type upon its absence."
  (declare (type Interpreter interpreter))
  (declare (type string      table-name))
  (the Table
    (or (gethash table-name
          (interpreter-database interpreter))
        (error 'Invalid-Table-Name-Error :offending-name table-name))))

;;; -------------------------------------------------------

(defun interpreter-interpret (interpreter)
  "Interprets the TCC commands stored in the INTERPRETER and returns no
   value."
  (declare (type Interpreter interpreter))
  
  ;; Ensure the start of a line for potential output operations.
  (fresh-line)
  
  (dolist (command (interpreter-program interpreter))
    (declare (type Command command))
    (case (command-type command)
      (:create
        (interpreter-create-table interpreter
          (first (command-arguments command))))
      
      (:in
        (destructuring-bind (table-name column-index)
            (command-arguments command)
          (declare (type string table-name))
          (declare (type index  column-index))
          (table-choose-column
            (interpreter-get-table interpreter table-name)
            column-index)))
      
      (:insert
        (destructuring-bind (table-name values)
            (command-arguments command)
          (declare (type string     table-name))
          (declare (type value-list values))
          (table-insert-row
            (interpreter-get-table interpreter table-name)
            values)))
      
      (:delete
        (destructuring-bind (table-name row-index)
            (command-arguments command)
          (declare (type string table-name))
          (declare (type index  row-index))
          (table-delete-row
            (interpreter-get-table interpreter table-name)
            row-index)))
      
      (otherwise
        (error "Unrecognized command: ~s." command))))
  (values))

;;; -------------------------------------------------------

(defun interpret-TCC (code)
  "Interprets the piece of TCC source CODE and returns no value."
  (interpreter-interpret
    (make-interpreter
      (parse-TCC code)))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-TCC
  "CREATE hello_world
   INSERT hello_world VALUES (\"H\")
   INSERT hello_world VALUES (\"e\")
   INSERT hello_world VALUES (\"l\")
   INSERT hello_world VALUES (\"l\")
   INSERT hello_world VALUES (\"o\")
   INSERT hello_world VALUES (\",\")
   INSERT hello_world VALUES (\" \")
   INSERT hello_world VALUES (\"W\")
   INSERT hello_world VALUES (\"o\")
   INSERT hello_world VALUES (\"r\")
   INSERT hello_world VALUES (\"l\")
   INSERT hello_world VALUES (\"d\")
   INSERT hello_world VALUES (\"!\")
   IN     hello_world CHOOSE 1")

;;; -------------------------------------------------------

;; Print "Hello, World!" employing two columns, the sinistral of which
;; comprehends the moeity "Hello, ", whereas its dextral compernage
;; bears the coda "World!".
(interpret-TCC
  "CREATE hello_world
   INSERT hello_world VALUES (\"H\", \"W\")
   INSERT hello_world VALUES (\"e\", \"o\")
   INSERT hello_world VALUES (\"l\", \"r\")
   INSERT hello_world VALUES (\"l\", \"l\")
   INSERT hello_world VALUES (\"o\", \"d\")
   INSERT hello_world VALUES (\",\", \"!\")
   INSERT hello_world VALUES (\" \")
   IN     hello_world CHOOSE 1
   IN     hello_world CHOOSE 2")
