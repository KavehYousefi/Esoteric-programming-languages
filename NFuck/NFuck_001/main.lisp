;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "NFuck", invented by the Esolang user
;; "SoYouWantMeToDoSomethingButIWont" in the year 2019, which translates
;; the memory of its linguistic entheus, Urban Mueller's "brainfuck",
;; into an n-dimensional space.
;; 
;; Instructions
;; ============
;; NFuck, as a cognate of brainfuck, enlists all of its capabilities
;; either in a verbatim or modified guise, extending, where necessity
;; redes, the compass. The exclusive environment in which these
;; discrepancies arise relates to an incompatibility in the memory
;; models: The derivate's specimen enhances its provenance's linear
;; arrangement to an arbitrary tally of dimensions.
;; 
;; == OVERVIEW ==
;; Please note the caret ("^") which, when empighted below a segment of
;; a command, marks the same as a placeholder, not an element to be
;; inserted verbatim into the code.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   +       | Increments the current cell value by one.
;;           | Appropriated verbatim from brainfuck.
;;   ..................................................................
;;   -       | Decrements the current cell value by one.
;;           | Appropriated verbatim from brainfuck.
;;   ..................................................................
;;   :       | Toggles the movement mode betwixt negative and positive.
;;           | A warkloom which operates implicitly to enable the
;;           | simulation of brainfuck's "<" and ">" instructions.
;;   ..................................................................
;;   n       | Moves the memory cell pointer one unit (1) in the {n}-th
;;           | dimension along the current direction determined by the
;;   ^       | program's mode.
;;           | The value must be an unsigned integer number composed of
;;           | one or more decimal digits.
;;           | Achieves, in coefficiency with the ":" instruction, an
;;           | effect tantamount to brainfuck's "<" and ">".
;;   ..................................................................
;;   .       | Outputs to the standard conduit the ASCII character
;;           | associated with the current cell value.
;;           | Appropriated verbatim from brainfuck.
;;   ..................................................................
;;   ,       | Queries the user for a character and stores its ASCII
;;           | code into the current cell.
;;           | Appropriated verbatim from brainfuck.
;;   ..................................................................
;;   [       | If the current cell value equals zero (0), relocates the
;;           | instruction pointer to the position immediately
;;           | following the matching "]"; otherwise proceeds as usual.
;;           | Appropriated verbatim from brainfuck.
;;   ..................................................................
;;   ]       | If the current cell value does not equal zero (0),
;;           | relocates the instruction pointer to the position
;;           | immediately following the matching "["; otherwise
;;           | proceeds as usual.
;;           | Appropriated verbatim from brainfuck.
;;   ------------------------------------------------------------------
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Despite its state as a derivation of brainfuck, a few points of
;; ambiguity retain commorancy in the language's protolog; a few of them
;; shall be enumerated in this section.
;; 
;; == WHICH MODE DOES A PROGRAM INITIALLY ASSUME? ==
;; A dioristic attribute of NFuck, its memory's multidimensional
;; extension imposes the requisitum of adscititious facilities for its
;; pointer's relocation. The language solves this predicament by the
;; introduction of a "mode", a discriminator of the general motion that
;; either assigns it to the negative or the positive moeity, and a
;; command defined by a single non-negative integer scalar which
;; specifies the dimension to translate along.
;; 
;; It eludes, however, explicit mentioning which mode the program
;; resides in at its inception --- that is, whether inclined along the
;; positive or the negative axis.
;; 
;; The judgment has been settled that, by acquaintance with similar
;; brainfuck derivatives, the positive case shall be assumed as the
;; default.
;; 
;; == HOW IS THE ENUMERATION OF THE DIMENSIONS EXERCISED? ==
;; NFuck, being based upon an n-dimensional perspective upon the program
;; memory, requires an indexing of the same. In spite of this, no
;; explicit enumeration scheme has been announced as canonical. Two
;; alternatives may be propounded with a serious mete of tenability:
;; 
;;   (1) ZERO-BASED INDEXING:
;;       Subscripts start with the value zero (0).
;;   
;;   (2) ONE-BASED INDEXING:
;;       Subscripts start with the value one (1).
;; 
;; Its adhibition in a preponderance of computer science contexts,
;; especially popular programming languages such as C and Java, have
;; been reckoned as sufficienly peisant to approve the first option (1),
;; the zero-based indexing, as official.
;; 
;; == DOES NFUCK OFFER INPUT AND OUTPUT? ==
;; While the original author delineates their creation with curtailed
;; communication as "Basically this is BrainFuck with N Dimensions.",
;; two substantial constituents from its inspiration are lacking: the
;; output command "." and the input facility ",". It cannot be declared
;; with perfect certitude whether this omission represents an act of
;; deliberation or a lapsus.
;; 
;; It has been chosen to incorporate these two language constructs based
;; upon the claim of a consanguinity with brainfuck.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2022-09-17
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/NFuck"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype hash-table-of (&optional (key-type T) (value-type T))
  "The ``hash-table-of'' type defines a hash table of zero or more
   entries, each key of which conforms to the KEY-TYPE and associates
   with a value of the VALUE-TYPE, both defaulting to ``T''."
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

(deftype mode ()
  "The ``mode'' type enumerates the possible directions along a
   dimension's axis."
  '(member :negative :positive))

;;; -------------------------------------------------------

(deftype rank ()
  "The ``rank'' type defines the number of dimensions as a non-negative
   integer."
  '(integer 0 *))

;;; -------------------------------------------------------

(deftype pointer ()
  "The ``pointer'' type defines a memory cell pointer as a
   one-dimensional simple array of integers, the values of which
   designate the dimensions, while the sequence length corresponds to
   the memory's rank (tally of dimensions)."
  '(simple-array integer (*)))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type represents the n-dimensional memory as a hash
   table which maps a one-dimensional simple array, containing for each
   dimension the current location, to an arbitrary integer
   representative of the respective cell value."
  '(hash-table-of pointer integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pointer (rank)
  "Creates and returns a memory cell pointer of the RANK tally of
   dimensions."
  (declare (type rank rank))
  (the pointer
    (make-array rank
      :element-type 'integer
      :adjustable   NIL
      :fill-pointer NIL)))

;;; -------------------------------------------------------

(defun query-for-rank ()
  "Queries the user for the number of dimensions, or rank, using the
   standard input, and returns the same.
   ---
   If the user input does not resolve to an unsigned integer number, or
   represents a negative value, an error of an unspecified type is
   signaled."
  (format T "~&Please enter the number of dimensions \"n\": ")
  (let ((input (parse-integer (read-line))))
    (declare (type integer input))
    (clear-input)
    (the rank
      (if (minusp input)
        (error "Negative dimension specified: ~d." input)
        input))))

;;; -------------------------------------------------------

(defun toggle-mode (mode)
  "Returns the opposite of the specified MODE."
  (declare (type mode mode))
  (the mode
    (case mode
      (:negative :positive)
      (:positive :negative)
      (otherwise (error "Invalid mode: ~s." mode)))))

;;; -------------------------------------------------------

(defun interpret-NFuck (code)
  "Interprets the piece of NFuck CODE and returns no value."
  (declare (type string code))
  
  (let ((rank (query-for-rank)))
    (declare (type rank rank))
    
    (when (plusp (length code))
      (let ((position 0)
            (token    (char code 0))
            (mode     :positive)
            (memory   (make-hash-table :test #'equalp))
            (pointer  (make-pointer rank)))
        (declare (type fixnum              position))
        (declare (type (or null character) token))
        (declare (type mode                mode))
        (declare (type memory              memory))
        (declare (type pointer             pointer))
        
        (labels
            ((advance ()
              "Moves the POSITION cursor to the next character, if
               possible, updates the current TOKEN, and returns no
               value."
              (setf token
                (when (array-in-bounds-p code (1+ position))
                  (char code (incf position))))
              (values))
             
             (recede ()
              "Moves the POSITION cursor to the previous character, if
               possible, updates the current TOKEN, and returns no
               value."
              (setf token
                (when (array-in-bounds-p code (1- position))
                  (char code (decf position))))
              (values))
             
             (move-past-jump-end ()
              "Moves the POSITION cursor forward to the location
               immediately following the matching \"]\", updates the
               current TOKEN, and returns no value."
              (loop with level of-type fixnum = 0 do
                (case token
                  ((NIL)
                    (error "Unterminated \"[\"."))
                  (#\[
                    (incf level)
                    (advance))
                  (#\]
                    (cond
                      ((zerop level)
                        (advance)
                        (loop-finish))
                      (T
                        (decf level)
                        (advance))))
                  (otherwise
                    (advance))))
              (values))
             
             (move-past-jump-start ()
              "Moves the POSITION cursor backward to the location
               immediately following the matching \"[\", updates the
               current TOKEN, and returns no value."
              (loop with level of-type fixnum = 0 do
                (case token
                  ((NIL)
                    (error "Unterminated \"]\"."))
                  (#\]
                    (incf level)
                    (recede))
                  (#\[
                    (cond
                      ((zerop level)
                        (advance)
                        (loop-finish))
                      (T
                        (decf level)
                        (recede))))
                  (otherwise
                    (recede))))
              (values))
             
             (read-dimension ()
              "Reads an unsigned integer number and returns it, moving
               the POSITION cursor to the location immediately following
               the consumed digits, updating the current TOKEN, and
               returning no value."
              (the rank
                (parse-integer
                  (with-output-to-string (digits)
                    (declare (type string-stream digits))
                    (loop while (and token (digit-char-p token)) do
                      (write-char token digits)
                      (advance))))))
             
             (check-dimension (dimension)
              "Checks whether the DIMENSION is located inside of the
               valid bounds established by the POINTER, on confirmation
               returning no value; otherwise signaling an error of an
               unspecified type."
              (declare (type rank dimension))
              (unless (array-in-bounds-p pointer dimension)
                (error "Invalid dimension ~d. ~
                        Should be in the range [0, ~d]."
                  dimension (1- rank)))
              (values))
             
             (current-cell ()
              "Returns the current cell value."
              (the integer (gethash pointer memory 0)))
             
             ((setf current-cell) (new-value)
              "Sets the current cell's content to the NEW-VALUE and
               returns no value."
              (declare (type integer new-value))
              (setf (gethash pointer memory 0) new-value)
              (values)))
          
          (loop while token do
            (case token
              ((NIL)
                (loop-finish))
              
              ;; Increment current cell.
              (#\+
                (incf (current-cell))
                (advance))
              
              ;; Decrement current cell.
              (#\-
                (decf (current-cell))
                (advance))
              
              ;; Toggle direction mode.
              (#\:
                (setf mode (toggle-mode mode))
                (advance))
              
              ;; Read dimension and translate along it.
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (let ((dimension (read-dimension)))
                  (declare (type rank dimension))
                  (check-dimension dimension)
                  (case mode
                    (:positive
                      (incf (aref pointer dimension)))
                    (:negative
                      (decf (aref pointer dimension)))
                    (otherwise
                      (error "Invalid mode: ~s." mode))))
                (advance))
              
              ;; Jump forward past matching "]" if current cell value
              ;; equals zero (0).
              (#\[
                (advance)
                (when (zerop (current-cell))
                  (move-past-jump-end)))
              
              ;; Jump back after matching "[" if current cell value does
              ;; not equal zero (0).
              (#\]
                (cond
                  ((not (zerop (current-cell)))
                    (recede)
                    (move-past-jump-start))
                  (T
                    (advance))))
              
              ;; Output ASCII character corresponding to current cell
              ;; value.
              (#\.
                (write-char (code-char (current-cell)))
                (advance))
              
              ;; Query for ASCII character and store its code in the
              ;; current cell.
              (#\,
                (format T "~&Please enter an ASCII character: ")
                (let ((input (read-char)))
                  (declare (type character input))
                  (clear-input)
                  (setf (current-cell) (char-code input)))
                (advance))
              
              ;; Any other character is construed as a comment.
              (otherwise
                (advance))))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Infinitely repeating cat program.
(interpret-NFuck ",.[,.]")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which operates in the sixth
;; dimension.
;; Requires a memory of at least n = 6 dimensions.
(interpret-NFuck "5 ,.[,.]")
