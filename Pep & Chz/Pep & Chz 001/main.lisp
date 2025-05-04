;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Pep & Chz", invented by the Esolang user "PaxtonPenguin"
;; and presented on October 31st, 2023, its haecceity's provenance a
;; novel dictionary's adhibition to Urban Mueller's "brainfuck",
;; desumed from the agnominations imparted to the author's stuffed
;; penguins.
;; 
;; 
;;                             ##########                     
;;                         #################                  
;;                       ######################               
;;                     #########################              
;;                    ###########################             
;;                   #############################            
;;                   ###    #########    ##########           
;;                  ###      #######      ##########          
;;                  ### ##   ####### ##   ##########          
;;                  ### ##   ####### ##   ##########          
;;                 ####      #######      ##########          
;;                 #####    #########    ###########          
;;                 #################################          
;;                 #################################          
;;                ##################################          
;;              ###############  ####################         
;;             ##########         ###################         
;;            #######              ##################         
;;           ######                ##################         
;;           #####                ###################         
;;          #####             #######################         
;;          #####         ############################        
;;          ##########################################        
;;           ######################    ###############        
;;            #################         ##############        
;;              ###########             ###############       
;;              #########                ##############       
;;             #########                 ##############       
;;            ##########                 ##############       
;;            #########                  ##############       
;;           ##########                  ###############      
;;           #########                    ##############      
;;          #########                     ##############      
;;          ########                      ##############      
;;         #########                       #############      
;;         ########                        #############      
;;         ########                        #############      
;;        ########                         #############      
;;        ########                          #############     
;;        #######                           #############     
;;        #######                           #############     
;;       ########                            ############     
;;       ########                            ###########      
;;       ########                            ###########      
;;       ########                            ###########      
;;        #######                            ###########      
;;        #######                            ###########      
;;        ########                           ##########       
;;         ########                         ###########       
;;         #########                        ##########        
;;          ##########                     ###########        
;;           ############                ############         
;;            ##################     ###############          
;;             ####################################           
;;              #################################             
;;                 #############################              
;;                    #######################                 
;;                         ##############                     
;; 
;; 
;; Concept
;; =======
;; The Pep & Chz programming language establishes a syntactical
;; renovation upon the substratum provided by the brainfuck language,
;; applying to the entheus' single-symbol names a cambistry for the
;; derived language's author's stuffed penguins' nevenings.
;; 
;; The novelty retains any other aspect in its ipsissima verba
;; designment; in particular, operating on a bilaterally infinite
;; dispansion of unsigned byte-valued cells, upon whose entirety a
;; mobile cell pointer exerts is influence, selecting at any instant
;; during a program's execution the currently active cell, this object
;; being entalented with the exclusive amenability for perquisitions
;; and modulations.
;; 
;; 
;; Instructions
;; ============
;; A mere reformulation of brainfuck, Pep & Chz's competences do neither
;; remain alow nor rise aboon that of its stock-father's, enumerating in
;; its acquisition the acquainted octuple membership.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be a requisite mete of nortelry's
;; adhibition concerning the language's operative components:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   peep    | Increments the current cell value by one. Upon a
;;           | transgression of its upper bourne of 255, the state
;;           | wraps around to the minimum of zero (0).
;;   ..................................................................
;;   chaz    | Decrements the current cell value by one. Upon a
;;           | transgression of its lower bourne of zero (0), the state
;;           | wraps around to the maximum of 255.
;;   ..................................................................
;;   rebeca  | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   petey   | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   peeper  | If the current cell contains the value zero (0), moves
;;           | the instruction pointer (IP) forward to the position
;;           | immediately succeeding the matching "plopers"
;;           | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   plopers | If the current cell does not contain the value zero (0),
;;           | moves the instruction pointer (IP) back to the position
;;           | immediately succeeding the matching "peeper"
;;           | instruction; otherwise proceeds as usual.
;;   ..................................................................
;;   gery    | Prints the character whose ASCII code corresponds to the
;;           | current cell value to the standard output conduit.
;;   ..................................................................
;;   cristal | Queries the standard input conduit for a character and
;;           | stores its ASCII code in the current cell.
;;   ------------------------------------------------------------------
;; 
;; == PEP & CHZ AND BRAINFUCK ==
;; The fact of its cleronomy entalents the Pep & Chz programming
;; language with patration in its potential for an equiparation with
;; the brainfuck entheus. The following table shall furnish the vincula
;; betwixt these two specimens in regards to their syntaxis and
;; operative elements:
;; 
;;   ---------------------
;;   Pep & Chz | brainfuck
;;   ----------+----------
;;   peep      | +
;;   .....................
;;   chaz      | -
;;   .....................
;;   rebeca    | >
;;   .....................
;;   petey     | <
;;   .....................
;;   peeper    | [
;;   .....................
;;   plopers   | ]
;;   .....................
;;   gery      | .
;;   .....................
;;   cristal   | ,
;;   ---------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been realized in the programming
;; language Common Lisp, operating in a kenspeckle mode by which the
;; source code string is directly transformed into a tantamount
;; Common Lisp program by adminiculum of macros.
;; 
;; The highest mete of conspectuity harbors its commorancy in the
;; jump instruction replications, "peeper" and "plopers", the same
;; enjoy their causata's replication via a "go to"-based approach,
;; rendered as the ultimity of a Common Lisp "tagbody" form that assigns
;; to each twissel of matching forward and back jump token a navigable
;; label of unique identification.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-04-29
;; 
;; Sources:
;;   [esolang2023Pep&Chz]
;;   The Esolang contributors, "Pep & Chz", November 2nd, 2023
;;   URL: "https://esolangs.org/wiki/Pep_%26_Chz"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Interprets the OBJECT in its role as a \"generalized boolean\" and
   produces a veridicous Boolean paregal thereof, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, returns ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of type operations.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun designates-any-type-p (type-specifier)
  "Determines whether the TYPE-SPECIFIER represents the generic sentinel
   ``*'', its capacitation sufficiently potent as to represent any
   type."
  (declare (type T type-specifier))
  (the boolean
    (get-boolean-value-of
      (and (symbolp type-specifier)
           (eq      type-specifier '*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value compact of eight
   accolent bits, and as such a commorant of the closed integral
   interval [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table whose componency's
   amplexation includes zero or more entries, each such a twissel of a
   key conforming with the KEY-TYPE and a value adhering to the
   VALUE-TYPE, both governed by the generic sentinel ``*'' as a
   default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (or
              (and (designates-any-type-p key-type)
                   (designates-any-type-p value-type))
              (loop
                for current-key
                  of-type T
                  being the hash-keys in (the hash-table candidate)
                using
                  (hash-value current-value)
                always
                  (and
                    (or (designates-any-type-p key-type)
                        (typep current-key   key-type))
                    (or (designates-any-type-p value-type)
                        (typep current-value value-type))))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a list whose componency's edification
   founds upon a list encompassing zero or more elements of the
   ELEMENT-TYPE, as the default serving the generic sentinel ``*''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (or
              (designates-any-type-p element-type)
              (loop
                for current-element of-type T in (the list candidate)
                always (typep current-element element-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype tokenizer ()
  "The ``tokenizer'' type defines a function which upon each invocation
   returns the next word from an underlying string."
  '(function () string))

;;; -------------------------------------------------------

(deftype tokenizer-function ()
  "The ``tokenizer-function'' type defines a function which upon each
   invocation returns the next word from an underlying string, serving
   as an alternative to the ``tokenizer'' type in cases where a type
   specifier's participation is inflicted with prohibition, such as in
   a ``the'' special form."
  'function)

;;; -------------------------------------------------------

(deftype jump-label-twain ()
  "The ``jump-label-twain'' type defines a composite of two jump point
   labels, ligated in a conceptul sense by their contingency for an
   iterance construct's replication, and reified as symbolic names."
  '(cons symbol symbol))

;;; -------------------------------------------------------

(deftype jump-point-table ()
  "The ``jump-point-table'' type defines a mapping betwixt a numeric
   jump point pair identifier and the twain of symbolic labels
   entalented with the eligibility for the deployment as ``tagbody'' or
   ``prog'' program points, and realized in a hash table whose fixnum
   keys accommodate the jump point twissel identifiers, responding with
   a cons of symbols, the first compartment lending a commorancy to the
   forward jump point's label name, the second moeity entailing the
   back jump label identifier."
  '(hash-table-of fixnum jump-label-twain))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   demarcating in its diorism, among other specimens, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   the compass of its diorism amplecting the space, horizontal tab,
   linefeed, as well as newline entities, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate
        '(#\Linefeed #\Newline #\Space #\Tab)
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string operations.                         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-start-of-next-word (source start-position)
  "Proceeding from the START-POSITION into the SOURCE, returns the
   position at which the nearest word commences, or, upon the search's
   failure, responds with the SOURCE's length."
  (declare (type string source))
  (declare (type fixnum start-position))
  (the fixnum
    (or (position-if-not #'whitespace-character-p source
          :start start-position)
        (length source))))

;;; -------------------------------------------------------

(defun locate-end-of-current-word (source start-position)
  "Proceeding from the START-POSITION into the SOURCE, and expecting
   the same to wone inwith a word's bournes, locates the position
   immediately succeeding this word's desinent character."
  (declare (type string source))
  (declare (type fixnum start-position))
  (the fixnum
    (or (position-if #'whitespace-character-p source
          :start start-position)
        (length source))))

;;; -------------------------------------------------------

(defun locate-next-word-bournes (source start-position)
  "Proceeding from the START-POSITION into SOURCE, locates the nearest
   word's bournes and returns two values:
     (1) The detected word's inclusive start index in the SOURCE.
     (2) The position into the SOURCE immediately succeeding the
         detected word's desinent character."
  (declare (type string source))
  (declare (type fixnum start-position))
  (let ((word-start-position
          (locate-start-of-next-word source start-position)))
    (declare (type fixnum word-start-position))
    (the (values fixnum fixnum)
      (values word-start-position
        (locate-end-of-current-word source word-start-position)))))

;;; -------------------------------------------------------

(defun extract-next-word (source start-position)
  "Proceeding from the START-POSITION into SOURCE, locates the nearest
   word and returns two values:
     (1) The detected word as a fresh string.
     (2) The position into the SOURCE immediately succeeding the
         detected word's desinent character."
  (declare (type string source))
  (declare (type fixnum start-position))
  (multiple-value-bind (word-start-position word-end-position)
      (locate-next-word-bournes source start-position)
    (declare (type fixnum word-start-position))
    (declare (type fixnum word-end-position))
    (the (values string fixnum)
      (values
        (subseq source word-start-position word-end-position)
        word-end-position))))

;;; -------------------------------------------------------

(defun string-is-empty-p (source)
  "Determines whether the SOURCE represents an empty string, tallying
   exactly zero constituents, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (the boolean
    (get-boolean-value-of
      (string= source ""))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of string tokenizer.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tokenizer (source)
  "Creates and returns a fresh tokenizer function dedicated to the
   SOURCE string's segregation into words.
   ---
   Upon each invocation, the tokenizer will deliver the next word from
   its underlying SOURCE. In its exhaustion's case, this result will
   always amount to a fresh empty string."
  (declare (type string source))
  (let ((current-position 0))
    (declare (type fixnum current-position))
    (the tokenizer-function
      #'(lambda ()
          (multiple-value-bind (next-word new-position)
              (extract-next-word source current-position)
            (declare (type string next-word))
            (declare (type fixnum new-position))
            (the string
              (prog1 next-word
                (setf current-position new-position))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program memory.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type unsigned-byte *tape-bits*))
(declaim (type integer       *cell-pointer-position*))
(declaim (type integer       *smallest-cell-index*))
(declaim (type T             *cell-pointer-byte-location*))

;;; -------------------------------------------------------

(defparameter *tape-bits* #b00000000
  "The program memory tape conceived as a bilaterally infinite
   dispansion of unsigned byte-valued cells, and implemented by
   adminiculum of a single integer-encoded bit sequence.
   ---
   Each eight consecutive bits coalesce to form one cell, with the
   left-most defined unit always residing at the lowest bits,
   commencing with the bit offset zero (0).")

(defparameter *cell-pointer-position* 0
  "The cell pointer's current position, that is, the signed integer
   cell index.")

(defparameter *smallest-cell-index* 0
  "The smallest value assumed by the *CELL-POINTER-POSITION* during the
   course of a program, employed during the translation of the cell
   pointer index into a non-negative offset into the *TAPE-BITS*.")

;;; -------------------------------------------------------

(defun prepare-pristine-tape ()
  "Resets the memory's state to its inchoate configuration and returns
   no value."
  (psetf *tape-bits*             #b00000000
         *cell-pointer-position* 0
         *smallest-cell-index*   0)
  (values))

;;; -------------------------------------------------------

(defun translate-cell-index-to-bit-offset ()
  "Returns the zero-based bit offset into the *TAPE-BITS* corresponding
   to the address of the first bit comprising the cell located at the
   *CELL-POINTER-POSITION*."
  (the (integer 0 *)
    (* (- *cell-pointer-position*
          *smallest-cell-index*)
       8)))

;;; -------------------------------------------------------

;; The byte specifier comprehending the zero-based index into the
;; *TAPE-BITS* corresponding to the first bit that edifies the cell
;; selected by the *CELL-POINTER-POSITION*.
;; 
;; Please note the implementation-dependent nature of the byte specifier
;; in Common Lisp, which prohibits any conclusion about the internal
;; representation of the ``byte'' function or its phenotypical design.
(define-symbol-macro *cell-pointer-byte-location*
  (the T
    (byte 8
      (translate-cell-index-to-bit-offset))))

;;; -------------------------------------------------------

(defun current-cell-value ()
  "Returns the unsigned byte value stored in the current cell."
  (the octet
    (ldb *cell-pointer-byte-location* *tape-bits*)))

;;; -------------------------------------------------------

(defun (setf current-cell-value) (new-value)
  "Stores the NEW-VALUE in the current cell, contingently preceded by
   its wrapping around to accommodate the admissible unsigned byte range
   of [0, 255], and returns no value."
  (declare (type integer new-value))
  (setf (ldb *cell-pointer-byte-location* *tape-bits*)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell ()
  "Increments the current cell value by one, contingently wrapping its
   state around in order to accommodate the admissible byte range of
   [0, 255], and returns no value."
  (incf (current-cell-value))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell ()
  "Decrements the current cell value by one, contingently wrapping its
   state around in order to accommodate the admissible byte range of
   [0, 255], and returns no value."
  (decf (current-cell-value))
  (values))

;;; -------------------------------------------------------

(defun current-cell-contains-zero-p ()
  "Determines whether the current cell contains the value zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (the boolean
    (get-boolean-value-of
      (zerop
        (current-cell-value)))))

;;; -------------------------------------------------------

(defun move-cell-pointer-right ()
  "Translates the cell pointer one step to the right and returns no
   value."
  (incf *cell-pointer-position*)
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left ()
  "Translates the cell pointer one step to the left and returns no
   value."
  (decf *cell-pointer-position*)
  (when (< *cell-pointer-position* *smallest-cell-index*)
    (psetf *smallest-cell-index* *cell-pointer-position*
           *tape-bits*           (ash *tape-bits* 8)))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input and output operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-current-cell ()
  "Prints the character whose ASCII code conforms to the memory's
   current cell value to the standard output and returns no value."
  (format *standard-output* "~c"
    (code-char
      (current-cell-value)))
  (values))

;;; -------------------------------------------------------

(defun input-into-current-cell ()
  "Queries the standard input for a character, stores its ASCII code in
   the memory's current cell, and returns no value."
  (format *standard-output* "~&>> ")
  (finish-output)
  (setf (current-cell-value)
    (char-code
      (read-char *standard-input* NIL #\Null)))
  (clear-input)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type fixnum           *jump-point-enumerator*))
(declaim (type (list-of fixnum) *jump-point-matcher*))
(declaim (type jump-point-table *jump-point-table*))

;;; -------------------------------------------------------

(defparameter *jump-point-enumerator* 0
  "Maintains the next jump point twain labels' number component.")

(defparameter *jump-point-matcher* NIL
  "A stack whose topmost element comprehends the most recently assigned
   jump point label number component, descending in this aspect while
   traversing towards its bottom.")

(defparameter *jump-point-table*
  (make-hash-table :test #'eql)
  "A warklume for the memoization of symbolic forward and back jump 
   label names corresponding to a given numeric jump point identifier.
   ---
   Each recognized jump point number maps two a cons cell composed of
   two symbols, the first constituting the forward jump label name, the
   second the matching back jump label.")

;;; -------------------------------------------------------

(defun prepare-pristine-jump-point-table ()
  "Resets the jump point table builder and returns no value."
  (psetf *jump-point-enumerator* 0
         *jump-point-matcher*  NIL)
  (clrhash *jump-point-table*)
  (values))

;;; -------------------------------------------------------

(defun build-forward-jump-label (jump-point-id)
  "Creates and returns a symbol covenable as a forward jump point label
   in a ``tagbody'' or ``prog'' form, incorporating in its agnomination
   the JUMP-POINT-ID as a suffix component."
  (declare (type fixnum jump-point-id))
  (the symbol
    (intern
      (format NIL "~:@(forward-jump-point-~d~)" jump-point-id))))

;;; -------------------------------------------------------

(defun build-back-jump-label (jump-point-id)
  "Creates and returns a symbol covenable as a back jump point label in
   a ``tagbody'' or ``prog'' form, incorporating in its agnomination
   the JUMP-POINT-ID as a suffix component."
  (declare (type fixnum jump-point-id))
  (the symbol
    (intern
      (format NIL "~:@(back-jump-point-~d~)" jump-point-id))))

;;; -------------------------------------------------------

(defun get-jump-labels (jump-point-id)
  "Returns for the JUMP-POINT-ID the twissel of matching forward and
   back jump labels as a cons comprehending as its components this two
   pieces of data:
     (1) The symbolic agnomination of the forward jump label.
     (2) The symbolic agnomination of the back    jump label."
  (declare (type fixnum jump-point-id))
  (multiple-value-bind (jump-labels entry-for-id-exists-p)
      (gethash jump-point-id *jump-point-table*)
    (declare (type (or null jump-label-twain) jump-labels))
    (declare (type T                          entry-for-id-exists-p))
    (the jump-label-twain
      (if entry-for-id-exists-p
        jump-labels
        (let ((new-jump-labels
                (cons
                  (build-forward-jump-label jump-point-id)
                  (build-back-jump-label    jump-point-id))))
          (declare (type jump-label-twain new-jump-labels))
          (setf (gethash jump-point-id *jump-point-table*)
                new-jump-labels)
          new-jump-labels)))))

;;; -------------------------------------------------------

(defun parse-token (token)
  "Evaluates the TOKEN and returns for thilk a Common Lisp code fragment
   covenable as an operative reprsentation of its causatum.
   ---
   Every response constitutes a list composed of zero or more forms,
   intended for its contents' extraction and incoropration into a final
   compound form, which subsequently will serve as the ultimate Lisp
   program to execute.
   ---
   Upon a non-instruction TOKEN's commission, an empty list, tantamount
   to the ``NIL'' object, is returned."
  (declare (type string token))
  (the (list-of T)
    (cond
      ((string= token "peep")
        (list '(increment-current-cell)))
      
      ((string= token "chaz")
        (list '(decrement-current-cell)))
      
      ((string= token "rebeca")
        (list '(move-cell-pointer-right)))
      
      ((string= token "petey")
        (list '(move-cell-pointer-left)))
      
      ((string= token "peeper")
        (push *jump-point-enumerator* *jump-point-matcher*)
        (incf *jump-point-enumerator*)
        (destructuring-bind (forward-jump-label . back-jump-label)
            (get-jump-labels (first *jump-point-matcher*))
          (declare (type symbol forward-jump-label))
          (declare (type symbol back-jump-label))
          (list
            forward-jump-label
            `(when (current-cell-contains-zero-p)
               (go ,back-jump-label)))))
      
      ((string= token "plopers")
        (if *jump-point-matcher*
          (destructuring-bind (forward-jump-label . back-jump-label)
              (get-jump-labels (pop *jump-point-matcher*))
            (declare (type symbol forward-jump-label))
            (declare (type symbol back-jump-label))
            (list
              back-jump-label
              `(unless (current-cell-contains-zero-p)
                 (go ,forward-jump-label))))
          (error "Unmatched back jump point.")))
      
      ((string= token "gery")
        (list '(output-current-cell)))
      
      ((string= token "cristal")
        (list '(input-into-current-cell)))
      
      (T
        NIL))))

;;; -------------------------------------------------------

(defun parse-tokens (tokenizer)
  "Assembles from the code fragment yielded by the TOKENIZER tokens'
   evaluation a Common Lisp program capacitated to fufil the expected
   devers and returns thilk."
  (declare (type tokenizer tokenizer))
  (the (list-of T)
    `(progn
       (prepare-pristine-tape)
       (prepare-pristine-jump-point-table)
       (tagbody
         ,@(loop
             for current-token
               of-type string
               =       (funcall tokenizer)
             until
               (string-is-empty-p current-token)
             append
               (parse-token current-token))))))

;;; -------------------------------------------------------

(defun parse-code (code)
  "Parses the piece of Pep & Chz source CODE, assembles from the code
   fragment yielded by the TOKENIZER tokens' evaluation a Common Lisp
   program capacitated to fufil the expected devers, and returns thilk."
  (declare (type string code))
  (the (list-of T)
    (parse-tokens
      (make-tokenizer code))))

;;; -------------------------------------------------------

(defmacro evaluate-program-for-code (code)
  "Executes the piece of Pep & Chz source CODE and returns no value."
  (let ((evaluated-code (gensym)))
    (declare (type symbol evaluated-code))
    `(let ((,evaluated-code ,code))
       (declare (type string ,evaluated-code))
       (eval
         (parse-code ,evaluated-code)))))

;;; -------------------------------------------------------

(defun interpret-Pep&Chz (code)
  "Interprets the piece of Pep & Chz source CODE and returns no value."
  (declare (type string code))
  (evaluate-program-for-code code)
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of converter from brainfuck to Pep & Chz.     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-brainfuck-to-Pep&Chz (brainfuck-code
                                       &key (destination NIL))
  "Translates the BRAINFUCK-CODE into an equivalent Pep & Chz program
   and writes thilk to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value, otherwise, for a ``NIL'' DESTINATION,
   produces a fresh string comprehending the result."
  (declare (type string      brainfuck-code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        for current-brainfuck-token
          of-type character
          across brainfuck-code
        do
          (format destination "~@?"
            (case current-brainfuck-token
              (#\+       "~&peep")
              (#\-       "~&chaz")
              (#\>       "~&rebeca")
              (#\<       "~&petey")
              (#\[       "~&peeper")
              (#\]       "~&plopers")
              (#\.       "~&gery")
              (#\,       "~&cristal")
              (otherwise ""))))
      (with-output-to-string (pep&chz-code)
        (declare (type string-stream pep&chz-code))
        (translate-brainfuck-to-Pep&Chz brainfuck-code
          :destination pep&chz-code)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!" to the standard output conduit.
(interpret-Pep&Chz
  "
  peep
  peeper
  chaz
  chaz
  rebeca
  chaz
  peeper
  rebeca
  rebeca
  peep
  rebeca
  chaz
  chaz
  chaz
  chaz
  chaz
  petey
  petey
  plopers
  petey
  chaz
  chaz
  petey
  chaz
  chaz
  chaz
  plopers
  rebeca
  chaz
  gery
  rebeca
  rebeca
  rebeca
  peep
  gery
  rebeca
  rebeca
  gery
  gery
  peep
  peep
  peep
  peeper
  gery
  rebeca
  plopers
  petey
  petey
  petey
  petey
  gery
  peep
  peep
  peep
  gery
  chaz
  chaz
  chaz
  chaz
  chaz
  chaz
  gery
  petey
  petey
  chaz
  gery
  rebeca
  rebeca
  rebeca
  rebeca
  peep
  gery")

;;; -------------------------------------------------------

;; Convert a "Hello, World!" program from brainfuck to Pep & Chz and
;; interpret the same.
(interpret-Pep&Chz
  (translate-brainfuck-to-Pep&Chz
    "+[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+."))

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-Pep&Chz "cristal peeper gery cristal plopers")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(let ((source-code "cristal peeper gery cristal plopers"))
  (declare (type string source-code))
  (interpret-Pep&Chz source-code))

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-Pep&Chz
  "peep
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   rebeca
   rebeca
   cristal
   petey
   petey
   peeper
   chaz
   rebeca
   peep
   peep
   peep
   peep
   peep
   petey
   plopers
   rebeca
   peep
   peep
   peep
   peeper
   rebeca
   chaz
   petey
   chaz
   plopers
   rebeca
   rebeca
   peep
   peep
   peep
   peep
   peep
   peeper
   rebeca
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   peep
   petey
   chaz
   plopers
   rebeca
   chaz
   petey
   petey
   peeper
   rebeca
   rebeca
   gery
   petey
   petey
   plopers
   rebeca
   rebeca
   chaz
   gery")
