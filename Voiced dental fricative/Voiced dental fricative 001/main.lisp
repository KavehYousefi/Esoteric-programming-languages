;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Voiced dental fricative", also yclept, "ðis", invented by
;; the Esolang user "ChuckEsoteric08" and presented on May 29th, 2022,
;; the specimen limning a haecceity's pernor in the derivation from the
;; same author's "Schwa" language, employing a quadruple instruction
;; set in conjunction with a twissel of program states, thilk filsen in
;; an enhaused capacity's gendrure.
;; 
;; 
;; Concept
;; =======
;; The "Voiced dental fricative" establishes a derivation of the
;; "Schwa" programming language, the tenets of whose haecceity are
;; entreparted most conspicuously in the binary program state whose
;; configuration at the instant of a command's execution determines the
;; involved epiphenomena.
;; 
;; == COMMAND EFFECTS DEPEND UPON THE PROGRAM STATE ==
;; A command's deportment at a specific instant during the program's
;; execution limns a dependency upon the contemporaneously governing
;; program state; this set of contingencies exhausts a twissel's
;; coefficiency:
;; 
;;   --------------------
;;   State name | Symbol
;;   -----------+--------
;;   Voiceless  | θ
;;   ....................
;;   Voiced     | ð
;;   --------------------
;; 
;; Commencing at the program's inchoacy in the "Voiceless" state; the
;; command "h" filsts in the switch atwixen the two modes, its causatum
;; the transition from one option to its alternative:
;; 
;;   --------------------------------
;;   Current state | Alternate state
;;   --------------+-----------------
;;   Voiceless (θ) | Voiced (ð)
;;   ................................
;;   Voiced (ð)    | Voiceless (θ)
;;   --------------------------------
;; 
;; == THE MEMORY: AN INPUT/OUTPUT REGISTER ==
;; The eath nature assigned to the language bewrays itself in particular
;; by one's conspectuity's application onto the program memory, thilk
;; wists of a single register, or "accumulator", the perquisition and
;; modification of which does not enjoy a more sophisticated
;; constitution than through input requests and output issuance; both
;; tokens of the commerce being strings of arbitrary length, the
;; characters partaking in the same are desumed from the Unicode
;; repertoire.
;; 
;; == ONLY IDENTIFIERS AND WHITESPACES ARE ADMITTED TO A PROGRAM ==
;; Anent its syntactical edification, a "Voiced dental fricative"
;; program's source code's tolerance does not dispand its magnanimity
;; ayond the prial of instruction identifiers, as well as whitespaces,
;; the latter parhedral constituents to the formatting's valorization.
;; Any other contents' involvement will instigate an abortive error.
;; 
;; 
;; Instructions
;; ============
;; The "Voiced dental fricative" programming language's instruction
;; set exhausts a tesseratomy's numeration. Maugre its dioristic
;; program state, an aefauld member from this contingent, the
;; input/output facility's actuator, patefies a modulation via the
;; mode's governail.
;; 
;; == OVERVIEW ==
;; The following apercu's wike shall be realized in a requisite mete of
;; gnarity's vouchsafement concerning the operative feature, with an
;; equiparation concerning the program state's influence:
;; 
;;   ------------------------------------------------------------------
;;   Command | Voiceless state            | Voiced state
;;   --------+----------------------------+----------------------------
;;   h       | Switches to the alternate state.
;;           |---------------------------------------------------------
;;           | Given the current state, the new mode's obtainance
;;           | emerges from this nomothesy's application:
;;           | 
;;           |   ------------------------------
;;           |   Current state | New state
;;           |   --------------+---------------
;;           |   Voiceless (θ) | Voiced (ð)
;;           |   ..............................
;;           |   Voiced (ð)    | Voiceless (θ)
;;           |   ------------------------------
;;   ..................................................................
;;   t       | Queries the standard input | Prints the accumulator
;;           | conduit for a line of text | string to the standard
;;           | and stores the string in   | output conduit. If no input
;;           | the accumulator.           | has been received
;;           |                            | preveniently, no causatum
;;           |                            | is accompassed.
;;   ..................................................................
;;   d       | Marks the start of a "while" loop which repeats the
;;           | statements ensconced atwixen this "d" token and the
;;           | matching "e" instruction until the accumulator contains
;;           | the character "ð" or "θ".
;;   ..................................................................
;;   e       | Marks the end of the matching "while" loop's body,
;;           | introduced via the "d" instruction.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort peracted in
;; the Common Lisp programming language, the execution itself a per
;; saltum gestion, administered on the input source code string.
;; 
;; == UNICODE SUPPORT IS IMPLEMENTATION-DEPENDENT ==
;; Please note that the concrete character set deployed constitutes a
;; dependency on the Common Lisp implementation; in corollary, Unicode
;; support may or may not be a feature incorporated in the personal
;; environment. The interpreter at hand has been developed and tested
;; with "Steel Bank Common Lisp" (SBCL) version 1.1.4 as part of the
;; "Lisp Cabinet 0.3.5" bundle [christensen:2013:lispcabinet035].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2026-03-08
;; 
;; Sources:
;;   [christensen:2013:lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang:2022:Voiced dental fricative]
;;   The Esolang contributors, "Voiced dental fricative",
;;     June 10th, 2022
;;   URL: "https://esolangs.org/wiki/Voiced_dental_fricative"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-predicated-type
    (name (candidate-name &rest lambda-list)
     &body body)
  "Defines a derived type whose agnomination constitutes the NAME's
   dation, and which acts as a pernor to the LAMBDA-LIST's ipsissima
   verba specifications as its personal formal parameters, concomitantly
   assigning the probed object to the CANDIDATE-NAME, evaluates the BODY
   forms, and construes the desinent form's primary return value as the
   docimasy's adjudgment, a \"generalized boolean\" truth value of
   \"true\" peracting a successful compatibility's assessment's
   signification, while a \"false\" response concludes in the
   candidate's rejection.
   ---
   The first BODY form, in the case of its resolution to a string
   object, is adhibited the role of a documentation string to the type
   definition, being, as a corollary, reappropriated for this purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
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
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-predicated-type hash-table-of
    (candidate
     &optional (key-type '*) (value-type '*))
  "The ``hash-table-of'' type defines a hash table edified upon a
   componency tallying zero or more entries, everichon member among
   these dimidiated into a key compliant with the KEY-TYPE and an allied
   value of the VALUE-TYPE, both governed by a configuration which
   assigns the generic sentinel ``*'' as the default state."
  (and
    (hash-table-p candidate)
    (loop
      for current-key
        of-type T
        being the hash-keys in (the hash-table candidate)
      using
        (hash-value current-value)
      always
        (and (typep current-key   key-type)
             (typep current-value value-type)))))

;;; -------------------------------------------------------

(define-a-predicated-type list-of (candidate
                                   &optional (element-type '*))
  "The ``list-of'' type defines a linked list comprehending zero or more
   members, each element partaking of the same complying with the
   ELEMENT-TYPE, for thilk is specified the generic sentinel ``*'' as
   the default."
  (and
    (listp candidate)
    (every
      #'(lambda (current-element)
          (declare (type T current-element))
          (typep current-element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(deftype iterance-table ()
  "The ``iterance-table'' type defines a bidirectional mapping betwixt
   the loop end points in a \"Voiced dental fricative\" program,
   mediated per procurationem of their respective zero-based indices
   into the underlying code, and patefying in a hash table whose keys
   and values both assume ``fixnum'' objects."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype state ()
  "The ``state'' type enumerates the valid program states."
  '(member :voiceless :voiced))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, responding for a
   non-``NIL'' input with a ``boolean'' value of ``T''; otherwise, for
   a ``NIL'' OBJECT, produces ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program state operations.              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun query-the-next-state (current-state)
  "Returns the alternate state to the CURRENT-STATE."
  (declare (type state current-state))
  (the state
    (case current-state
      (:voiceless :voiced)
      (:voiced    :voiceless)
      (otherwise
        (error "The program state ~s cannot be recognized."
          current-state)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the iterance table operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contex-the-iterance-points (code)
  "Returns a fresh ``iterance-table'' which alligates the loop end
   points of the respective instructions in the piece of
   \"Voiced dental fricative\" source CODE."
  (declare (type string code))
  (let ((iterance-table (make-hash-table :test #'eql))
        (start-points   NIL))
    (declare (type iterance-table iterance-table))
    (declare (type list           start-points))
    (loop
      for current-token    of-type character across code
      and current-position of-type fixnum    from   0 by 1
      do
        (case current-token
          (#\d
            (push current-position start-points))
          (#\e
            (if start-points
              (let ((start-point (pop start-points))
                    (end-point   current-position))
                (declare (type fixnum start-point))
                (declare (type fixnum end-point))
                (psetf
                  (gethash start-point iterance-table) end-point
                  (gethash end-point   iterance-table) start-point))
              (error "An unmatched loop end point has been detected ~
                      at the position ~d."
                current-position)))
          (otherwise
            NIL))
      finally
        (when start-points
          (error "There remain~p unmatched loop start point~:p at ~
                  the position~:p {~d~^, ~}."
            (length start-points)
            (nreverse start-points))))
    (the iterance-table iterance-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the accumulator operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accumulator-contains-a-loop-terminator-p (accumulator)
  "Determines whether the ACCUMULATOR contains a loop terminating
   character, that is, either \"ð\" or \"θ\", returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type (or null string) accumulator))
  (the boolean
    (convert-into-a-boolean-value
      (and
        accumulator
        (or (string= accumulator #\ð)
            (string= accumulator #\θ))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-the-voiced-dental-fricative-code (code)
  "Interprets the piece of \"Voiced dental fricative\" source CODE and
   returns no value."
  (declare (type string code))
  (let ((ip             0)
        (iterance-table (contex-the-iterance-points code))
        (state          :voiceless)
        (accumulator    NIL))
    (declare (type fixnum           ip))
    (declare (type iterance-table   iterance-table))
    (declare (type state            state))
    (declare (type (or null string) accumulator))
    (symbol-macrolet
        ((current-token
          (the character
            (char code ip))))
      (declare (type character current-token))
      
      (loop while (< ip (length code)) do
        (cond
          ((whitespace-character-p current-token)
            (incf ip))
          
          ((char= current-token #\h)
            (setf state (query-the-next-state state))
            (incf ip))
          
          ((char= current-token #\t)
            (case state
              (:voiceless
                (format        *query-io* "~&>> ")
                (finish-output *query-io*)
                (setf accumulator
                  (read-line *query-io* NIL NIL))
                (clear-input *query-io*))
              (:voiced
                (when accumulator
                  (format        *query-io* "~a" accumulator)
                  (finish-output *query-io*)))
              (otherwise
                (error "The program state ~s in conjunction with the ~
                        character \"~c\" at the position ~d cannot ~
                        be evaluated."
                  state current-token ip)))
            (incf ip))
          
          ((char= current-token #\d)
            (when (accumulator-contains-a-loop-terminator-p accumulator)
              (setf ip
                (gethash ip iterance-table)))
            (incf ip))
          
          ((char= current-token #\e)
            (setf ip
              (gethash ip iterance-table)))
          
          (T
            (error "The character \"~c\" at the position ~d is ~
                    invalid."
              current-token ip))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating cat program whose perpetuation may only be cancelled via a
;; "ð" or "θ" input.
(interpret-the-voiced-dental-fricative-code "dththe")
