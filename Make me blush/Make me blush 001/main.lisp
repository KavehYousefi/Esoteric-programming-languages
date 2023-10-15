;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Make me blush", invented by the Esolang user "Cinnamony"
;; and presented on June 26th, 2023, the kenspeckle proprium of which
;; emanates from a twissel of provenances, both defined in relation to
;; its descendancy from Urban Mueller's "brainfuck" --- imprimis, its
;; notational discrepancies, founded upon a more ludibund exposition, as
;; constrasted with the stringent mateotechny exemplified in the
;; stock-father; the second invention appertains to a functional
;; augmentation, provided in the form of a switch, capacitated to issue
;; input and output either in a numeric or classical character-based
;; format.
;; 
;; 
;; Concept
;; =======
;; The "Make me blush" programming language contributes a derivative of
;; brainfuck, divergent, however, in its instruction's diction, as well
;; as, a fortiori, in an input/output mode switch endowed with the
;; competence to influence the reception of input and commission of
;; output either in a verbatim number or conventional character form.
;; 
;; == THE INPUT/OUTPUT MODE: A SWITCH FOR NUMERIC/CHARACTER FORMATS ==
;; A parcel of supererogation, and the most dioristic exposition of the
;; "Make me blush" language's indicia, apart from its donet, manifests
;; in the input/output mode, or I/O mode, a switch whose state
;; determines whether input requests and output commissions shall be
;; accompassed in a direct numeric guise, or adhere to the classical
;; brainfuck notions of ASCII character communications.
;; 
;; At a program's inchoation specified in its numeric mode, a request
;; for input involves the expected receipt of a signed integer number to
;; be stored, contingently preceded by a wrapping into the byte range
;; [0, 255], in the current cell. The athwart procession, the display
;; conduit's transmission, directly prints the octet-valued cell datum
;; to the standard output.
;; 
;; The character mode conforms exactly to brainfuck's specification, in
;; that an input's obtention ought to be issued by aide of a character,
;; the ASCII code of which identified and stored in the current cell.
;; A display behest extracts the byte value under the active cell's
;; purview and prints the character corresponding in its ASCII mapping
;; to the standard output conduit.
;; 
;; The "how bout you?" command permits a translation from one mode into
;; the other, and back.
;; 
;; 
;; Instructions
;; ============
;; "Make me blush"'s instruction set, maugre its substrate's
;; ascertainment in brainfuck, applies several enhancements to the
;; cleronomy, concretely in matters relating to the input and output
;; bailiwicks, resulting not only in a numeric superiority --- by
;; tallying nine instead of an octuple of members ---, but also the
;; twifaced operations of its communication channels.
;; 
;; == OVERVIEW ==
;; An apercu concerning the language's operative facilities shall be
;; administered as a warklume for a basic nortelry's communication:
;; 
;;   ------------------------------------------------------------------
;;   Command           | Effect
;;   ------------------+-----------------------------------------------
;;   rawr              | Moves the cell pointer one step to the right.
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent ">".
;;   ..................................................................
;;   uwu               | Moves the cell pointer one step to the left.
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent "<".
;;   ..................................................................
;;   lets date at      | Increments the current cell value by one (1).
;;                     | If the new cell value exceeds the upper bourne
;;                     | of 255, the state is wrapped around to the
;;                     | minimum of zero (0).
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent "+".
;;   ..................................................................
;;   lets kiss at      | Decrements the current cell value by one (1).
;;                     | If the new cell value violates the lower
;;                     | bourne of zero (0), the state is wrapped
;;                     | around to the maximum of 255.
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent "-".
;;   ..................................................................
;;   wanna marry?      | If the input/output mode is set to numbers,
;;                     | queries the standard input for an signed
;;                     | integer number and stores in the current cell,
;;                     | contingently wrapping around the value to
;;                     | ascertain the valid byte range of [0, 255].
;;                     | If the input/output mode is set to characters,
;;                     | queries the standard input for an ASCII
;;                     | character and stores its character code in the
;;                     | current cell.
;;                     |-----------------------------------------------
;;                     | This command constitutes an accommodated
;;                     | variant of the brainfuck equivalent ",".
;;   ..................................................................
;;   you know          | If the input/output mode is set to numbers,
;;                     | prints the current cell value to the standard
;;                     | output in its verbatim numeric form,
;;                     | segregated from other extant content by a
;;                     | space character (" ").
;;                     | If the input/output mode is set to characters,
;;                     | prints the character whose ASCII code equals
;;                     | the current cell value to the standard output.
;;                     |-----------------------------------------------
;;                     | This command constitutes an accommodated
;;                     | variant of the brainfuck equivalent ".".
;;   ..................................................................
;;   how bout you?     | Switches to input/output mode to its opposite
;;                     | state.
;;                     | The following causata hold:
;;                     |   -------------------------------
;;                     |   Current I/O mode | New I/O mode
;;                     |   -----------------+-------------
;;                     |   numbers          | characters
;;                     |   ...............................
;;                     |   characters       | numbers
;;                     |   -------------------------------
;;                     |-----------------------------------------------
;;                     | This command constitutes a novel supplement
;;                     | not present in brainfuck.
;;   ..................................................................
;;   your gay because: | If the current cell value equals zero (0),
;;                     | moves the instruction pointer (IP) forward to
;;                     | the position immediately succeeding the
;;                     | matching "and thats it" command. Otherwise
;;                     | proceeds as usual.
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent "[".
;;   ..................................................................
;;   and thats it      | If the current cell value does not equal zero
;;                     | (0), moves the instruction pointer (IP) back
;;                     | to the position immediately succeeding the
;;                     | matching "your gay because:" command.
;;                     | Otherwise proceeds as usual.
;;                     |-----------------------------------------------
;;                     | This command constitutes a verbatim
;;                     | appropriation of the brainfuck equivalent "]".
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-10-14
;; 
;; Sources:
;;   [esolang2023Makemeblush]
;;   The Esolang contributors, "Make me blush", September 24th, 2023
;;   URL: "https://esolangs.org/wiki/Make_me_blush"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-predicated-type
    (type-name (candidate-variable &rest parameter-list)
     &body body)
  "Defines a new derived type naiting the ``deftype'' infrastructure in
   conjunction with the ``satisfies'' predicate, its agnomination
   nevened by the TYPE-NAME and its parameters specified in the
   PARAMETER-LIST, the essay realized in an automatically accommodated
   anonymous function whose sole input comprehends the object norned by
   the CANDIDATE-VARIABLE and whose implementation is provided by the
   BODY forms, accessing this CANDIDATE-VARIABLE, and returning for a
   confirmation of its eligibility a non-``NIL'' primary return value,
   otherwise ``NIL''.
   ---
   The first BODY form, if constituting a string, is construed as the
   documentation string to the newly derived type, as a corollary its
   removal and appropriation for such purpose reduces the BODY content.
   ---
   This macro's causatum, in simple diction and from a general prospect,
   emulates the following:
     (deftype TYPE-NAME (PARAMETER-LIST)
       (let ((predicate ;; Generate anonymous function in PREDICATE.))
         (if (stringp (first BODY))
           (pop BODY)
           ;; Generate default documentation string.
           )
         `(satisfies
            #'(lambda (CANDIDATE-VARIABLE)
                ,@BODY))))"
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,type-name (,@parameter-list)
       ,(or (and (stringp (first body)) (pop body))
            (format NIL "Defines the derived type ~a." type-name))
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-name)))))

;;; -------------------------------------------------------

(define-predicated-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a list composed of zero or more
   elements, each member of which conforms to the ELEMENT-TYPE,
   defaulting to the generic sentinel ``*''."
  (and
    (listp candidate)
    (every
      #'(lambda (element)
          (declare (type T element))
          (typep element element-type))
      (the list candidate))))

;;; -------------------------------------------------------

(define-predicated-type hash-table-of
    (candidate
     &optional (key-type   '*)
               (value-type '*))
  "The ``hash-table-of'' type defines a hash table compact of zero or
   more entries, each key of which conforms to the KEY-TYPE and
   associates with a value of the VALUE-TYPE, both defaulting to the
   generic sentinel ``*''."
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

(deftype parser-processor ()
  "The ``parser-processor'' type defines a function responsible for the
   production of a parser or combinator's actual effect, responding to
   an input ``Parse-State'' with a ``Parse-Result'' the same
   communicates its success or failure, the ensuing ``Parse-State'', and
   the output as the parser or combinator's contribution to the entire
   parsing stage produce, and which thus conforms to the signature
     lambda (Parse-State) => Parse-Result"
  '(function (Parse-State) Parse-Result))

;;; -------------------------------------------------------

(deftype parser-list ()
  "The ``parser-list'' type defines a list composed of zero or more
   ``Parser'' instances."
  '(list-of Parser))

;;; -------------------------------------------------------

(deftype command ()
  "The ``command'' type enumerates the recognized variations of
   \"Make me blush\" instructions."
  '(member
    :move-left
    :move-right
    :increment
    :decrement
    :input
    :output
    :toggle-io-mode
    :jump-forward
    :jump-back))

;;; -------------------------------------------------------

(deftype io-mode ()
  "The ``io-mode'' type enumerates the recognized variants of input and
   output modes."
  '(member
    :number-mode
    :character-mode))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value composed of eight
   accolent bits, thus acting as a commorant of the integral range
   [0, 255]."
  '(unsigned-byte 8))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of input/output (IO) mode operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-next-io-mode (current-io-mode)
  "Returns the IO mode opposite to the CURRENT-IO-MODE."
  (declare (type io-mode current-io-mode))
  (the io-mode
    (case current-io-mode
      (:number-mode :character-mode)
      (:character-mode :number-mode)
      (otherwise
        (error "No valid IO mode: ~s." current-io-mode)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of program.                                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Program
  (:constructor make-program
    (command-list
      &aux (commands
             (coerce command-list
               '(simple-array command (*)))))))
  "The ``Program'' class maintains an indexed sequence of
   \"Make me blush\" commands in a form convenient for their traversal
   and interpretation."
  (commands (error "Missing commands.")
            :type      (simple-array command (*))
            :read-only T))

;;; -------------------------------------------------------

(defun valid-command-index-p (program index)
  "Determines whether the INDEX constitutes a valid command position
   designator into the PROGRAM, returning on confirmation a ``boolean''
   value of ``T'', otherwise ``NIL''."
  (declare (type Program program))
  (declare (type fixnum  index))
  (the boolean
    (not (null
      (array-in-bounds-p
        (program-commands program)
        index)))))

;;; -------------------------------------------------------

(defun get-command-at (program index)
  "Returns the command in the PROGRAM at the zero-based INDEX, or
   signals an error of an unspecified type upon the position
   designator's transgression of the admissible bounds."
  (declare (type Program program))
  (declare (type fixnum  index))
  (the command
    (if (valid-command-index-p program index)
      (aref (program-commands program) index)
      (error "The index ~d does not designate a valid command. ~
              Admissible indices span the range [0, ~d]."
        index
        (1- (length (program-commands program)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of conditions.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Parser-Error (simple-error)
  ()
  (:documentation
    "The ``Parser-Error'' condition type serves to signal an anomalous
     situation ensuing from a failed parsing attempt exercised on a
     piece of \"Make me blush\" source code."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse state.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-State
  (:constructor make-initial-parse-state
    (source
     &aux (cursor 0)
          (token  (when (array-in-bounds-p source cursor)
                    (char source cursor)))))
  (:constructor advance-parse-state
    (current-state
     &aux (source (parse-state-source current-state))
          (cursor (1+ (parse-state-cursor current-state)))
          (token  (when (array-in-bounds-p source cursor)
                    (char source cursor))))))
  "The ``Parse-State'' class applies itself to the encapsulation of the
   entire parsing activity's progress, amplecting the piece of
   \"Make me blush\" source code to evaluate, the position represented
   by this advancement point, and the character located at this index."
  (source (error "Missing source.")
          :type      string
          :read-only T)
  (cursor 0
          :type      fixnum
          :read-only T)
  (token  NIL
          :type      (or null character)
          :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parse result.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parse-Result
  (:constructor make-parse-result (succeeded-p state output)))
  "The ``Parse-Result'' class assumes the wike of encapsulating a
   ``Parser'''s response to the request of processing a ``Parse-State'',
   encompassing a success or faiilure flag, the resulting state, either
   paregal to the probed instance or forming an chevisance therefrom,
   and the output, the same usually determines the parser's contribution
   to the entire parsing stage's produce."
  (succeeded-p (error "Missing success flag.")
               :type      boolean
               :read-only T)
  (state       (error "Missing state.")
               :type      Parse-State
               :read-only T)
  (output      (error "Missing output.")
               :type      T
               :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of parser.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Parser
  (:constructor make-parser (processor)))
  "The ``Parser'' class, representing either a parser or a combinator,
   applies itself to the procession of a ``Parse-State'' in order to
   respond with a ``Parse-Result'' that entails a success/failure flag,
   a progress designator in the form of a parse state, and the parser's
   contribution to the entire parsing stage, delivered as its output."
  (processor (error "Missing processor.")
             :type      parser-processor
             :read-only T))

;;; -------------------------------------------------------

(defun apply-parser (parser state)
  "Applies the PARSER to the parse STATE and returns a ``Parse-Result''
   representative of its success or failure, encompassing the ensuing
   ``Parse-State'' and its output, the latter the contribution to the
   overall parsing process result."
  (declare (type Parser      parser))
  (declare (type Parse-State state))
  (the Parse-Result
    (funcall (parser-processor parser) state)))

;;; -------------------------------------------------------

(defmacro build-parser ((state-variable) &body body)
  "Accoutres a convenience mechanism for the definition of a ``Parser''
   invoking the ``make-parser'' constructor with an anonymous function
   whose ``Parse-State'' parameter is norned by the STATE-VARIABLE, and
   whose actions comprehend the BODY, returning the desinent BODY form's
   evaluated results, which is expected to produce in its primary value
   a ``Parse-Result'' object."
  `(the Parser
     (make-parser
       #'(lambda (,state-variable)
           (declare (type Parse-State ,state-variable))
           (declare (ignorable        ,state-variable))
           ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of generic parsers and combinators.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun probe-token (predicate)
  "Returns a new parser which matches if the PREDICATE, applied to its
   input parse STATE's token, returns a generalized Boolean \"true\"
   value, otherwise failing, upon success returning in its parse result
   the probed token."
  (declare (type (function ((or null character)) *) predicate))
  (the Parser
    (build-parser (state)
      (let ((probed-token (parse-state-token state)))
        (declare (type (or null character) probed-token))
        (the Parse-Result
          (if (funcall predicate probed-token)
            (make-parse-result T
              (advance-parse-state state)
              probed-token)
            (make-parse-result NIL state probed-token)))))))

;;; -------------------------------------------------------

(defun bind-parser (antecedent consequent-generator)
  "Returns a new parser which implements a monadic binding, matching if
   the ANTECEDENT parser matches, as well as the consequent parser,
   produced by invoking the CONSEQUENT-GENERATOR callback on the
   successful ANTECEDENT result's output, returning the consequent
   parser's result yielded by an invocation on the ANTECEDENT result's
   parse state."
  (declare (type Parser                antecedent))
  (declare (type (function (*) Parser) consequent-generator))
  (the Parser
    (build-parser (state)
      (let ((antecedent-result (apply-parser antecedent state)))
        (declare (type Parse-Result antecedent-result))
        (the Parse-Result
          (if (parse-result-succeeded-p antecedent-result)
            (apply-parser
              ;; Obtain the consequent parser ...
              (funcall consequent-generator
                (parse-result-output antecedent-result))
              ;; ... and apply it to the antecedent result's state.
              (parse-result-state antecedent-result))
            (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defmacro bind-let ((antecedent-output-variable antecedent)
                    &body body)
  "Returns a new parser which implements a monadic binding by binding
   the ANTECEDENT result's output, if successful, to the
   ANTECEDENT-OUTPUT-VARIABLE, and executing the BODY forms granting
   access to the same, expecting the desinent evaluated form to produce
   a ``Parser'' whose result, applied to the ANTECEDENT result's state,
   is returned."
  `(bind-parser ,antecedent
     #'(lambda (,antecedent-output-variable)
         (declare (type T    ,antecedent-output-variable))
         (declare (ignorable ,antecedent-output-variable))
         ,@body)))

;;; -------------------------------------------------------

(defun return-output (output)
  "Returns a new parser which always succeeds, returning in its result
   the OUTPUT."
  (declare (type T output))
  (the Parser
    (build-parser (state)
      (make-parse-result T state output))))

;;; -------------------------------------------------------

(defun chain-of (&rest parsers)
  "Returns a new parser which matches if all of its PARSER succeed in
   this exact arrangement, on confirmation returning the desinent
   parser's output in its result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for parser
            of-type Parser
            in      parsers
          
          for result
            of-type Parse-Result
            =       (apply-parser parser new-state)
          
          unless (parse-result-succeeded-p result) do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return result))))))

;;; -------------------------------------------------------

(defun sequence-of (&rest parsers)
  "Returns a new parser which matches if all of its PARSERS succeed in
   this exact arrangement, on success returning a list of the collected
   parser outputs in the correct order in its result."
  (declare (type parser-list parsers))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for parser
            of-type Parser
            in      parsers
          
          for result
            of-type Parse-Result
            =       (apply-parser parser new-state)
          
          if (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          else do
            (return
              (make-parse-result NIL state NIL))
          
          finally
            (return
              (make-parse-result T
                (parse-result-state result)
                outputs)))))))

;;; -------------------------------------------------------

(defun any-of (&rest choices)
  "Returns a new parser which matches if any of its CHOICES, probed in
   this exact order, succeeds, on confirmation returning the first
   eligible choice's output in its result."
  (declare (type parser-list choices))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for choice of-type Parser       in choices
          for result of-type Parse-Result = (apply-parser choice state)
          
          when (parse-result-succeeded-p result) do
            (return result)
          
          finally
            (return
              (make-parse-result NIL state NIL)))))))

;;; -------------------------------------------------------

(defun one-or-more-times (parser)
  "Returns a new parser which matches if the specifier PARSER succeeds
   one or more times in immediate succession, on confirmation returning
   in its parse result's output a list of the PARSER's outputs in their
   order of encounter."
  (declare (type Parser parser))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for result
            of-type Parse-Result
            =       (apply-parser parser new-state)
          
          while (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          
          finally
            (return
              (if outputs
                (make-parse-result T   new-state outputs)
                (make-parse-result NIL state     NIL))))))))

;;; -------------------------------------------------------

(defun zero-or-more-times (parser)
  "Returns a new parser which always matches, probing the specified
   PARSER zero or more times in succession, upon each confirmation
   gathering the output, and finally returns in its own parse result's
   output a list of the gleaned data in the encountered ordonnance."
  (declare (type Parser parser))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for result
            of-type Parse-Result
            =       (apply-parser parser new-state)
          
          while (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun all-separated-by (parsers separator)
  "Returns a new parser which matches if all of the specified PARSERS,
   in this exact order, succeed, with each twissel's intermede filled by
   exactly one instance of the SEPARATOR, on confirmation returning in
   the new parser result's output a list of the collected PARSERS
   outputs in the order of their encounter."
  (declare (type parser-list parsers))
  (declare (type Parser      separator))
  (flet ((apply-separated-element (current-parser current-state)
          "Invokes a new ``chain-of'' parser, composed of an instance of
           the SEPARATOR followed by the CURRENT-PARSER, with the
           CURRENT-STATE, and returns the thus generated
           ``Parse-Result''."
          (declare (type Parser      current-parser))
          (declare (type Parse-State current-state))
          (the Parse-Result
            (apply-parser
              (chain-of separator current-parser)
              current-state))))
    (the Parser
      (build-parser (state)
        (the Parse-Result
          (loop
            for new-state
              of-type Parse-State
              =       state
              then    (parse-result-state result)
            
            for parser
              of-type Parser
              in      parsers
            
            for result of-type Parse-Result
              =    (apply-parser            parser new-state)
              then (apply-separated-element parser new-state)
            
            if (parse-result-succeeded-p result)
              collect (parse-result-output result)
              into    outputs
            else do
              (return
                (make-parse-result NIL state NIL))
            
            finally
              (return
                (make-parse-result T new-state outputs))))))))

;;; -------------------------------------------------------

(defun zero-or-more-separated-by (parser separator)
  "Returns a new parser which always succeedes, matching zero or more
   instances of the specified PARSER, each two consecutive invocations
   segregated by the SEPARATOR, returning in its parse result's output
   a list of the gathered PARSER outputs in the order of their
   encounter."
  (declare (type Parser parser))
  (declare (type Parser separator))
  (the Parser
    (build-parser (state)
      (the Parse-Result
        (loop
          for new-state
            of-type Parse-State
            =       state
            then    (parse-result-state result)
          
          for result
            of-type Parse-Result
            =       (apply-parser parser new-state)
            then    (apply-parser
                      (chain-of separator parser)
                      new-state)
          
          while (parse-result-succeeded-p result)
            collect (parse-result-output result)
            into    outputs
          
          finally
            (return
              (make-parse-result T new-state outputs)))))))

;;; -------------------------------------------------------

(defun return-for (antecedent consequent-output)
  "Returns a new parser which matches if the ANTECEDENT parser succeeds,
   on confirmation returning in its parse result's output the
   CONSEQUENT-OUTPUT."
  (declare (type Parser antecedent))
  (declare (type T      consequent-output))
  (the Parser
    (chain-of antecedent
      (return-output consequent-output))))

;;; -------------------------------------------------------

(defun fail (datum &rest arguments)
  "Returns a new parser which always fails, signaling an error whose
   specification concords with that of the ``error'' function, composed
   of the DATUM and the ARGUMENTS."
  (the Parser
    (build-parser (state)
      (apply #'error datum arguments))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of specialized parsers and combinators.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun end-of-program ()
  "Returns a new parser which matches if its input parse state's token
   represents the ``NIL'' value, that is, designating the underlying
   \"Make me blush\" source code's exhaustion, or \"end of file\"
   (EOF)."
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type (or null character) token))
          (null token)))))

;;; -------------------------------------------------------

(defun character-of (expected-character)
  "Returns a new parser which matches if its input parse state's token
   equals the EXPECTED-CHARACTER, on confirmation including in its
   response the probed token."
  (declare (type character expected-character))
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type (or null character) token))
          (and token
               (char= token expected-character))))))

;;; -------------------------------------------------------

(defun whitespace ()
  "Returns a new parser which matches if its input parse state's token
   represents a whitespace character, on confirmation including in its
   response the probed token."
  (the Parser
    (probe-token
      #'(lambda (token)
          (declare (type (or null character) token))
          (and token
               (member token '(#\Newline #\Space #\Tab)
                 :test #'char=))))))

;;; -------------------------------------------------------

(defun separator ()
  "Returns a new parser which matches one or more accolent whitespace
   tokens, on confirmation returning in its response the ``T''
   sentinel."
  (the Parser
    (chain-of
      (one-or-more-times
        (whitespace))
      (return-output T))))

;;; -------------------------------------------------------

(defun padding ()
  "Returns a new parser which always succeeds by matching zero or more
   accolent whitespaces, returning in its parse result's output the
   Boolean sentinel ``T''."
  (the Parser
    (return-for
      (zero-or-more-times
        (whitespace))
      T)))

;;; -------------------------------------------------------

(defun word-of (expected-word)
  "Returns a new parser which matches if the subsequent characters
   replicate the EXPECTED-WORD, on confirmation returning in its
   result's output the Boolean truth value ``T''."
  (declare (type string expected-word))
  (the Parser
    (chain-of
      (apply #'sequence-of
        (map 'list #'character-of expected-word))
      (return-output T))))

;;; -------------------------------------------------------

(defun words (&rest expected-words)
  "Returns a new parser which succeeds if the consecutive parse state
   tokens replicate the EXPECTED-WORDS in their specified order, on
   confirmation returning in its parse result's output the Boolean
   sentinel ``T''."
  (declare (type (list-of string) expected-words))
  (the Parser
    (return-for
      (all-separated-by
        (mapcar #'word-of expected-words)
        (separator))
      T)))

;;; -------------------------------------------------------

(defun one-command ()
  "Returns a new parser which matches an aefauld \"Make me blush\"
   command identifier, on confirmation returning in its parse result's
   output the corresponding ``command'' object."
  (the Parser
    (any-of
      (return-for (words "uwu")                   :move-left)
      (return-for (words "rawr")                  :move-right)
      (return-for (words "lets" "date" "at")      :increment)
      (return-for (words "lets" "kiss" "at")      :decrement)
      (return-for (words "wanna" "marry?")        :input)
      (return-for (words "you" "know")            :output)
      (return-for (words "how" "bout" "you?")     :toggle-io-mode)
      (return-for (words "your" "gay" "because:") :jump-forward)
      (return-for (words "and" "thats" "it")      :jump-back))))

;;; -------------------------------------------------------

(defun zero-or-more-commands ()
  "Returns a new parser which matches zero or more \"Make me blush\"
   commands, each twain demarcated by one or more whitespaces, returning
   in its parse result's output a list of the gathered ``command''
   representatives."
  (the Parser
    (zero-or-more-separated-by
      (one-command)
      (separator))))

;;; -------------------------------------------------------

(defun parse-program ()
  "Returns a new parser which attempts to parse a piece of
   \"Make me blush\" source code, on success returning in its parse
   result's output a ``Program'' encapsulation of its extracted
   commands."
  (the Parser
    (any-of
      (chain-of
        (padding)
        (bind-let (commands (zero-or-more-commands))
          (declare (type (list-of command) commands))
          (chain-of
            (padding)
            (end-of-program)
            (return-output
              (make-program commands)))))
      (fail 'Parser-Error))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of jump table.                                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Jump-Table
  (:constructor make-empty-jump-table ()))
  "The ``Jump-Table'' class serves in the association of jump forward
   and back points in an executable \"Make me blush\" program by
   adminiculum of their positions."
  (connections (make-hash-table :test #'eql)
               :type (hash-table-of fixnum fixnum)))

;;; -------------------------------------------------------

(defun connect-jump-points (jump-table first-endpoint second-endpoint)
  "Associates the FIRST-ENDPOINT and the SECOND-ENDPOINT in the
   JUMP-TABLE and returns no value."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     first-endpoint))
  (declare (type fixnum     second-endpoint))
  (setf (gethash first-endpoint
          (jump-table-connections jump-table))
        second-endpoint)
  (setf (gethash second-endpoint
          (jump-table-connections jump-table))
        first-endpoint)
  (values))

;;; -------------------------------------------------------

(defun get-jump-target (jump-table source)
  "Returns the jump endpoint location associated with the SOURCE
   position in the JUMP-TABLE, or signals an error of an unspecified
   type upon its disrespondency."
  (declare (type Jump-Table jump-table))
  (declare (type fixnum     source))
  (the fixnum
    (or (gethash source
          (jump-table-connections jump-table))
        (error "No jump target associated with the position ~d."
          source))))

;;; -------------------------------------------------------

(defun supputate-jump-table (program)
  "Generates and returns for the \"Make me blush\" PROGRAM the jump
   table which connects the forward jump commands and back jump
   equivalents by mediation of their positions in the PROGRAM."
  (let ((jump-table          (make-empty-jump-table))
        (forward-jump-points NIL))
    (declare (type Jump-Table       jump-table))
    (declare (type (list-of fixnum) forward-jump-points))
    (loop
      for command  of-type command across (program-commands program)
      for position of-type fixnum  from 0 by 1
      
      if (eq command :jump-forward) do
        (push position forward-jump-points)
      else if (eq command :jump-back) do
        (if forward-jump-points
          (let ((start-point (pop forward-jump-points))
                (end-point   position))
            (declare (type fixnum start-point))
            (declare (type fixnum end-point))
            (connect-jump-points jump-table start-point end-point))
          (error "Unmatched back jump point at position ~d."
            position))
      end
      finally
        (when forward-jump-points
          (error "Unmatched forward jump points at ~
                  positions ~{~d~^, ~}."
            forward-jump-points)))
    (the Jump-Table jump-table)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tape.                                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Tape
  (:constructor make-tape ()))
  "The ``Tape'' class realizes a bilaterally infinite tape composed of
   unsigned byte-valued cells, operated upon by a mobile cell pointer,
   the same designates at any instant the currently active cell, the
   sole instance amenable to perquisitions and modifications."
  (cells   (make-hash-table :test #'eql)
           :type (hash-table-of integer octet))
  (pointer 0
           :type integer))

;;; -------------------------------------------------------

(defun move-cell-pointer-right (tape)
  "Translates the TAPE's cell pointer one step to the right and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun move-cell-pointer-left (tape)
  "Translates the TAPE's cell pointer one step to the left and returns
   no value."
  (declare (type Tape tape))
  (incf (tape-pointer tape))
  (values))

;;; -------------------------------------------------------

(defun current-cell (tape)
  "Returns the byte stored in the TAPE's current cell."
  (declare (type Tape tape))
  (the octet
    (gethash (tape-pointer tape) (tape-cells tape) 0)))

;;; -------------------------------------------------------

(defun (setf current-cell) (new-value tape)
  "Stores the NEW-VALUE in the TAPE's current cell, contingently
   preceded by a wrapping around in order to abide to the unsigned byte
   range [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type Tape    tape))
  (setf (gethash (tape-pointer tape) (tape-cells tape) 0)
        (mod new-value 256))
  (values))

;;; -------------------------------------------------------

(defun increment-current-cell (tape)
  "Increments the TAPE's current cell value by one, contingently
   wrapping around at the upper march upon its transcendence in order to
   ascertain the abidance of the unsigned byte range [0, 255], and
   returns no value."
  (declare (type Tape tape))
  (incf (current-cell tape))
  (values))

;;; -------------------------------------------------------

(defun decrement-current-cell (tape)
  "Decrements the TAPE's current cell value by one, contingently
   wrapping around at the lower march upon its transcendence in order to
   ascertain the abidance of the unsigned byte range [0, 255], and
   returns no value."
  (declare (type Tape tape))
  (decf (current-cell tape))
  (values))

;;; -------------------------------------------------------

(defun current-cell-zero-p (tape)
  "Determines whether the TAPE's current cell contains the value zero
   (0), returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (the boolean
    (not (null
      (zerop (current-cell tape))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Interpreter
  (:constructor make-interpreter
    (program
     &aux (jump-table (supputate-jump-table program)))))
  "The ``Interpreter'' class is apportioned the bailiwick of
   accompassing actual utility to an executable \"Make me blush\"
   program."
  (program    (error "Missing program.")
              :type      Program
              :read-only T)
  (ip         0
              :type      fixnum)
  (jump-table (error "Missing jump table.")
              :type      Jump-Table
              :read-only T)
  (io-mode    :number-mode
              :type      io-mode)
  (tape       (make-tape)
              :type      Tape))

;;; -------------------------------------------------------

(defun program-exhausted-p (interpreter)
  "Determines whether the INTERPRETER's program has been processed in
   its entirety, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (not
      (valid-command-index-p
        (interpreter-program interpreter)
        (interpreter-ip      interpreter)))))

;;; -------------------------------------------------------

(defun get-current-command (interpreter)
  "Returns the command at the INTERPRETER's instruction pointer (IP)
   location, or signal an error of an unspecified type upon its
   transgression of the underlying program's boundaries."
  (declare (type Interpreter interpreter))
  (the command
    (get-command-at
      (interpreter-program interpreter)
      (interpreter-ip      interpreter))))

;;; -------------------------------------------------------

(defun advance-program (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   command in its program, if possible, and returns no value."
  (declare (type Interpreter interpreter))
  (unless (program-exhausted-p interpreter)
    (incf (interpreter-ip interpreter)))
  (values))

;;; -------------------------------------------------------

(defun jump-to-opposite-boundary (interpreter)
  "Expecting the INTERPRETER's temporary commorancy to assume a jump
   forward or back command, relocates its instruction pointer (IP) to
   the obverse bourne and returns no value, otherwise, upon the current
   location's disrespondency, signals an error of an unspecified type."
  (declare (type Interpreter interpreter))
  (setf (interpreter-ip interpreter)
    (get-jump-target
      (interpreter-jump-table interpreter)
      (interpreter-ip         interpreter)))
  (values))

;;; -------------------------------------------------------

(defun toggle-io-mode (interpreter)
  "Toggles the INTERPRETER's input/output mode and returns no value."
  (declare (type Interpreter interpreter))
  (setf (interpreter-io-mode interpreter)
        (get-next-io-mode
          (interpreter-io-mode interpreter)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-command (interpreter command)
  (:documentation
    "Processes the COMMAND in the INTERPRETER's context and returns no
     value."))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :move-right)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (move-cell-pointer-right
    (interpreter-tape interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :move-left)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (move-cell-pointer-left
    (interpreter-tape interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :increment)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (increment-current-cell
    (interpreter-tape interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :decrement)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (decrement-current-cell
    (interpreter-tape interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :input)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (case (interpreter-io-mode interpreter)
    (:number-mode
      (format T "~&Please input an integer: ")
      (finish-output)
      (setf (current-cell
              (interpreter-tape interpreter))
            (parse-integer
              (read-line)))
      (clear-input))
    (:character-mode
      (format T "~&Please input a character: ")
      (finish-output)
      (setf (current-cell
              (interpreter-tape interpreter))
            (char-code
              (read-char)))
      (clear-input))
    (otherwise
      (error "Invalid IO mode: ~s."
        (interpreter-io-mode interpreter))))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :output)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (case (interpreter-io-mode interpreter)
    (:number-mode
      (format T " ~d"
        (current-cell
          (interpreter-tape interpreter))))
    (:character-mode
      (format T "~c"
        (code-char
          (current-cell
            (interpreter-tape interpreter)))))
    (otherwise
      (error "Invalid IO mode: ~s."
        (interpreter-io-mode interpreter))))
  (finish-output)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :toggle-io-mode)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (toggle-io-mode interpreter)
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :jump-forward)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (when (current-cell-zero-p
          (interpreter-tape interpreter))
    (jump-to-opposite-boundary interpreter))
  (values))

;;; -------------------------------------------------------

(defmethod process-command ((interpreter Interpreter)
                            (command     (eql :jump-back)))
  (declare (type Interpreter interpreter))
  (declare (type command     command))
  (declare (ignore           command))
  (unless (current-cell-zero-p
            (interpreter-tape interpreter))
    (jump-to-opposite-boundary interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-program (interpreter)
  (declare (type Interpreter interpreter))
  (loop until (program-exhausted-p interpreter) do
    (process-command interpreter
      (get-current-command interpreter))
    (advance-program interpreter))
  (values))

;;; -------------------------------------------------------

(defun interpret-Make-me-blush (code)
  "Interprets the piece of \"Make me blush\" source CODE and returns no
   value."
  (interpret-program
    (make-interpreter
      (parse-result-output
        (apply-parser
          (parse-program)
          (make-initial-parse-state code)))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeating numeric cat program which terminates on a user input of
;; zero (0).
(interpret-Make-me-blush "lets date at your gay because: wanna marry? you know and thats it")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on an input of the
;; "null character".
(interpret-Make-me-blush
  "how bout you?
   wanna marry?
   your gay because:
   you know
   wanna marry?
   and thats it")

;;; -------------------------------------------------------

;; ASCII loop: Prints the characters whose ASCII codes occupy closed
;; range [1, 255] in an infinite loop.
(interpret-Make-me-blush
  "how bout you? lets date at your gay because: your gay because: you know lets date at and thats it lets date at and thats it")

;;; -------------------------------------------------------

;; Truth-machine which exploits the numeric input/output mode.
(interpret-Make-me-blush
  "wanna marry?
   your gay because: you know and thats it
   you know")
