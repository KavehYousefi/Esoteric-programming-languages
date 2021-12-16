;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements a simple interpreter for the esoteric
;; programming language "BWTFN", invented by the Esolang user
;; "FlyHamsterPaul".
;; 
;; Concept
;; =======
;; BWTFN represents a joke language specialized on writing text to
;; one or more variables, a single instance of the same can be receptive
;; at one time, while switching between them is permitted. The thus
;; constructed string variables can be printed to the standard output.
;; 
;; == BWTFN: A Joke Language ==
;; The etiology of BWTFN's naming is traced to the phrase "Because Why
;; The Fuck Not", an acronym that reverberates its autotelic conception.
;; This specimen can be subsumed into joke languages, a branch of
;; esoteric programming languages designed with no pragmatism in the
;; mind, instead bartering teleology for amusement.
;; 
;; == A SCANT SET OF CAPABILITIES EXISTS ==
;; BWTFN represents an output-only language, severely impaired in a
;; multitude of other aspects as well, including the lack of arithmetics
;; or the manipulation of any datum except for its variables, and the
;; dismissal of control flow constructs, such as conditional expressions
;; and iterations. This peculiarity does not enlist as a blemish, as its
;; author admits with no ambivalence in diction the language's
;; intention.
;; 
;; == VARIABLES CONSTITUTE THE CYNOSURE OF OPERATIONS ==
;; The focus of a BWTFN program's functioning resides in the definition
;; of variables, the assignment of characters to the same, and their
;; ultimate printing to the standard output. The faculties are hence so
;; meager as to solely permit text display. Multiple variables might be
;; declared --- or "opened" --- coevally, while merely a single instance
;; at one time is amenable to the reception of character data. The
;; deactivation, or "closing", of a variable does not coerce its
;; destruction, and its reactivation and reuse remains an option at any
;; point.
;; 
;; == A TERMINOLOGY OF VARIABLES ==
;; The interaction of variables with the program inflicts some
;; intricacies into their management's comprehension, which the
;; following diorisms shall help in elucidation and discrimination:
;; 
;;   active
;;     A variable is active if it is the currently operated on entity.
;;     This bears ponderance especially in the "add" command invocation
;;     with its dependence on the existence and identity of the variable
;;     in action. At one time exactly zero or one variable may be
;;     active, and only an OPEN variable may be such. If an active
;;     variable is CLOSED, it becomes DEACTIVATED; analogously, if a
;;     new variable is opened while another one is active, the latter
;;     becomes deactivated --- but not closed --- and the former is
;;     marked as active.
;;   
;;   closed
;;     A variable whose name has been submitted to the "close" command
;;     is declared closed. If having hitherto been the ACTIVE variable,
;;     it becomes deactivated. A closed variable cannot be declared
;;     ACTIVE. It can be reopened using the "open" command, and if done
;;     so, immediately transforms into the ACTIVE state.
;;   
;;   open
;;     A variable is open if it has been just created using the "open"
;;     command or has been created before and its name has been invoked
;;     with the same command. In any case, such variable automatically
;;     is declared ACTIVE, deactivating a previously active one, if
;;     such is present. Only an open variable can be the program's
;;     active variable.
;; 
;; For a disquisition attending more meticulously to the instructions'
;; perspective on this subject, please consult the "Instructions"
;; section below.
;; 
;; 
;; Architecture
;; ============
;; BWTFN's foundation does not explicitly mandate a particular
;; architecture, with the only objects in currency pertaining to
;; variables, each such storing a dynamic string. Despite the
;; simplicity, the placeholder management imposes some requisites.
;; 
;; The architecture applied to this kind of castaldy is encumbered by a
;; number of requirements that shall now be postulated:
;;   
;;   (1) Extant variables must be retrievable by their name.
;;   (2) Closed variables must be marked and memorized as deactivated.
;;   (3) The order of variable opening (creation and/or activation) and
;;       closing (deactivation) must be traced and handled.
;; 
;; It is tenable to adduce the conjecture that a suitable data structure
;; must perforce concord with an associative trait, a capacity of
;; mapping to a name the pertinent variable data, including its string
;; content and state anenst being open or closed.
;; 
;; A second but no paraveil component of its haecceity, the storage
;; must be amenable to attendance of the order of variable openings and
;; closures, forcause the currently active instance bears significance
;; to the operation of the "add" command. Opening a variable must, at
;; least conceptually, locate or relocate it onto a paravaunt position,
;; while its closure permits the previously topmost instance to again
;; be rendered active. This notion equals the scoping of variables in
;; many programming languages, especially such that homologate the reuse
;; of an identifier's name, with the local interpretation prevailing
;; over those in more distant scopes.
;; 
;; Foreby, a consanguinity of this, and as incidentally related, the
;; management of an open variable with preeminent agency as the active
;; one must find incorporation into the data structure.
;; 
;; 
;; Data Types
;; ==========
;; BWTFN incorporates two elementary data types: characters for the
;; storage of content and integers as ASCII code designators and
;; repetition counts.
;; 
;; == CHARACTERS OCCUPY A PARAMOUNT RANK ==
;; ASCII characters constitute the capital elements, being actually the
;; only content embodied by variables. Their conveyance, however, is
;; accomplished by mediation of numeric character codes.
;; 
;; == INTEGERS SPECIFIY CHARACTER CODES AND REPETITIONS ==
;; A subordinate in rank, integer numbers find employment in two
;; circumstances: Imprimis, they specify the characters to store in a
;; variable in the form of an ASCII code; secondary, multitudes are
;; expressed in this type. The former utilization confines the valid
;; range to the standard's [0, 255]. In the latter circumstances the
;; gamut is restricted only at the minimum with zero (0), while no
;; upper bound prevails, yielding a range of [0, +infinity].
;; 
;; 
;; Syntax
;; ======
;; The language's syntax conforms to the ASCII character set, with
;; letters assigned a capital significance, while digits act in the
;; agency of character codes and repetitions counts. The language is
;; case-sensitive but tolerant with respect to whitespaces.
;; 
;; == INSTRUCTIONS ARE WORDS ==
;; Each command encompasses a sequence of lowercase letters.
;; 
;; == VARIABLES ARE DESCRIBED BY LETTERS ==
;; A variable name is composed of one or more majuscular or minuscular
;; letters.
;; 
;; == ASCII CODES ==
;; The variable content is described by a sequence of letters, conveyed
;; to the program in the form of its ASCII character codes, each such
;; an integer number in the range [0, 255]; which is an equivalent of
;; one to three digits.
;; 
;; == REPETITIONS ==
;; Some instructions permit an additional specification of the tally of
;; repetitions immediately following the only argument. This item is
;; introduced by an asterisk ('*'), followed by a non-negative integer
;; number destitute of an upper bound. Whitespaces are tolerated in the
;; interstices.
;; 
;; == GRAMMAR ==
;; The syntax of the language can be described in the Extended
;; Backus-Naur Form (EBNF) notation as such:
;; 
;;   program      := { command } ;
;;   command      := "add"    , asciiCode    , [ cardinality ]
;;                |  "close"  , variableName
;;                |  "open"   , variableName
;;                |  "out"    , variableName , [ cardinality ]
;;                |  "output" , variableName , [ cardinality ] ;
;;   cardinality  := "*" , digit , { digit } ;
;;   variableName := letter , { letter } ;
;;   asciiCode    := digit , [ digit , [ digit ] ] ;
;;   letter       := "A" | ... | "Z" | "a" | ... | "z" ;
;;   digit        := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
;; 
;; 
;; Instructions
;; ============
;; BWTFN's instruction set is described a quintuple of specimens, all
;; of which appertaining to the indagation and manipulation of
;; variables.
;; 
;; == DESIGN ==
;; A dependence upon a single mandatory argument establishes the
;; vinculum among all instructions, whereas a subset embraces an
;; optional specification, determining the tally of repetitions to
;; administer to the effect.
;; 
;; == OVERVIEW ==
;; The essence of all five instructions shall be elucidated in a concise
;; manner below:
;; 
;;   Command | Description
;;   --------+---------------------------------------------------------
;;    add    | Syntax:
;;           |   add {asciiCode}
;;           |   add {asciiCode} * {repetitions}
;;           | Description:
;;           |   Appends to the active variable's value the ASCII
;;           |   character corresponding to the numeric "asciiCode".
;;           |   If the "repetitions" are specified, the character is
;;           |   appended a "repetitions" tally of times.
;;   ..................................................................
;;    close  | Syntax:
;;           |   close {variableName}
;;           | Description:
;;           |   Closes the variable specified by the "variableName",
;;           |   concomitantly ascertaining that is becomes inactive.
;;   ..................................................................
;;    open   | Syntax:
;;           |   open {variableName}
;;           | Description:
;;           |   Opens the variable specified by the "variableName",
;;           |   concomitantly ascertaining that is becomes the active
;;           |   variable.
;;   ..................................................................
;;    out    | Syntax:
;;           |   out {variableName}
;;           |   out {variableName} * {repetitions}
;;           | Description:
;;           |   Prints to the standard output the value of the
;;           |   variable specified by the "variableName", not followed
;;           |   by a linebreak.
;;           |   If the "repetitions" are specified, the output is
;;           |   printed a "repetitions" tally of times.
;;   ..................................................................
;;    output | Syntax:
;;           |   output {variableName}
;;           |   output {variableName} * {repetitions}
;;           | Description:
;;           |   Prints to the standard output the value of the
;;           |   variable specified by the "variableName", followed by
;;           |   a linebreak.
;;           |   If the "repetitions" are specified, the output is
;;           |   printed a "repetitions" tally of times, each iteration
;;           |   concluding with a linebreak.
;; 
;; The following sections are dedicated to the commands' elucidations
;; with particular indagation into their vinculum to open and active
;; variables.
;; 
;; == ADD ==
;; The "add" command appends a character, potentially multiple times, to
;; the content of the currently active variable.
;; 
;; Relationship with the variable management:
;;   - If no active variable exists, an error is thrown.
;; 
;; == CLOSE ==
;; The "close" command closes a variable, concomitantly deactivating it.
;; 
;; Relationship with the variable management:
;;   - If no variable with this name exists, an error is signaled.
;;   - If a variable with this name exists and is already closed, no
;;     effect is exercised.
;;   - If a variable with this name exists, is open but not active, the
;;     variable is closed.
;;   - If a variable with this name exists, is open and active, the
;;     variable is closed, relocated to the end of the variable deque,
;;     and the variable at the deque head becomes the active variable.
;;     If the deque comprises the just closed variable only, no active
;;     variable will be declared.
;; 
;; == OPEN ==
;; The "open" command creates a variable, if not extant, coevally
;; selecting it as the active one.
;; 
;; This command ensures that
;;    (a) a variable with the specified name exists,
;;    (b) this variable is open,
;;    (c) and this variable is the active variable.
;; 
;; Relationship with the variable management:
;;   - If no variable with this name exists yet, it is created,
;;     registered, and set as the active variable.
;;   - If a variable with this name exists, is open and is active, no
;;     effect is exercised.
;;   - If a variable with this name exists, is open but not active, it
;;     becomes the active variable.
;;   - If a variable with this name exists and is closed, it is opened
;;     and becomes the active variable.
;; 
;; == OUT ==
;; The "out" command prints the content of a variable to the standard
;; output, contingently multiple times, without appending a linebreak.
;; 
;; Relationship with the variable management:
;;   - If no variable with this name exists, an error is signaled.
;;   - The variable may be open or closed.
;; 
;; == OUTPUT ==
;; The "output" command prints the content of a variable to the standard
;; output, contingently multiple times, each time appending a linebreak.
;; 
;; Relationship with the variable management:
;;   - If no variable with this name exists, an error is signaled.
;;   - The variable may be open or closed.
;; 
;; 
;; Lacunae in the Specification
;; ============================
;; Being a project of utterly specialized scope, and a jocular specimen,
;; the reader may not be inflicted with awe about the briefness assumed
;; by the original specification. Proportional to this comprehension,
;; the emergence of questions accompanies one's expections. The coming
;; list shall install an excerpt, without postulation of plenitude.
;; 
;; == WHICH NAMING CONVENTIONS APPLY TO VAIABLES? ==
;; The only respondent faculty in the documentation anenst the variable
;; design is embodied in the examples. This scarcity expands into the
;; inquisitions about the valid format for variable names, which in the
;; text assume merely an alphabetic guise. As the BWTFN specification
;; might be extended in future revisions, with particular regard to its
;; characters that might accommodate apertures for infringement, only
;; letters shall be applicable in any position of the variable name.
;; 
;; == HOW MANY LINEBREAKS DOES "OUTPUT" PRINT? ==
;; The capacity of quantifying actions does not elude the "output"
;; instruction, empowering its effect to be exercised in arbitrary
;; numbers. Yet, an explicit statement about whether each invocation
;; appends a linebreak --- the caract distinguishing it from the "out"
;; command --- or merely the desinent one is wanting. It is has been
;; chosen to conclude every single iteration of a multiple call with a
;; newline character.
;; 
;; 
;; Implementation
;; ==============
;; Maugre its simplicity, an attribute of intricate value accompanies
;; the storage and management of variables.
;; 
;; == VARIABLES ==
;; Variables are represented by a dedicated class ``BWTFN-Variable'',
;; a composite of the identifying name, a flag determining its state as
;; open or closed, and, most importantly, a dynamic string intended to
;; receive the character content. All major operations --- opening,
;; closing, adding characters, and both variants of output --- are
;; affiliated with the responsibilities of this class, alluded by the
;; respective function denominations.
;; 
;; == VARIABLE MANAGEMENT ==
;; BWTFN's necessity in retention and modification of variable orders
;; anenst their opening and closing during the course of a program
;; eschews a plain mapping solution. A conflict issues from the
;; unordered hash table concept and the influence of the layout on the
;; variables. As a consectary, the salvatory for the same has been
;; chosen as a dequeue, manifesting in a simple list. The amenities
;; accommodated by this structure deliberately succumb into approval of
;; the inefficient nature levied on the implementation --- concretely
;; related, the time efficiency involved in querying a variable by its
;; name deviates to O(n).
;; 
;; == OPENING A VARIABLE PUSHES IT UNTO THE DEQUE ==
;; The dequeue concords with the order of variable openings in the
;; athwart airt: A recently opened individual is pushed to its top,
;; concomitantly declaring its agency as the active specimen. By this
;; structural simulacrum's avail the interactions of opening and closing
;; are eath traced: A variable located aboon another is guaranteed to
;; have been opened or reopened more recently than the occupant below.
;; 
;; == CLOSING A VARIABLE RELOCATES IT TO THE TAIL ==
;; Akin to the activation of variables in their adherence to the stack
;; nature's last-in-first-out (LIFO) principle, the overthwart
;; construction inherent in the deque, the queue's first-in-first-out
;; (FIFO) arrangement, is utilized in the handling of variable closures:
;; A variable having been closed is removed from its position in the
;; salvatory and appended to the end (tail) of the deque, thus obviating
;; a corruption of the opening hierarchy.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2021-12-09
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/BWTFN"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "BWTFN-Variable".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (BWTFN-Variable
  (:constructor make-variable (name))
  (:conc-name   variable-))
  "The ``BWTFN-Variable'' defines a named variable storing a dynamic
   string, capable of being opened and closed."
  (name    NIL :type string)
  (is-open NIL :type boolean)
  (value   (make-array 0 :element-type 'character
             :adjustable   T
             :fill-pointer 0)
           :type string))

;;; -------------------------------------------------------

(defun variable-add (variable ascii-code &optional (repetitions 0))
  "Appends to the VARIABLE's value the character associated with the
   ASCII-CODE a REPETITIONS number of types, and returns the modified
   VARIABLE."
  (declare (type BWTFN-Variable    variable))
  (declare (type (unsigned-byte 8) ascii-code))
  (declare (type (integer 0 *)     repetitions))
  (cond
    ((variable-is-open variable)
      (loop repeat repetitions do
        (vector-push-extend (code-char ascii-code)
          (variable-value variable))))
    (T
      (error "The variable ~s is closed and thus cannot respond to the ~
              'add' command."
        (variable-name variable))))
  (the BWTFN-Variable variable))

;;; -------------------------------------------------------

(defun variable-close (variable)
  "Closes the VARIABLE, returning the modified VARIABLE."
  (declare (type BWTFN-Variable variable))
  (cond
    ((variable-is-open variable)
      (setf (variable-is-open variable) NIL))
    (T
      (error "The variable ~s is already closed and thus cannot ~
              respond to the 'close' command."
        (variable-name variable))))
  (the BWTFN-Variable variable))

;;; -------------------------------------------------------

(defun variable-open (variable)
  "Opens the VARIABLE, returning the modified VARIABLE."
  (declare (type BWTFN-Variable variable))
  (setf (variable-is-open variable) T)
  (the BWTFN-Variable variable))

;;; -------------------------------------------------------

(defun variable-out (variable &optional (repetitions 0))
  "Prints the VARIABLE value a REPETITIONS number of times, without a
   succeeding linebreak, returning the VARIABLE itself."
  (declare (type BWTFN-Variable variable))
  (declare (type (integer 0 *)  repetitions))
  (loop repeat repetitions do
    (format T "~a" (variable-value variable)))
  (the BWTFN-Variable variable))

;;; -------------------------------------------------------

(defun variable-output (variable &optional (repetitions 0))
  "Prints the VARIABLE value a REPETITIONS number of times, each time
   succeeded by a linebreak, returning the VARIABLE itself."
  (declare (type BWTFN-Variable variable))
  (declare (type (integer 0 *)  repetitions))
  (loop repeat repetitions do
    (format T "~a~%" (variable-value variable)))
  (the BWTFN-Variable variable))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-BWTFN (code)
  "Interprets the BWTFN CODE and returns no value."
  (declare (type string code))
  (let ((position         0)
        (character        (char code 0))
        
        (variables        NIL)
        (current-variable NIL))
    (declare (type fixnum                   position))
    (declare (type (or null character)      character))
    (declare (type list                     variables))
    (declare (type (or null BWTFN-Variable) current-variable))
    
    (labels
        ((advance ()
          "Moves the POSITION to the next character in the CODE,
           updating the current CHARACTER in the process, and returning
           no value."
          (setf character
            (when (< position (1- (length code)))
              (char code (incf position))))
          (values))
         
         (whitespace-character-p (subject)
          "Checks whether the SUBJECT represents a whitespace character,
           returning on confirmation a ``boolean'' value of ``T'',
           otherwise ``NIL''."
          (declare (type character subject))
          (the boolean
            (not (null (member subject '(#\Space #\Tab) :test #'char=)))))
         
         (skip-whitespaces ()
          "Starting the current POSITION, skips zero or more
           whitespaces, setting the POSITION to the first non-whitespace
           character, and returning no value."
          (loop while (and character (whitespace-character-p character)) do
            (advance))
          (values))
         
         (read-command ()
          "Starting at the current POSITION, reads an identifier
           construed as a command name, moves the POSITION to the first
           non-letter character, and returns the name."
          (the string
            (with-output-to-string (identifier)
              (declare (type string-stream identifier))
              (loop while (and character (alpha-char-p character)) do
                (write-char character identifier)
                (advance)))))
         
         (read-variable ()
          "Starting at the current POSITION, reads and returns a
           variable name."
          (the string
            (with-output-to-string (identifier)
              (declare (type string-stream identifier))
              (loop while (and character (alpha-char-p character)) do
                (write-char character identifier)
                (advance)))))
         
         (read-number ()
          "Starting at the current POSITION, reads and returns a
           non-negative integer number."
          (the (integer 0 *)
            (parse-integer
              (with-output-to-string (digits)
                (declare (type string-stream digits))
                (loop while (and character (digit-char-p character)) do
                  (write-char character digits)
                  (advance))))))
         
         (expect-whitespace ()
          "Checks whether the current CHARACTER represents a whitespace,
           either advancing on confirmation or failing with an error."
          (unless (whitespace-character-p character)
            (error "Expected one or more whitespaces starting at ~
                    position ~d."
              position))
          (values))
         
         (expect-digit ()
          "Checks whether the current CHARACTER represents a decimal
           digit, either advancing on confirmation or failing with an
           error."
          (unless (digit-char-p character)
            (error "Expected one or more digits starting at position ~d."
              position))
          (values))
         
         (expect-character ()
          "Checks whether the current CHARACTER represents a letter,
           either advancing on confirmation or failing with an error."
          (unless (alpha-char-p character)
            (error "Expected on or more digits starting at position ~d."
              position))
          (values))
         
         (check-for-repetitions (&optional (default 1))
          "Checks whether the current CHARACTER introduces a repetition,
           on confirmation consuming and returning the tally, while
           setting the POSITION to the first character immediately
           following the detected number; otherwise resorting to
           the DEFAULT value."
          (declare (type T default))
          (cond
            ((and character (char= character #\*))
              (advance)
              (skip-whitespaces)
              (expect-digit)
              (the (integer 0 *) (read-number)))
            (T
              (the T default))))
         
         
         (get-variable (variable-name)
          "Returns the ``BWTFN-Variable'' with the VARIABLE-NAME or
           ``NIL'' if none such exists."
          (declare (type string variable-name))
          (the (or null BWTFN-Variable)
            (find variable-name variables
              :key  #'variable-name
              :test #'string=)))
         
         (register-variable (variable)
          "Adds the VARIABLE to the VARIABLES and returns no value."
          (declare (type BWTFN-Variable variable))
          (push variable variables)
          (values))
         
         (close-variable (variable-name)
          (declare (type string variable-name))
          (let ((variable-to-close (get-variable variable-name)))
            (declare (type (or null BWTFN-Variable) variable-to-close))
            (cond
              (variable-to-close
                (variable-close variable-to-close)
                (setf variables
                  (append
                    (delete variable-name variables
                      :key  #'variable-name
                      :test #'string=)
                    (list variable-to-close)))
                (setf current-variable
                  (find-if #'variable-is-open variables)))
              (T
                (error "No variable with the name ~s is registered, ~
                        thus it cannot be closed."
                  variable-name))))
          (values)))
      
      (loop do
        
        (cond
          ((null character)
            (loop-finish))
          
          ((whitespace-character-p character)
            (skip-whitespaces))
          
          ((char= character #\Newline)
            (advance))
          
          ((alpha-char-p character)
            (let ((command (read-command)))
              (declare (type string command))
              
              (cond
                ((string= command "add")
                  (cond
                    (current-variable
                      (expect-whitespace)
                      (skip-whitespaces)
                      (expect-digit)
                      
                      (let ((ascii-code (read-number)))
                        (declare (type (integer 0 *) ascii-code))
                        (skip-whitespaces)
                        (variable-add current-variable ascii-code
                          (check-for-repetitions 1))))
                    (T
                      (error "No open variable exists for adding to it."))))
                
                ((string= command "close")
                  (expect-whitespace)
                  (skip-whitespaces)
                  (expect-character)
                  
                  (let ((variable-name (read-variable)))
                    (declare (type string variable-name))
                    (close-variable variable-name)))
                
                ((string= command "open")
                  (expect-whitespace)
                  (skip-whitespaces)
                  (expect-character)
                  
                  (let ((variable-name (read-variable)))
                    (declare (type string variable-name))
                    
                    (let ((open-variable (get-variable variable-name)))
                      (declare (type (or null BWTFN-Variable) open-variable))
                      (cond
                        (open-variable
                          (variable-open open-variable)
                          (setf current-variable open-variable))
                        (T
                          (setf current-variable
                            (make-variable variable-name))
                          (variable-open     current-variable)
                          (register-variable current-variable))))))
                
                ((string= command "out")
                  (expect-whitespace)
                  (skip-whitespaces)
                  (expect-character)
                  
                  (let ((variable-name (read-variable)))
                    (declare (type string variable-name))
                    (let ((variable (get-variable variable-name)))
                      (declare (type (or null BWTFN-Variable) variable))
                      (skip-whitespaces)
                      (cond
                        (variable
                          (variable-out variable
                            (check-for-repetitions 1)))
                        (T
                          (error "No variable with the name ~s is ~
                                  registered, thus the 'out' command ~
                                  cannot be executed."
                            variable-name))))))
                
                ((string= command "output")
                  (expect-whitespace)
                  (skip-whitespaces)
                  (expect-character)
                  
                  (let ((variable-name (read-variable)))
                    (declare (type string variable-name))
                    (let ((variable (get-variable variable-name)))
                      (declare (type (or null BWTFN-Variable) variable))
                      (skip-whitespaces)
                      (cond
                        (variable
                          (variable-output variable
                            (check-for-repetitions 1)))
                        (T
                          (error "No variable with the name ~s is ~
                                  registered, thus the 'output' ~
                                  command cannot be executed."
                            variable-name))))))
                
                (T
                  (error "Invalid command ~s at position ~d."
                    command position)))))
          
          (T
            (error "Invalid character ~s at position ~d."
              character position))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-BWTFN
"
open helloworld
add 72
add 101
add 108*2
add 111
add 44
add 32
add 87
add 111
add 114
add 108
add 100
add 33
close helloworld
out helloworld
")

;;; -------------------------------------------------------

(interpret-BWTFN
"
open AAA
add 65*3
close AAA
output AAA*1000
")

;;; -------------------------------------------------------

;; Demonstrates the interplay of multiple open variables in conjunction
;; with the effects of opening and closing them.
(interpret-BWTFN
"
open letters
open numbers

open numbers
add 48

open letters
add 65

close letters
add 49
add 50

open letters
add 66
add 67

close letters
close numbers

output letters
output numbers
")
