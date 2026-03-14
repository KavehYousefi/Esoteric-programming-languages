;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Flux", invented by the Esolang user "Esolangist" and
;; presented on November 2nd, 2025, the diorism of which wones in its
;; appropriation of Chris Pressey's "Befunge" as a firmament, in both
;; the bidimensional program conformation and the integer-valued stack's
;; data castaldy, the former, norned the "playfield", accommodating the
;; instructions as characters commorant in cells.
;; 
;; 
;; Concept
;; =======
;; The Flux programming language constitutes a specimen edified upon the
;; foundational principles of Befunge, its entheus' cleronomy the
;; "playfield" as an ordonnance applied to the program code in two
;; dimenses of characters, wrapping around at the edges; as well as a
;; stack dedicated to the castaldy of signed integer numbers, its
;; respondency wisting of no carency, even if exhausted, in which case
;; the zero (0) value accommodates the resulting object.
;; 
;; == THE PROGRAM: A PLAYFIELD OF TWO DIMENSIONS ==
;; A Flux program's designment ensues from the principle of a reticulum
;; commorant in a bidimensional Cartesian space of finitude in the
;; dispansion; the elements of the thus formed cells structure
;; represented by characters, each such endowed with the amenability to
;; an x-y coordinates twain of a zero-based enumeration scheme.
;; 
;; The assignment of subscripts to the cells, in an augmented dation of
;; details, inchoates in the top-left corner, the original O(0, 0),
;; experiencing an incrementation in the x-coordinate when peragrating
;; in a sinistrodextral airt; and pursuing an auctive modulation of the
;; y-component as a proportional epiphenomenon concurring with a
;; catabasis.
;; 
;; Given a playfield of the width "w" and the height "h", the maximum
;; horizontal coordinate, "m", and the maximum vertical coordinate, "n",
;; assume the following values:
;; 
;;   m = w - 1
;;   n = h - 1
;; 
;; Ensuing from this dioristic terminology, the following visualization
;; appertains to the playfield cells' enumeration (x,y):
;; 
;;   +-------+-------+-------+-------+-------+
;;   |       |       |       |       |       |
;;   | (0,0) | (1,0) | (2,0) |  ...  | (m,0) |
;;   |       |       |       |       |       |
;;   +-------+-------+-------+-------+-------+
;;   |       |       |       |       |       |
;;   | (0,1) | (1,1) | (2,1) |  ...  | (m,1) |
;;   |       |       |       |       |       |
;;   +-------+-------+-------+-------+-------+
;;   |       |       |       |       |       |
;;   |  ...  |  ...  |  ...  |  ...  |  ...  |
;;   |       |       |       |       |       |
;;   +-------+-------+-------+-------+-------+
;;   |       |       |       |       |       |
;;   | (0,n) | (1,n) | (2,n) |  ...  | (m,n) |
;;   |       |       |       |       |       |
;;   +-------+-------+-------+-------+-------+
;; 
;; == THE PLAYFIELD EDGES WRAP AROUND ==
;; All edges fixated upon the playfield subsume into a particular
;; nomothesia which imposes their transgressions to a wrapping around
;; towards the obverse point along the same axis, while exerting no
;; deviation from the other axis.
;; 
;; Molded in a graphical ostention, the following principle holds:
;; 
;;               (0,n)   (1,n)   (2,n)   (j,m)   (m,n)
;;                 ^       ^       ^       ^       ^
;;                 |       |       |       |       |
;;             +-------+-------+-------+-------+-------+
;;             |       |       |       |       |       |
;;   (0,0) <-- | (0,0) | (1,0) | (2,0) |  ...  | (m,0) | --> (0,0)
;;             |       |       |       |       |       |
;;             +-------+-------+-------+-------+-------+
;;             |       |       |       |       |       |
;;   (m,1) <-- | (0,1) | (1,1) | (2,1) |  ...  | (m,1) | --> (0,1)
;;             |       |       |       |       |       |
;;             +-------+-------+-------+-------+-------+
;;             |       |       |       |       |       |
;;   (m,i) <-- |  ...  |  ...  |  ...  |  ...  |  ...  | --> (0,i)
;;             |       |       |       |       |       |
;;             +-------+-------+-------+-------+-------+
;;             |       |       |       |       |       |
;;   (m,n) <-- | (0,n) | (1,n) | (2,n) |  ...  | (m,n) | --> (0,n)
;;             |       |       |       |       |       |
;;             +-------+-------+-------+-------+-------+
;;                 |       |       |       |       |
;;                 v       v       v       v       v
;;               (0,0)   (1,0)   (2,0)   (j,0)   (m,0)
;; 
;; == THE INSTRUCTION POINTER (IP) PERAGRATES THE PLAYFIELD ==
;; An incolant of this plasmature constitutes the instruction pointer
;; (IP), whose location at a program's inchoacy empights the same in the
;; top-left corner, (0, 0), and whose direction at the same instant
;; points into a sinistrodextral motion.
;; 
;; During its peragration, each symbol engaging in a collision with this
;; cursor is evaluated as a contingent behest; upon its eloignment from
;; an operative wike, no causatum's gendrure, as well as no demur, shall
;; be expected.
;; 
;; A consectary issuing from the playfield's wrapping deportment along
;; the edges, for which please consult the section "THE PLAYFIELD EDGES
;; WRAP AROUND" aboon, the instruction pointer's traversal, upon any of
;; these terminal locality's violation, continues at the respective
;; overthwart destination, with a concomitant retention of the pointer's
;; direction.
;; 
;; == THE STACK: A STORAGE OF SIGNED INTEGER NUMBERS ==
;; The recipiency of the data castaldy's onus accompts for a stack's
;; bailiwick, the engagement of whom appertains to the storage of
;; integer numbers, nescient of any impositions concerning the sign or
;; mickleness.
;; 
;; Maugre its natural propensity for vacancy, indagations and removals
;; from an empty stack's top do not eventuate an illicit undertaking,
;; as in such circumstances the salvatory constantly responds with a
;; zero (0) element.
;; 
;; 
;; Instructions
;; ============
;; Flux's instruction set enumerates a cardinality of 51 members, the
;; bailiwicks entailed in this meiny dispanding across navigational
;; specimens, numerical, arithmetic, and logical vouchsafements, stack
;; and playfield manpulations, input and output communication warklumes,
;; and several conditional as well as unconditional control flow duction
;; constructs.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be attest its patefaction in a
;; sufficient mete of nortelry's adhibition anent the language's
;; operative facility.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   ==================================================================
;;   NAVIGATIONAL OPERATIONS
;;   ------------------------------------------------------------------
;;   v       | Redirects the instruction pointer's (IP) direction
;;           | downwards.
;;   ..................................................................
;;   ^       | Redirects the instruction pointer's (IP) direction
;;           | upwards.
;;   ..................................................................
;;   <       | Redirects the instruction pointer's (IP) direction
;;           | leftwards.
;;   ..................................................................
;;   >       | Redirects the instruction pointer's (IP) direction
;;           | rightwards.
;;   ..................................................................
;;   [       | Pops the top stack element; if the same equals zero (0),
;;           | redirects the instruction pointer's (IP) direction
;;           | leftwards; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   ]       | Pops the top stack element; if the same equals zero (0),
;;           | redirects the instruction pointer's (IP) direction
;;           | rightwards; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   (       | Pops the top stack element; if the same equals zero (0),
;;           | redirects the instruction pointer's (IP) direction
;;           | upwards; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   )       | Pops the top stack element; if the same equals zero (0),
;;           | redirects the instruction pointer's (IP) direction
;;           | downwards; otherwise accompasses no causatum.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   -       | Reflects the instruction pointer's (IP) direction on a
;;           | horizontal mirror.
;;           |---------------------------------------------------------
;;           | In a concrete diction, it holds:
;;           |   ----------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+---------------
;;           |   left              | left
;;           |   ..................................
;;           |   right             | right
;;           |   ..................................
;;           |   up                | down
;;           |   ..................................
;;           |   down              | up
;;           |   ----------------------------------
;;   ..................................................................
;;   |       | Reflects the instruction pointer's (IP) direction on a
;;           | vertical mirror.
;;           |---------------------------------------------------------
;;           | In a concrete diction, it holds:
;;           |   ----------------------------------
;;           |   Current direction | New direction
;;           |   ------------------+---------------
;;           |   left              | right
;;           |   ..................................
;;           |   right             | left
;;           |   ..................................
;;           |   up                | up
;;           |   ..................................
;;           |   down              | down
;;           |   ----------------------------------
;;   ..................................................................
;;   ?       | Aleatorily chooses a direction and redirects the
;;           | instruction pointer (IP) into the same.
;;   ==================================================================
;;   NUMERICAL OPERATIONS
;;   ------------------------------------------------------------------
;;   0       | Pushes the number zero (0) onto the stack.
;;   ..................................................................
;;   1       | Pushes the number one (1) onto the stack.
;;   ..................................................................
;;   2       | Pushes the number two (2) onto the stack.
;;   ..................................................................
;;   3       | Pushes the number three (3) onto the stack.
;;   ..................................................................
;;   4       | Pushes the number four (4) onto the stack.
;;   ..................................................................
;;   5       | Pushes the number five (5) onto the stack.
;;   ..................................................................
;;   6       | Pushes the number six (6) onto the stack.
;;   ..................................................................
;;   7       | Pushes the number seven (7) onto the stack.
;;   ..................................................................
;;   8       | Pushes the number eight (8) onto the stack.
;;   ..................................................................
;;   9       | Pushes the number nine (9) onto the stack.
;;   ..................................................................
;;   r       | Pushes a random number desumed from the closed integer
;;           | interval [0, 255] onto the stack.
;;   ==================================================================
;;   ARITHMETIC OPERATIONS
;;   ------------------------------------------------------------------
;;   +       | Pops the top stack element, here norned "a", and
;;           | subsequently the new top element, "b", supputates the
;;           | sum (b + a), and pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   S       | Pops the top stack element, here norned "a", and
;;           | subsequently the new top element, "b", supputates the
;;           | difference (b - a), and pushes the result onto the
;;           | stack.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   *       | Pops the top stack element, here norned "a", and
;;           | subsequently the new top element, "b", supputates the
;;           | product (b * a), and pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   /       | Pops the top stack element, here norned "a", and
;;           | subsequently the new top element, "b", supputates the
;;           | quotient (b / a), and pushes the result onto the stack.
;;           |---------------------------------------------------------
;;           | If the divisor, "a", resolves to zero (0), an error
;;           | displaying the message "Don't divide by 0" is signaled.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   %       | Pops the top stack element, here norned "a", and
;;           | subsequently the new top element, "b", supputates the
;;           | remainder (b modulo a), and pushes the result onto the
;;           | stack.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ==================================================================
;;   LOGICAL OPERATIONS
;;   ------------------------------------------------------------------
;;   =       | If the top two stack elements are equal, pushes the
;;           | value one (1) onto the stack, otherwise pushes zero (0).
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   !       | If the top stack element equals zero (0), pushes the
;;           | value one (1) onto the stack, otherwise pushes zero (0).
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ==================================================================
;;   STACK MANIPULATION
;;   ------------------------------------------------------------------
;;   ~       | Pops the top stack element and discards the same.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   :       | Duplicates the top stack element, that is, pushes a copy
;;           | of the top item onto the stack.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   x       | Pops the top stack element, here norned "n", and
;;           | duplicates the top "n" elements.
;;           |---------------------------------------------------------
;;           | In diction entalented with enhaused lucidity, the
;;           | stack's "n" top elements are copied, and this new
;;           | ordered sequence is prepended in the extant order to the
;;           | stack top. For instance, given a stack
;;           | 
;;           |   [top> 1 2 3 4 5 <bottom]
;;           | 
;;           | whose n = 3 top elements shall be duplicated, the new
;;           | constitutions limns:
;;           | 
;;           |   [top> 1 2 3 1 2 3 4 5 <bottom]
;;           |         *****
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   $       | Swaps the two top stack elements.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   {       | Rotates the stack up, that is, moves the element two
;;           | positions below the top item to the stack's top
;;           | position, while retaining the relative configuration of
;;           | the erstwhile top element and its immediate neighbor
;;           | alow.
;;           |---------------------------------------------------------
;;           | In a view endowed with more graphical puissance, a
;;           | principle applies which transition from the original
;;           | constitution
;;           | 
;;           |   [top> a, b, c <bottom]
;;           | 
;;           | to the new conformation:
;;           | 
;;           |   [top> c, a, b <bottom]
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- stack.pop()
;;           |   let b <- stack.pop()
;;           |   let c <- stack.pop()
;;           |   
;;           |   stack.push(b)
;;           |   stack.push(a)
;;           |   stack.push(c)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   }       | Rotates the stack down, that is, moves the top stack
;;           | element two positions down, while retaining the relative
;;           | configuration of its erstwhile subordinate neighbors.
;;           |---------------------------------------------------------
;;           | In a view endowed with more graphical puissance, a
;;           | principle applies which transition from the original
;;           | constitution
;;           | 
;;           |   [top> a, b, c <bottom]
;;           | 
;;           | to the new conformation:
;;           | 
;;           |   [top> b, c, a <bottom]
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let a <- stack.pop()
;;           |   let b <- stack.pop()
;;           |   let c <- stack.pop()
;;           |   
;;           |   stack.push(a)
;;           |   stack.push(c)
;;           |   stack.push(b)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   c       | Clears the stack by removing its entire content.
;;   ==================================================================
;;   INPUT AND OUTPUT INTERCOURSE
;;   ------------------------------------------------------------------
;;   .       | Pops the top stack element and prints the same in its
;;           | verbatim numeric form to the standard output conduit.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   ,       | Pops the top stack element and prints the character
;;           | whose ASCII code concurs with this value to the standard
;;           | output conduit.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   &       | Queries the standard input conduit for a signed or
;;           | unsigned integer number and pushes the result onto the
;;           | stack.
;;   ..................................................................
;;   '       | Queries the standard input conduit for a character and
;;           | pushes its ASCII code onto the stack.
;;   ..................................................................
;;   "       | Commences the string mode: Until the matching '"' symbol
;;           | has been encountered, sojourns the subsequent code
;;           | characters, interprets thilk as character literals, in
;;           | lieu of instruction identifiers, gathers their ASCII
;;           | codes in their specified arrangement in an ordered list,
;;           | and, traversing this list from right to left, pushes the
;;           | thus collected character codes onto the stack; finally
;;           | moving ayond the concluding '"' cell.
;;           |---------------------------------------------------------
;;           | Please heed that the resulting stack will replicate the
;;           | obtained string's character codes, when perused from top
;;           | to bottom, in their correct order.
;;   ==================================================================
;;   STACK-PLAYFIELD INTERCOURSE OPERATIONS
;;   ------------------------------------------------------------------
;;   g       | Pops the top stack element, here norned "x", and the
;;           | new top stack element, "y", requests the playfield cell
;;           | symbol amenable to the coordinates ("x", "y"), yclept
;;           | "w", transcripts this character to its ASCII code, "z",
;;           | and pushes the character code in "z" onto the stack.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x <- stack.pop()
;;           |   let y <- stack.pop()
;;           |   let z <- playfield[x, y]
;;           |   let w <- asciiCodeForCharacter(z)
;;           |   
;;           |   stack.push(w)
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   p       | Pops the three top stack elements, with the top item
;;           | norned "x", the element immediate below as "y", and the
;;           | next lower member "z", obtains the character whose ASCII
;;           | code concurs with the value of "z", here yclept "w", and
;;           | writes the character stored in "w" to the playfield cell
;;           | amenable to the zero-based coordinates ("x", "y").
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x <- stack.pop()
;;           |   let y <- stack.pop()
;;           |   let z <- stack.pop()
;;           |   let w <- asciiCodeToCharacter(z)
;;           |   
;;           |   playfield[x, y] <- w
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ==================================================================
;;   CONTROL FLOW DUCTION
;;   ------------------------------------------------------------------
;;   #       | Skips the next instruction.
;;   ..................................................................
;;   £       | Peeks without removing the top stack element; if the
;;           | same does not equal zero (0), skips the next
;;           | instruction; otherwise accompasses no further causatum.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   j       | Pops the top stack element, here norned "x", and
;;           | subsequently the new top element, "y", and relocates the
;;           | instruction pointer (IP) to the zero-based coordinates
;;           | ("x", "y") in the program grid.
;;           |---------------------------------------------------------
;;           | In a pseudocode diction, it holds:
;;           | 
;;           |   let x <- stack.pop()
;;           |   let y <- stack.pop()
;;           |   
;;           |   ip.x <- x
;;           |   ip.y <- y
;;           |---------------------------------------------------------
;;           | Please heed that the instruction pointer retains its
;;           | current traversal direction.
;;           |---------------------------------------------------------
;;           | If the stack cannot accommodate a requisite accompt of
;;           | elements for this operation, the empty stack will
;;           | respond with a zero (0) value to the respective request.
;;   ..................................................................
;;   R       | Moves the instruction pointer (IP) back to the most
;;           | recently visited "m" command, if such exists; otherwise
;;           | accompasses no further causatum.
;;           |---------------------------------------------------------
;;           | Please heed that the instruction pointer retains its
;;           | current traversal direction.
;;   ..................................................................
;;   m       | Does itself not incorporate any causatum, but serves as
;;           | a back jump point for the "R" instruction, which please
;;           | concredit to your conspectuity.
;;   ..................................................................
;;   l       | Peeks without removing the top stack element; if the
;;           | same equals zero (0), moves the instruction pointer (IP)
;;           | forward to the position immediately succeeding the
;;           | matching "e" token along the instruction pointer's (IP)
;;           | current direction; otherwise accompasses no further
;;           | causatum.
;;           |---------------------------------------------------------
;;           | Please heed that the instruction pointer retains its
;;           | current traversal direction.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   e       | Peeks without removing the top stack element; if the
;;           | same does not equal zero (0), moves the instruction
;;           | pointer (IP) back to the position immediately succeeding
;;           | the matching "l" token along the opposite direction to
;;           | the instruction pointer's current trajectory; otherwise
;;           | accompasses no further causatum.
;;           |---------------------------------------------------------
;;           | Please heed that the instruction pointer retains its
;;           | current traversal direction.
;;           |---------------------------------------------------------
;;           | If the stack is empty at the instant of this
;;           | instruction's invocation, the value zero (0) is naited.
;;   ..................................................................
;;   @       | Immediately halts the program.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter's implementation constitutes an effort actuated in
;; the programming language Common Lisp, its operation dimidiated into
;; a parasceve, thilk begets from the one-dimensional Flux code string
;; a veridicous two-dimensional reticulation, the playfield, ere the
;; same is administered actually efficacy.
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
;; Date:   2026-03-13
;; 
;; Sources:
;;   [christensen:2013:lispcabinet035]
;;   G. Christensen, "Lisp Cabinet 0.3.5", 2013
;;   URL: "https://sourceforge.net/projects/lispcabinet/"
;;   Notes:
;;     - Download page of the "Lisp Cabinet" project.
;;   
;;   [esolang:2025:Flux (Esolangist)]
;;   The Esolang contributors, "Flux (Esolangist)", November 2nd, 2025
;;   URL: "https://esolangs.org/wiki/Flux_(Esolangist)"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the type operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-a-new-type
    (name (candidate-variable &rest lambda-list) &body body)
  "Defines a derived type whose agnomination limns the NAME's dation,
   and whose formal parameters is appropriated in an ipsissima verba
   fashion from the LAMBDA-LIST, evaluates the BODY forms, admitting an
   adit to the probed object per procurationem of the
   CANDIDATE-VARIABLE, and construes the desinent BODY form's primary
   result as the docimasy's perclose, a \"generalized boolean\" value
   of \"true\" establishing a tantamount to the candidate's
   compatibility with the BODY form's predicate; while a \"false\"
   response is interpreted its rejection.
   ---
   The first BODY form, is resolving to a string object, is contrued as
   the type's documentation string, and is being reappropriated for this
   purpose."
  (let ((predicate-name (gensym)))
    (declare (type symbol predicate-name))
    `(deftype ,name ,lambda-list
       ,(or (and (stringp body))
            "")
       (let ((,predicate-name (gensym)))
         (declare (type symbol ,predicate-name))
         (setf (symbol-function ,predicate-name)
           #'(lambda (,candidate-variable)
               (declare (type T    ,candidate-variable))
               (declare (ignorable ,candidate-variable))
               ,@body))
         `(satisfies ,,predicate-name)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-new-type hash-table-of (candidate
                                  &optional
                                    (key-type   '*)
                                    (value-type '*))
  "The ``hash-table-of'' type defines a hash table comprehending zero
   or more entries, each member of which is delineated by a key
   complying to the KEY-TYPE and an associated value subsuming into the
   VALUE-TYPE, for both contributes the generic sentinel ``*'' the
   default state."
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

(define-a-new-type list-of (candidate &optional (element-type '*))
  "The ``list-of'' type defines a linked list whose componency admits
   zero or more elements, each such adhering to the ELEMENT-TYPE, for
   thilk is assigned the generic sentinel ``*'' in the default
   configuration."
  (and
    (listp candidate)
    (loop
      for    current-element of-type T in (the list candidate)
      always (typep current-element element-type))))

;;; -------------------------------------------------------

(deftype location ()
  "The ``location'' type defines a two-dimensional position signifier,
   expressing a Cartesian coordinates jumelle as an (x, y) compounds,
   its patefaction either a complex number edified upon two signed
   integer numbers, or a scalar integer of the same liberal
   constitution."
  '(or integer (complex integer)))

;;; -------------------------------------------------------

(deftype character-matrix ()
  "The ``character-matrix'' type defines a sparse two-dimensional array
   of characters, the patefaction of which is delineated by a hash table
   whose keys admit ``location'' object encapsulations of the x-y
   ponibility's voucher, and whose values contribute the characters at
   the thus designated point."
  '(hash-table-of location character))

;;; -------------------------------------------------------

(deftype direction ()
  "The ``direction'' type enumerates the valid directions along which
   the instruction pointer (IP) may traverse athwart the Flux
   playfield."
  '(member :left :right :up :down))

;;; -------------------------------------------------------

(deftype binary-operation ()
  "The ``binary-operation'' type defines a dyadic function operating on
   integer inputs and returning a single number from the selfsame
   vale."
  '(function (integer integer) integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the condition types.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition Do-Not-Divide-By-Zero-Error (error)
  ()
  (:report
    "Don't divide by 0")
  (:documentation
    "The ``Do-Not-Divide-By-Zero-Error'' serves in the apprizal about an
     anomalous circumstance whose etiology wones in the attempt to
     divide a number by the inadmissible divisor of zero (0)."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Configuration of the random number generator.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *random-state* (make-random-state T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the general arithmetic operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun divide-the-integers (dividend divisor)
  "Divides the DIVIDEND by the DIVISOR and returns the integral result
   of the quotient rounded down.
   ---
   Upon the DIVISOR's owelty to zero (0), an error of the type
   ``Do-Not-Divide-By-Zero-Error'' is signaled."
  (declare (type integer dividend))
  (declare (type integer divisor))
  (the integer
    (if (zerop divisor)
      (error 'Do-Not-Divide-By-Zero-Error)
      (floor dividend divisor))))

;;; -------------------------------------------------------

(defun supputate-the-integer-remainder (dividend divisor)
  "Divides the DIVIDEND by the DIVISOR and returns the integral
   remainder.
   ---
   Upon the DIVISOR's owelty to zero (0), an error of the type
   ``Do-Not-Divide-By-Zero-Error'' is signaled."
  (declare (type integer dividend))
  (declare (type integer divisor))
  (the integer
    (if (zerop divisor)
      (error 'Do-Not-Divide-By-Zero-Error)
      (mod dividend divisor))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-into-a-boolean-value (object)
  "Construes the OBJECT in its aspect as a \"generalized boolean\" and
   returns a veridicous Boolean tantamount thereof, producing for a
   non-``NIL'' input a ``boolean'' value of ``T''; otherwise, for a
   ``NIL'' OBJECT, responds with ``NIL'' itself."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the character operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newline-character-p (candidate)
  "Determines whether the CANDIDATE represents a newline entity,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type character candidate))
  (the boolean
    (convert-into-a-boolean-value
      (member candidate '(10 11 12 13)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the general string operations.             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun concatenate-the-lines (&rest lines)
  "Creates and returns a fresh simple string obtained by concatenating
   the LINES, each attiguous twissel's interstice contexed by a single
   newline character as the vinculum."
  (declare (type (list-of string) lines))
  (the simple-string
    (coerce
      (format NIL "~{~a~^~%~}" lines)
      'simple-string)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the location operations.                   -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun specify-a-location (x y)
  "Creates and returns a fresh ``location'' whose horizontal coordinate
   conflates with X, and whose vertical positioning represents Y's
   dation."
  (declare (type integer x))
  (declare (type integer y))
  (the location
    (complex x y)))

;;; -------------------------------------------------------

(defun location-x (location)
  "Returns the LOCATION's x-coordinate."
  (declare (type location location))
  (the integer
    (realpart location)))

;;; -------------------------------------------------------

(defun location-y (location)
  "Returns the LOCATION's y-coordinate."
  (declare (type location location))
  (the integer
    (imagpart location)))

;;; -------------------------------------------------------

(defun location-coordinates (location)
  "Returns the LOCATION's coordinates as two values:
     (1) The location's x-coordinate.
     (2) The location's y-coordinate."
  (declare (type location location))
  (the (values integer integer)
    (values
      (location-x location)
      (location-y location))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the directional operations.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-array direction (4)) +DIRECTIONS+))

;;; -------------------------------------------------------

(defparameter +DIRECTIONS+
  (coerce
    '(:left :right :up :down)
    '(simple-array direction (4)))
  "Maintains the valid instruction pointer (IP) directions in a
   one-dimensional simple array, nait for the purpose of a random airt's
   selection.")

;;; -------------------------------------------------------

(defun reverse-the-direction (direction)
  "Returns the obverse airt to the DIRECTION."
  (declare (type direction direction))
  (the direction
    (case direction
      (:left  :right)
      (:right :left)
      (:up    :down)
      (:down  :up)
      (otherwise
        (error "The opposite direction to ~s cannot be determined."
          direction)))))

;;; -------------------------------------------------------

(defun reflect-on-a-horizontal-mirror (direction)
  "Returns the ``direction'' obtained by the input DIRECTION's
   reflection on a horizontally aligned mirror."
  (declare (type direction direction))
  (the direction
    (case direction
      (:left  :left)
      (:right :right)
      (:up    :down)
      (:down  :up)
      (otherwise
        (error "The direction ~s cannot be reflected." direction)))))

;;; -------------------------------------------------------

(defun reflect-on-a-vertical-mirror (direction)
  "Returns the ``direction'' obtained by the input DIRECTION's
   reflection on a vertically aligned mirror."
  (declare (type direction direction))
  (the direction
    (case direction
      (:left  :right)
      (:right :left)
      (:up    :up)
      (:down  :down)
      (otherwise
        (error "The direction ~s cannot be reflected." direction)))))

;;; -------------------------------------------------------

(defun select-a-random-direction ()
  "Selects and returns a ``direction'' in an aleatory fashion."
  (the direction
    (aref +DIRECTIONS+
      (random
        (length +DIRECTIONS+)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the playfield.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type standard-char +DEFAULT-CELL-SYMBOL+))

;;; -------------------------------------------------------

(defconstant +DEFAULT-CELL-SYMBOL+
  (code-char 32)
  "The default character to impute in a cell in the case its direct
   specification's carency.")

;;; -------------------------------------------------------

(defclass Playfield ()
  ((cells
    :initform      (make-hash-table)
    :accessor      playfield-cells
    :type          character-matrix
    :documentation "A sparse two-dimensional array of the characters
                    representing the instructions.")
   (width
    :initform      0
    :reader        playfield-width
    :type          fixnum
    :documentation "The tally of columns comprising the playfield.")
   (height
    :initform      0
    :reader        playfield-height
    :type          fixnum
    :documentation "The tally of rows comprising the playfield."))
  (:documentation
    "The ``Playfield'' class furnishes an implementation of the
     bidimensional grid dedicated to the convenient representation of a
     Flux program's source code.
     ---
     A Flux playfield obeys a dioristic nomothesia, according to which
     the edges wrap around to the overthwart mear, thus accommodating a
     plasmature of an annulate constitution along each axis."))

;;; -------------------------------------------------------

(defun prepare-an-empty-playfield ()
  "Creates and returns a fresh ``Playfield'' whose incipial dimensity
   athwart both axes amounts to zero, and whose content, as a
   consectary, does not wist of any plenitude."
  (the Playfield
    (make-instance 'Playfield)))

;;; -------------------------------------------------------

(defun adjust-the-location-to-the-playfield (playfield
                                             original-location)
  "Returns a fresh ``location'' based upon the ORIGINAL-LOCATION with
   consideration of the PLAYFIELD's dimensions.
   ---
   A Flux playfield obeys a dioristic nomothesia, according to which
   the edges wrap around to the overthwart mear, thus accommodating a
   plasmature of an annulate constitution along each axis."
  (declare (type Playfield playfield))
  (declare (type location  original-location))
  (the location
    (multiple-value-bind (original-x original-y)
        (location-coordinates original-location)
      (declare (type integer original-x))
      (declare (type integer original-y))
      (specify-a-location
        (mod original-x
          (playfield-width playfield))
        (mod original-y
          (playfield-height playfield))))))

;;; -------------------------------------------------------

(defun change-the-playfield-symbol-to (playfield point new-symbol)
  "Changes the content of the cell in the PLAYFIELD amenable to the
   POINT to the NEW-SYMBOL and returns no value."
  (declare (type Playfield playfield))
  (declare (type location  point))
  (declare (type character new-symbol))
  (multiple-value-bind (actual-x actual-y)
      (location-coordinates
        (adjust-the-location-to-the-playfield playfield point))
    (declare (type (integer 0 *) actual-x))
    (declare (type (integer 0 *) actual-y))
    (setf (gethash
            (specify-a-location actual-x actual-y)
            (playfield-cells    playfield))
           new-symbol))
  (values))

;;; -------------------------------------------------------

(defun request-the-playfield-symbol-at (playfield point)
  "Returns the symbol occupying the PLAYFIELD cell amenable to the
   POINT, contingently preceded by a wrapping around into the admissible
   grid mears."
  (declare (type Playfield playfield))
  (declare (type location  point))
  (the character
    (multiple-value-bind (actual-x actual-y)
        (location-coordinates
          (adjust-the-location-to-the-playfield playfield point))
      (declare (type (integer 0 *) actual-x))
      (declare (type (integer 0 *) actual-y))
      (gethash
        (specify-a-location actual-x actual-y)
        (playfield-cells    playfield)
        +DEFAULT-CELL-SYMBOL+))))

;;; -------------------------------------------------------

(defun write-the-playfield-symbol-to (playfield x y destination)
  "Prints the PLAYFIELD symbol located at the position (X, Y) to the
   DESTINATION stream and returns no value."
  (declare (type Playfield playfield))
  (declare (type fixnum    x))
  (declare (type fixnum    y))
  (declare (type stream    destination))
  (format destination "~c"
    (request-the-playfield-symbol-at playfield
      (specify-a-location x y)))
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((playfield Playfield) (stream T))
  (declare (type Playfield playfield))
  (declare (type stream    stream))
  (with-slots (cells width height) playfield
    (declare (type character-matrix cells))
    (declare (type fixnum           width))
    (declare (type fixnum           height))
    (format stream "~&Playfield with ~d columns and ~d rows:"
      width height)
    (dotimes (y height)
      (declare (type fixnum y))
      (format stream "~&")
      (dotimes (x width)
        (declare (type fixnum x))
        (write-the-playfield-symbol-to playfield x y stream))))
  (the Playfield playfield))

;;; -------------------------------------------------------

(defun arrange-the-code-in-a-playfield (code)
  "Creates and returns a fresh ``Playfield'' dedicated to the piece of
   Flux source CODE's arrangement in a bidimensional plasmature."
  (declare (type string code))
  (let ((playfield (make-instance 'Playfield))
        (x         0)
        (y         0))
    (declare (type Playfield playfield))
    (declare (type fixnum    x))
    (declare (type fixnum    y))
    (with-slots (cells width height) playfield
      (declare (type character-matrix cells))
      (declare (type fixnum           width))
      (declare (type fixnum           height))
      (loop for current-token of-type character across code do
        (cond
          ((newline-character-p current-token)
            (setf x 0)
            (incf y)
            (setf height (1+ y)))
          (T
            (setf (gethash (specify-a-location x y) cells)
                  current-token)
            (setf height (max height 1))
            (incf x)
            (setf width  (max width  x))))))
    (the Playfield playfield)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the program cursor.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Cursor ()
  ((location
    :initform      (specify-a-location 0 0)
    :accessor      cursor-location
    :type          location
    :documentation "The cursor's current position.")
   (direction
    :initform      :right
    :accessor      cursor-direction
    :type          direction
    :documentation "The cursor's current direction."))
  (:documentation
    "The ``Cursor'' class establishes a gressible instruction pointer
     (IP), its intended woning the two-dimensional Cartesian
     reticulation accommodated by a Flux playfield, and whose diorism
     entertains a dimerism into two bidimension position and the
     direction."))

;;; -------------------------------------------------------

(defun make-a-cursor ()
  "Creates and returns a fresh ``Cursor'', empight at its state of
   inchoacy at the position (0, 0), and pointing dextrally."
  (the Cursor
    (make-instance 'Cursor)))

;;; -------------------------------------------------------

(defun advance-the-cursor (cursor)
  "Advances the CURSOR one step into its currently assigned direction
   and returns no value."
  (declare (type Cursor cursor))
  (incf (cursor-location cursor)
    (case (cursor-direction cursor)
      (:left  (complex -1  0))
      (:right (complex +1  0))
      (:up    (complex  0 -1))
      (:down  (complex  0 +1))
      (otherwise
        (error "The cursor cannot move into the direction ~s."
          (cursor-direction cursor)))))
  (values))

;;; -------------------------------------------------------

(defun relocate-the-cursor-to (cursor new-location)
  "Relocates the CURSOR to the NEW-LOCATION and returns no value."
  (declare (type Cursor   cursor))
  (declare (type location new-location))
  (setf (cursor-location cursor) new-location)
  (values))

;;; -------------------------------------------------------

(defun redirect-the-cursor-to (cursor new-direction)
  "Modulates the CURSOR to the NEW-DIRECTION and returns no value."
  (declare (type Cursor    cursor))
  (declare (type direction new-direction))
  (setf (cursor-direction cursor) new-direction)
  (values))

;;; -------------------------------------------------------

(defmethod print-object ((cursor Cursor) (stream T))
  (declare (type Cursor cursor))
  (declare (type stream stream))
  (format stream "(Cursor x=~d, y=~d, direction=~s)"
    (location-x
      (cursor-location cursor))
    (location-y
      (cursor-location cursor))
    (cursor-direction cursor))
  (the Cursor cursor))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Flux interpreter.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Interpreter ()
  ((playfield
    :initarg       :playfield
    :initform      (error "No playfield has been supplied.")
    :reader        interpreter-playfield
    :type          Playfield
    :documentation "The two-dimensional program code.")
   (cursor
    :initform      (make-a-cursor)
    :reader        interpreter-cursor
    :type          Cursor
    :documentation "The instruction pointer (IP).")
   (retains-location-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the CURSOR
                    shall, at one cycle's desition, retain its current
                    position, rather than advance to the next cell along
                    its airt.")
   (skips-next-symbol-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag dedicated to the signification of a
                    potential skipping's administration to the next
                    playfield symbol.")
   (has-halted-p
    :initform      NIL
    :type          boolean
    :documentation "A Boolean flag which determines whether the program
                    has been halted per procurationem of the dedicated
                    \"@\" command.")
   (last-m-location
    :initform      NIL
    :type          (or null location)
    :documentation "The position of the most recently encountered \"m\"
                    command.")
   (jump-stack
    :initform      NIL
    :type          (list-of location)
    :documentation "A stack concredited with the castaldy of the
                    currently active loop points demarcated by an
                    instigating \"l\" and a desitive \"e\" symbol;
                    the JUMP-STACK storing \"l\" component's location
                    for a contingently repeatedly requested sojourn back
                    via the \"e\" behest.")
   (stack
    :initform      NIL
    :accessor      interpreter-stack
    :type          (list-of integer)
    :documentation "The memory as a stack of signed integer numbers."))
  (:documentation
    "The ``Interpreter'' class serves in an entity's accoutrement whose
     parcery of duty appertains to the peracting of actual efficacy to
     a Flux program concredited in a playfield's plasmature."))

;;; -------------------------------------------------------

(defun furnish-an-interpreter-for (playfield)
  "Creates and returns a fresh ``Interpreter'' dedicated to the Flux
   PLAYFIELD's execution."
  (declare (type Playfield playfield))
  (the Interpreter
    (make-instance 'Interpreter :playfield playfield)))

;;; -------------------------------------------------------

(defmacro with-the-interpreter ((interpreter) &body body)
  "Evaluates the INTERPRETER, binds its slots to approximate eponymous
   local symbol macros, always prefixed with a dollar sign (\"$\"),
   evaluates the BODY forms, and returns the desinent form's results.
   ---
   An apercu of compendiousness in haecceity shall constitute the
   participating symbol macros' vouchsafements:
     ------------------------------------------------------------------
     Symbol macro name    | Purpose
     ---------------------+--------------------------------------------
     $playfield           | The Flux playfield.
     ..................................................................
     $cursor              | The instruction pointer (IP).
     ..................................................................
     $retains-location-p  | A Boolean flag which determines whether the
                          | instruction pointer (IP) shall, for one
                          | secle, retain its current position, in lieu
                          | of an advancement.
     ..................................................................
     $skips-next-symbol-p | A Boolean flag signifying whether the next
                          | playfield cell shall be skipped.
     ..................................................................
     $has-halted-p        | A Boolean flag which determines whether a
                          | \"@\" command has halted the program.
     ..................................................................
     $last-m-location     | The position of the most recently
                          | encountered \"m\" command, if extant.
     ..................................................................
     $jump-stack          | A stack amplecting the locations of the
                          | currently active \"l\" forward jump
                          | commands.
     ..................................................................
     $stack               | The integer-valued memory stack.
     ------------------------------------------------------------------"
  `(with-slots (($playfield           playfield)
                ($cursor              cursor)
                ($retains-location-p  retains-location-p)
                ($skips-next-symbol-p skips-next-symbol-p)
                ($has-halted-p        has-halted-p)
                ($last-m-location     last-m-location)
                ($jump-stack          jump-stack)
                ($stack               stack))
       ,interpreter
     (declare (type Playfield          $playfield)
              (ignorable               $playfield))
     (declare (type Cursor             $cursor)
              (ignorable               $cursor))
     (declare (type boolean            $retains-location-p)
              (ignorable               $retains-location-p))
     (declare (type boolean            $skips-next-symbol-p)
              (ignorable               $skips-next-symbol-p))
     (declare (type boolean            $has-halted-p)
              (ignorable               $has-halted-p))
     (declare (type (or null location) $last-m-location)
              (ignorable               $last-m-location))
     (declare (type (list-of location) $jump-stack)
              (ignorable               $jump-stack))
     (declare (type (list-of integer)  $stack)
              (ignorable               $stack))
     ,@body))

;;; -------------------------------------------------------

(defmacro with-respect-to-the-skip-flag ((interpreter) &body body)
  "Evaluates the INTERPRETER, and determines whether its subsequent
   playfield cell skipping flag, ``skips-next-symbol-p'', is active,
   on confirmation negating the flag, while accompassing no further
   epiphenomenon, in particular assing a desuetude to the BODY, and
   returning no value; otherwise, for a \"false\"-valued skip flag, the
   BODY forms are evaluated, and the desinent form's results are
   returned."
  `(with-slots (skips-next-symbol-p) ,interpreter
     (declare (type boolean skips-next-symbol-p))
     (cond
       (skips-next-symbol-p
         (setf skips-next-symbol-p NIL)
         (values))
       (T
         ,@body))))

;;; -------------------------------------------------------

(defun request-the-current-symbol (interpreter)
  "Returns the symbol commorant in the INTERPRETER's currently selected
   Flux playfield cell."
  (declare (type Interpreter interpreter))
  (the character
    (with-the-interpreter (interpreter)
      (request-the-playfield-symbol-at $playfield
        (cursor-location $cursor)))))

;;; -------------------------------------------------------

(defun adjust-the-cursor-to-the-playfield (interpreter)
  "Ascertains the INTERPRETER's instruction pointer (IP)'s commorancy
   to partake in the playfield's bournes by wrapping its location into
   the admissible range and returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (relocate-the-cursor-to $cursor
      (adjust-the-location-to-the-playfield $playfield
        (cursor-location $cursor))))
  (values))

;;; -------------------------------------------------------

(defun advance-to-the-next-symbol (interpreter)
  "Advances the INTERPRETER's instruction pointer (IP) to the next
   position in the underlying Flux playfield and returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (if $retains-location-p
      (setf $retains-location-p NIL)
      (advance-the-cursor $cursor))
    (adjust-the-cursor-to-the-playfield interpreter))
  (values))

;;; -------------------------------------------------------

(defun reverse-the-instruction-pointer (interpreter)
  "Reverses the direction of the INTERPRETER's instruction pointer (IP)
   and returns no value."
  (declare (type Interpreter interpreter))
  (with-slots (cursor) interpreter
    (declare (type Cursor cursor))
    (setf (cursor-direction cursor)
      (reverse-the-direction
        (cursor-direction cursor))))
  (values))

;;; -------------------------------------------------------

(defun push-onto-the-stack (interpreter new-element)
  "Pushes the NEW-ELEMENT onto the INTERPRETER stack's top position and
   returns no value."
  (declare (type Interpreter interpreter))
  (declare (type integer     new-element))
  (push new-element
    (interpreter-stack interpreter))
  (values))

;;; -------------------------------------------------------

(defun pop-from-the-stack (interpreter)
  "Pops and returns the top element from the INTERPRETER's stack."
  (declare (type Interpreter interpreter))
  (the integer
    (or (pop (interpreter-stack interpreter))
        0)))

;;; -------------------------------------------------------

(defun pop-two-elements-from-the-stack (interpreter)
  "Removes the two top elements from the INTERPRETER's stack and
   returns two values:
     (1) The erstwhile top element from the stack.
     (2) The element immediately alow the erstwhile top element from
         the stack.
   ---
   If the stack cannot furnish the tharfed objects, each illicit request
   will produce a zero (0) response."
  (declare (type Interpreter interpreter))
  (the (values integer integer)
    (values
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter))))

;;; -------------------------------------------------------

(defun pop-three-elements-from-the-stack (interpreter)
  "Removes the three top elements from the INTERPRETER's stack and
   returns three values:
     (1) The erstwhile top element from the stack.
     (2) The element immediately alow the erstwhile top element from
         the stack.
     (3) The element two positions alow the erstwhile top element.
   ---
   If the stack cannot furnish the tharfed objects, each illicit request
   will produce a zero (0) response."
  (declare (type Interpreter interpreter))
  (the (values integer integer integer)
    (values
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter)
      (pop-from-the-stack interpreter))))

;;; -------------------------------------------------------

(defun peek-into-the-stack (interpreter)
  "Returns without removing the top element from the INTERPRETER's
   stack."
  (declare (type Interpreter interpreter))
  (the integer
    (or (first (interpreter-stack interpreter))
        0)))

;;; -------------------------------------------------------

(defun top-stack-element-equals-zero-p (interpreter)
  "Perquires, without a concomitant removal, the top element on the
   INTERPRETER's stack and determines whether the same equals zero (0),
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type Interpreter interpreter))
  (the boolean
    (convert-into-a-boolean-value
      (zerop
        (peek-into-the-stack interpreter)))))

;;; -------------------------------------------------------

(defun apply-a-binary-operator (interpreter operator)
  "Applies a binary OPERATOR to the INTERPRETER's stack by first
   removing the top element, here norned \"a\", then new top element,
   \"b\", subsequently supputating the result of applying the OPERATOR
   to \"b\" as the first and \"a\" as the second operand, and pushing
   the result onto the stack, finally returning no value."
  (declare (type Interpreter      interpreter))
  (declare (type binary-operation operator))
  (multiple-value-bind (right-operand left-operand)
      (pop-two-elements-from-the-stack interpreter)
    (declare (type integer right-operand))
    (declare (type integer left-operand))
    (push-onto-the-stack interpreter
      (funcall operator left-operand right-operand)))
  (values))

;;; -------------------------------------------------------

(defgeneric process-the-symbol (interpreter symbol)
  (:documentation
    "Processes the SYMBOL in the INTERPRETER's context and returns no
     value.")
  
  (:method ((interpreter Interpreter) (symbol character))
    (declare (type Interpreter interpreter)
             (ignore           interpreter))
    (declare (type character   symbol)
             (ignore           symbol))
    (values)))

;;; -------------------------------------------------------

(defmacro define-a-symbol-processor (symbol &body body)
  "Defines an implementation of the generic function
   ``process-the-symbol'', its first formal parameter being nevened
   ``$interpreter'' and specializing on the ``Interpreter'' class, its
   second, assigned the agnomination ``$symbol'', ``eql''-specializing
   on the evaluated SYMBOL character, the BODY forms, being ensconced
   in a twyfold context of a ``with-respect-to-the-skip-flag'' macro
   invocation and an ensconced ``with-the-interpreter'' envelope,
   evaluates the BODY forms, and returns no value.
   ---
   Ensuing from the moult strata of macros and definitions, the
   following tabulation's dation shall constitute a patration in the
   governing bindings' enumeration:
     ------------------------------------------------------------------
     Binding              | Purpose
     ---------------------+--------------------------------------------
     $interpreter         | The first parameter, signifying the
                          | interpreter operating on the symbol.
     ..................................................................
     $symbol              | The second parameter, representing the
                          | character evaluated.
     ..................................................................
     $playfield           | The Flux playfield.
     ..................................................................
     $cursor              | The instruction pointer (IP).
     ..................................................................
     $retains-location-p  | A Boolean flag which determines whether the
                          | instruction pointer (IP) shall, for one
                          | secle, retain its current position, in lieu
                          | of an advancement.
     ..................................................................
     $skips-next-symbol-p | A Boolean flag signifying whether the next
                          | playfield cell shall be skipped.
     ..................................................................
     $has-halted-p        | A Boolean flag which determines whether a
                          | \"@\" command has halted the program.
     ..................................................................
     $last-m-location     | The position of the most recently
                          | encountered \"m\" command, if extant.
     ..................................................................
     $jump-stack          | A stack amplecting the locations of the
                          | currently active \"l\" forward jump
                          | commands.
     ..................................................................
     $stack               | The integer-valued memory stack.
     ------------------------------------------------------------------"
  `(defmethod process-the-symbol (($interpreter Interpreter)
                                  ($symbol      (eql ,symbol)))
     (declare (type Interpreter $interpreter))
     (declare (type character   $symbol))
     (with-respect-to-the-skip-flag ($interpreter)
       (with-the-interpreter ($interpreter)
         ,@body))
     (values)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the navigational operations.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\v
  (redirect-the-cursor-to $cursor :down))

;;; -------------------------------------------------------

(define-a-symbol-processor #\^
  (redirect-the-cursor-to $cursor :up))

;;; -------------------------------------------------------

(define-a-symbol-processor #\<
  (redirect-the-cursor-to $cursor :left))

;;; -------------------------------------------------------

(define-a-symbol-processor #\>
  (redirect-the-cursor-to $cursor :right))

;;; -------------------------------------------------------

(define-a-symbol-processor #\[
  (when (top-stack-element-equals-zero-p $interpreter)
    (redirect-the-cursor-to $cursor :left)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\]
  (when (top-stack-element-equals-zero-p $interpreter)
    (redirect-the-cursor-to $cursor :right)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\(
  (when (top-stack-element-equals-zero-p $interpreter)
    (redirect-the-cursor-to $cursor :up)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\)
  (when (top-stack-element-equals-zero-p $interpreter)
    (redirect-the-cursor-to $cursor :down)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\-
  (redirect-the-cursor-to $cursor
    (reflect-on-a-horizontal-mirror
      (cursor-direction $cursor))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\|
  (redirect-the-cursor-to $cursor
    (reflect-on-a-vertical-mirror
      (cursor-direction $cursor))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\?
  (redirect-the-cursor-to $cursor
    (select-a-random-direction)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the numerical operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Processors for the decimal digits "0" through "9".
(loop for digit of-type standard-char across "0123456789" do
  (define-a-symbol-processor digit
    (push-onto-the-stack $interpreter
      (digit-char-p $symbol))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\r
  (push-onto-the-stack $interpreter
    (random 256)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the arithmetic operations.                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (list-of (cons standard-char function))
               +BINARY-OPERATORS+))

;;; -------------------------------------------------------

(defparameter +BINARY-OPERATORS+
  (list
    (cons #\+ #'+)
    (cons #\S #'-)
    (cons #\* #'*)
    (cons #\/ #'divide-the-integers)
    (cons #\% #'supputate-the-integer-remainder))
  "Associates the Flux command representing binary operations with the
   respective functions.")

;;; -------------------------------------------------------

(loop
  for (command-symbol . operator)
    of-type (standard-char . function)
    in      +BINARY-OPERATORS+
  do
    (let ((captured-operator operator))
      (declare (type binary-operation captured-operator))
      (define-a-symbol-processor command-symbol
        (apply-a-binary-operator $interpreter captured-operator))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\=
  (multiple-value-bind (right-operand left-operand)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type integer right-operand))
    (declare (type integer left-operand))
    (push-onto-the-stack $interpreter
      (if (= left-operand right-operand)
        1
        0))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\!
  (push-onto-the-stack $interpreter
    (prog1
      (if (top-stack-element-equals-zero-p $interpreter)
        1
        0)
      (pop-from-the-stack $interpreter))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the stack manipulation operations.         -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\~
  (pop-from-the-stack $interpreter))

;;; -------------------------------------------------------

(define-a-symbol-processor #\:
  (push-onto-the-stack $interpreter
    (peek-into-the-stack $interpreter)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\x
  (let ((number-of-duplicates (pop-from-the-stack $interpreter)))
    (declare (type integer number-of-duplicates))
    (when (plusp number-of-duplicates)
      (setf $stack
        (nconc
          (subseq $stack 0 number-of-duplicates)
          $stack)))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\$
  (multiple-value-bind (top-element next-lower-element)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type integer top-element))
    (declare (type integer next-lower-element))
    (push-onto-the-stack $interpreter top-element)
    (push-onto-the-stack $interpreter next-lower-element)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\{
  (multiple-value-bind (a b c)
      (pop-three-elements-from-the-stack $interpreter)
    (declare (type integer a))
    (declare (type integer b))
    (declare (type integer c))
    (push-onto-the-stack $interpreter b)
    (push-onto-the-stack $interpreter a)
    (push-onto-the-stack $interpreter c)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\}
  (multiple-value-bind (a b c)
      (pop-three-elements-from-the-stack $interpreter)
    (declare (type integer a))
    (declare (type integer b))
    (declare (type integer c))
    (push-onto-the-stack $interpreter a)
    (push-onto-the-stack $interpreter c)
    (push-onto-the-stack $interpreter b)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\c
  (setf $stack NIL))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the input and output operations.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\.
  (format T "~&~d~%"
    (pop-from-the-stack $interpreter))
  (finish-output))

;;; -------------------------------------------------------

(define-a-symbol-processor #\,
  (format T "~c"
    (code-char
      (pop-from-the-stack $interpreter)))
  (finish-output))

;;; -------------------------------------------------------

(define-a-symbol-processor #\&
  (format T "~&Please enter an integer: ")
  (finish-output)
  (push-onto-the-stack $interpreter
    (parse-integer
      (read-line NIL NIL "0")))
  (clear-input))

;;; -------------------------------------------------------

(define-a-symbol-processor #\'
  (format T "~&Please enter a character: ")
  (finish-output)
  (push-onto-the-stack $interpreter
    (char-code
      (read-char NIL NIL #\Null)))
  (clear-input))

;;; -------------------------------------------------------

(define-a-symbol-processor #\"
  (advance-to-the-next-symbol $interpreter)
  (loop
    for current-symbol
      of-type character
      =       (request-the-current-symbol $interpreter)
    until
      (char= current-symbol #\")
    do
      (push-onto-the-stack $interpreter
        (char-code current-symbol))
      (advance-to-the-next-symbol $interpreter)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the stack-and-playfield operations.        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\g
  (multiple-value-bind (x y)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type integer x))
    (declare (type integer y))
    (push-onto-the-stack $interpreter
      (char-code
        (request-the-playfield-symbol-at $playfield
          (specify-a-location x y))))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\p
  (multiple-value-bind (x y z)
      (pop-three-elements-from-the-stack $interpreter)
    (declare (type integer x))
    (declare (type integer y))
    (declare (type integer z))
    (change-the-playfield-symbol-to $playfield
      (specify-a-location x y)
      (code-char          z))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the control flow duction operations.       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-a-symbol-processor #\#
  (setf $skips-next-symbol-p T))

;;; -------------------------------------------------------

(define-a-symbol-processor #\£
  (unless (top-stack-element-equals-zero-p $interpreter)
    (setf $skips-next-symbol-p T)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\j
  (multiple-value-bind (x y)
      (pop-two-elements-from-the-stack $interpreter)
    (declare (type integer x))
    (declare (type integer y))
    (relocate-the-cursor-to $cursor
      (specify-a-location x y)))
  (setf $retains-location-p T)
  (values))

;;; -------------------------------------------------------

(define-a-symbol-processor #\m
  (setf $last-m-location
    (cursor-location $cursor)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\R
  (when $last-m-location
    (relocate-the-cursor-to $cursor $last-m-location)))

;;; -------------------------------------------------------

(define-a-symbol-processor #\l
  (cond
    ((top-stack-element-equals-zero-p $interpreter)
      (advance-to-the-next-symbol $interpreter)
      (loop
        for current-symbol
          of-type character
          =       (request-the-current-symbol $interpreter)
        until
          (char= current-symbol #\e)
        do
          (advance-to-the-next-symbol $interpreter)))
    (T
      (push (cursor-location $cursor) $jump-stack))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\e
  (if (top-stack-element-equals-zero-p $interpreter)
    (pop $jump-stack)
    (relocate-the-cursor-to $cursor
      (first $jump-stack))))

;;; -------------------------------------------------------

(define-a-symbol-processor #\@
  (setf $has-halted-p T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the executive operations.                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-the-current-symbol (interpreter)
  "Evaluates the currently selected symbol from the INTERPRETER's Flux
   playfield and returns no value."
  (declare (type Interpreter interpreter))
  (process-the-symbol interpreter
    (request-the-current-symbol interpreter))
  (values))

;;; -------------------------------------------------------

(defun start-the-interpreter (interpreter)
  "Executes the Flux program concredited to the INTERPRETER's castaldy
   as a playfield and returns no value."
  (declare (type Interpreter interpreter))
  (with-the-interpreter (interpreter)
    (loop until $has-halted-p do
      (process-the-current-symbol interpreter)
      (advance-to-the-next-symbol interpreter)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-flux-code (code)
  "Interprets the piece of Flux source CODE and returns no value."
  (declare (type string code))
  (start-the-interpreter
    (furnish-an-interpreter-for
      (arrange-the-code-in-a-playfield code)))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-flux-code-lines (&rest code-lines)
  "Assembles the CODE-LINES into a playfield, executes the thus formed
   piece of Flux source code, and returns no value."
  (declare (type (list-of string) code-lines))
  (start-the-interpreter
    (furnish-an-interpreter-for
      (arrange-the-code-in-a-playfield
        (apply #'concatenate-the-lines code-lines))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, world!".
(interpret-the-flux-code ">0\"!dlrow ,olleH\"l,e@")

;;; -------------------------------------------------------

;; Print "Hello, World!".
(interpret-the-flux-code-lines
  "<v\"Hello, World!\""
  " >l,e@")

;;; -------------------------------------------------------

;; Repeating cat program which terminates on a "null character" input.
(interpret-the-flux-code ">'l:,'e@")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-the-flux-code-lines
  "&m£vv"
  " l 0R"
  " 1 ."
  " :  "
  " . @"
  " e")

;;; -------------------------------------------------------

;; Choose and print a random integer number from the range [1, 4]
;; naiting the "j" command.
(interpret-the-flux-code-lines
  ">32j"
  "> > >v"
  "  2  v"
  "^1?3 >.@"
  "  4  ^"
  "  > >^")

;;; -------------------------------------------------------

;; Select, in an aleatory fashion, eight (8) characters from the ASCII
;; range [0, 255] and print the same to the standard output conduit.
(interpret-the-flux-code "24*lr,1Se@")

;;; -------------------------------------------------------

;; XKCD random number: always issues one instance of the number four (4)
;; to the standard output.
(interpret-the-flux-code-lines
  ">4.@"
  "Chosen by fair dice roll"
  "Guaranteed to be random")
