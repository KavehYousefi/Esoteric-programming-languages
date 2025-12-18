;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Brain#", an alternative to its agnomination, in siccan
;; circumstance to whom appertains certain symbol's disenfranchisement,
;; proposed as "Brainhash", invented by the Esolang user "Esolangist",
;; also known as "Hammy", and presented on November 5th, 2025, its
;; diorism's entelechy that of an encoding applied to Urban Mueller's
;; "brainfuck", edified upon the firmament involving a thrice conversion
;; catena that utilizes the relationship betwixt a digit's character
;; form and its ASCII code in both directions.
;; 
;; 
;; Concept
;; =======
;; The Brain# programming language subsumes into the species of
;; encodings applied to the brainfuck language, its proprium's
;; expression the edification upon several tiers with a concomitant
;; involvement of the ASCII character codes in relationship with the
;; subset of represented decimal digits.
;; 
;; == BRAIN# PROGRAMS: ORDERED CATENAS OF ASCII CODES ==
;; A Brain# program's conformation emerges from the arrangement of
;; certain ASCII codes in an ordered list, each twissel's sepiment
;; enumerated in one or more whitespaces.
;; 
;; == ENCODING: THREE STAGES OF CIPHER SUPPUTATION + ONE COLLECTION ==
;; The encoding process, in its status of efforts' distillation,
;; enumerates a treble dever, extended into a tesseratomy by the
;; desinent cipher codes' collation:
;; 
;;   (1) FIRST-PASS ENCODING: BRAINFUCK TOKEN TO INTEGER IN [0, 7]:
;;       The encoding process' incipiency avaunts from the brainfuck
;;       program's serelepes transcription into an integral object
;;       accompted as an incolant of the range [0, 7], and entailing
;;       the imposition of the following nomothesia's application to
;;       produce the first of three ciphers:
;;       
;;         --------------------------------------------
;;         brainfuck command | First-pass Brain# cipher
;;         ------------------+-------------------------
;;         +                 | 0
;;         ............................................
;;         -                 | 1
;;         ............................................
;;         <                 | 2
;;         ............................................
;;         >                 | 3
;;         ............................................
;;         ,                 | 4
;;         ............................................
;;         .                 | 5
;;         ............................................
;;         [                 | 6
;;         ............................................
;;         ]                 | 7
;;         --------------------------------------------
;;   
;;   (2) SECOND-PASS ENCODING: ASCII CODES OF THE FIRST-PASS CIPHERS:
;;       The second-pass cipher's obtention does not wist of any
;;       contributions induced via a forinsecal or adscititious
;;       governail, merely concluding from the translation of the
;;       first-pass cipher, an integer number in the interval [0, 7],
;;       and construed in this particular moment as a character, into
;;       its corresponding ASCII code:
;;       
;;         -----------------------------------------------------
;;         First-pass cipher | ASCII code (= second-pass cipher)
;;         ------------------+----------------------------------
;;         0                 | 48
;;         .....................................................
;;         1                 | 49
;;         .....................................................
;;         2                 | 50
;;         .....................................................
;;         3                 | 51
;;         .....................................................
;;         4                 | 52
;;         .....................................................
;;         5                 | 53
;;         .....................................................
;;         6                 | 54
;;         .....................................................
;;         7                 | 55
;;         -----------------------------------------------------
;;       
;;       As a corollary, the produced second-pass cipher's compass
;;       limns is accommodated a wonining in the integral interval
;;       [48, 55].
;;   
;;   (3) THIRD-PASS ENCODING: ASCII CODES OF THE SECOND-PASS DIGITS:
;;       A species of consanguinity's compernage valorizes our
;;       acquaintance in the third-pass cipher's supputation: Imprimis,
;;       the recently acquired second-pass cipher, a commorant of the
;;       integer range [48, 55], is segregated into its two digits.
;;       
;;       Each such decimal digit, construed as a character, experiences
;;       a private transcription into its ASCII code, akin to the
;;       first-pass to the second-pass encoding aboon, thus yielding
;;       for each second-pass cipher two third-pass ciphers, everichon
;;       among these twissels itself a member of the subset of ASCII
;;       codes; ensuing from its nature, the first digit, either 4 or
;;       5, admits the interval [52, 53]; while the second digit's
;;       mickleness in variation, producing decimal values of
;;       {0, 1, 2, 3, 4, 5, 8, 9}, furcates into the integral set
;;       {48, 49, 50, 51, 52, 53, 56, 57}:
;;       
;;         -----------------------------------------------------
;;         Second-pass | Second-pass | ASCII codes of the digits
;;         cipher      | digits      | (= third-pass cipher)
;;         ------------+-------------+--------------------------
;;         48          | 4, 8        | 52, 56
;;         .....................................................
;;         49          | 4, 9        | 52, 57
;;         .....................................................
;;         50          | 5, 0        | 53, 48
;;         .....................................................
;;         51          | 5, 1        | 53, 49
;;         .....................................................
;;         52          | 5, 2        | 53, 50
;;         .....................................................
;;         53          | 5, 2        | 53, 51
;;         .....................................................
;;         54          | 5, 2        | 53, 52
;;         .....................................................
;;         55          | 5, 2        | 53, 53
;;         -----------------------------------------------------
;;   
;;   (4) COLLATION:
;;       The encoding process' ultimity is defined in terms of a
;;       Brain# program's gendrure from the collated third-pass ciphers,
;;       presented inwith an ordered sequence of integer numbers desumed
;;       from the set {48, 49, 50, 51, 52, 53, 56, 57}, with no further
;;       merist's intercession betwixt these tokens except for
;;       whitespaces.
;; 
;; The aboon actuated elucidation of the encoding process shall now
;; constitute an enhaused mete of formality's recipiency, molded into
;; a pseudocode treatise's guise:
;; 
;;   function getFirstPassCipher (brainfuckToken)
;;     Input:
;;       brainfuckToken:  One of the eight recognized brainfuck command
;;                        symbols.
;;     
;;     Output:
;;       firstPassCipher: The first-pass cipher corresponding to the
;;                        BRAINFUCK_TOKEN and representing a decimal
;;                        digit in the closed integer interval [0, 7].
;;     
;;     Process:
;;       let firstPassCipher <- nil
;;       
;;       if brainfuckToken = "+" then
;;         firstPassCipher <- 0
;;       else if brainfuckToken = "-" then
;;         firstPassCipher <- 1
;;       else if brainfuckToken = "<" then
;;         firstPassCipher <- 2
;;       else if brainfuckToken = ">" then
;;         firstPassCipher <- 3
;;       else if brainfuckToken = "," then
;;         firstPassCipher <- 4
;;       else if brainfuckToken = "." then
;;         firstPassCipher <- 5
;;       else if brainfuckToken = "[" then
;;         firstPassCipher <- 6
;;       else if brainfuckToken = "]" then
;;         firstPassCipher <- 7
;;       else
;;         error: no brainfuck token
;;       end if
;;       
;;       return firstPassCipher
;;   end function
;;   
;;   function getASCIICode (digit)
;;     Input:
;;       digit:     The decimal digit, interpreted as a character
;;                  rather than a numeric object, whose ASCII code
;;                  shall be determined.
;;     
;;     Output:
;;       asciiCode: The ASCII code for the decimal digit considered as
;;                  a character.
;;     
;;     Process:
;;       let asciiCode <- 48 + digit
;;       
;;       return asciiCode
;;   end function
;;   
;;   function encodeBrainfuckProgram (brainfuckProgram)
;;     Input:
;;       brainfuckProgram: The brainfuck program to encode according to
;;                         the Brainhash standard.
;;     
;;     Output:
;;       brainhashProgram: The encoded BRAINFUCK_PROGRAM as an ordered
;;                         list of integer number, each member an
;;                         incolant of the closed integral interval of
;;                         [48, 55].
;;     
;;     Process:
;;       let brainhashProgram <- empty ordered list
;;       
;;       for character bfToken in brainfuckProgram do
;;         let firstPassCipher  <- getFirstPassCipher(bfToken)
;;         let secondPassCipher <- getASCIICode(firstPassCipher)
;;         let secondPassDigit1 <- first  digit of secondPassCipher
;;         let secondPassDigit2 <- second digit of secondPassCipher
;;         let thirdPassCipher1 <- getASCIICode(secondPassDigit1)
;;         let thirdPassCipher2 <- getASCIICode(secondPassDigit2)
;;         
;;         append thirdPassCipher1 to brainhashProgram
;;         append thirdPassCipher2 to brainhashProgram
;;       end for
;;       
;;       return brainhashProgram
;;   end function
;; 
;; As a supererogative furnishment, the concept involving the extraction
;; of the digits comprising a two-position integer number, as comprises
;; the second pass encoding's dever --- molded into the aboon pseudocode
;; via the variable "secondPassDigit1" and "secondPassDigit2" ---, shall
;; be a further elucidation's recipient:
;; 
;;   function extractDigitTwain (number)
;;     Input:
;;       number:  An two-digit integer number, that is, a value desumed
;;                from the closed interval [10, 99], whose separate
;;                digits shall be extracted.
;;     
;;     Output:
;;       digits:  A tuple which entails the NUMBER's left and right
;;                digit, each such a member of the closed integer
;;                interval [0, 9], in this exact order.
;;     
;;     Process:
;;       let digits     <- nil
;;       let leftDigit  <- floor(number / 10)
;;       let rightDigit <- number - (leftDigit * 10)
;;       
;;       digits         <- (leftDigit, rightDigit)
;;       
;;       return digits
;;   end function
;; 
;; == DECODING: AN ATHWART PROCESS TO THE ENCODING ==
;; The decoding process, from a Brainhash program towards the
;; represented brainfuck code, equiparates into a boustrophedon's
;; owelty, its designment's replication the wike apportioned to the alow
;; pseudocode formulation:
;; 
;;   function getDigitByASCIICode (asciiCode)
;;     Input:
;;       asciiCode: The ASCII code whose digit in its numeric form
;;                  shall be queried. As a corollary, the asciiCode
;;                  must constitute an integer number in the closed
;;                  interval [48, 57].
;;     
;;     Output:
;;       digit:     The digit represented by the ASCII_CODE.
;;     
;;     Process:
;;       let digit <- asciiCode - 48
;;       return digit
;;   end function
;;   
;;   function getBrainfuckCommand (firstPassCipher)
;;     Input:
;;       firstPassCipher:  The Brainhash first-pass, as an integer
;;                         number desumed from the closed interval
;;                         [0, 7], to decode into the corresponding
;;                         brianfuck command symbol.
;;     
;;     Output:
;;       brainfuckCommand: The brainfuck command symbol encoded by the
;;                         Brainhash FIRST_PASS_CIPHER.
;;     
;;     Process:
;;       let brainfuckCommand <- nil
;;       
;;       if firstPassCipher = 0 then
;;         brainfuckCommand <- "+"
;;       else if firstPassCipher = 1 then
;;         brainfuckCommand <- "-"
;;       else if firstPassCipher = 2 then
;;         brainfuckCommand <- "<"
;;       else if firstPassCipher = 3 then
;;         brainfuckCommand <- ">"
;;       else if firstPassCipher = 4 then
;;         brainfuckCommand <- ","
;;       else if firstPassCipher = 5 then
;;         brainfuckCommand <- "."
;;       else if firstPassCipher = 6 then
;;         brainfuckCommand <- "["
;;       else if firstPassCipher = 7 then
;;         brainfuckCommand <- "]"
;;       else
;;         error: no Brainhash first-pass cipher
;;       end if
;;       
;;       return brainfuckCommand
;;   end function
;;   
;;   function decodeBrainhashProgram (brainhashProgram)
;;     Input:
;;       brainhashProgram: The brainhashProgram, as a sequence of
;;                         integer numbers, to decode into the
;;                         equivalent brainfuck code.
;;     
;;     Output:
;;       brainfuckProgram: The brainfuck program, as a string, obtained
;;                         by a decoding of the BRAINHASH_PROGRAM.
;;     
;;     Process:
;;       let brainfuckProgram <- empty string
;;       
;;       while brainhashProgram contains more tokens do
;;         let thirdPassCipher1 <- read next token from brainhashProgram
;;         let thirdPassCipher2 <- read next token from brainhashProgram
;;         let secondPassCipher <- (thirdPassCipher1 * 10)
;;                                 + thirdPassCipher2
;;         let firstPassCipher  <- getDigitByASCIICode(secondPassCipher)
;;         let brainfuckToken   <- getBrainfuckCommand(firstPassCipher)
;;         
;;         append brainfuckToken to brainfuckProgram
;;       end while
;;       
;;       return brainfuckProgram
;;   end function
;; 
;; == BRAIN# AND BRAINFUCK: EQUIPARATION ==
;; The following tabulation's cynosure shall be realized in the
;; equiparation of the octuple brainfuck instruction symbols with the
;; corresponding twissel of third-pass, and thus ultimate,
;; Brain# ciphers.
;; 
;; Please heed the comma's (",") elision as a conceptual merist, to whom
;; no wike's apportionment exists in the Brain# language, thilk instead
;; expresses its reliance upon such secernent agents in one or more
;; whitespaces.
;; 
;;   --------------------------------------------
;;   brainfuck instruction | Final Brain# ciphers
;;   ----------------------+---------------------
;;   +                     | 52 56
;;   ............................................
;;   -                     | 52 57
;;   ............................................
;;   <                     | 53 48
;;   ............................................
;;   >                     | 53 49
;;   ............................................
;;   ,                     | 53 50
;;   ............................................
;;   .                     | 53 51
;;   ............................................
;;   [                     | 53 52
;;   ............................................
;;   ]                     | 53 53
;;   --------------------------------------------
;; 
;; == BRAINFUCK AND BRAIN#: EQUIPARATION ==
;; The following tabulation's dation shall be exercise its delineating
;; power in the juxtaposition of the third-pass, and thus ultimate,
;; Brain# cipher twains and their corresponding brainfuck instruction
;; symbols.
;; 
;; Please heed the comma's (",") elision as a conceptual merist, to whom
;; no wike's apportionment exists in the Brain# language, thilk instead
;; expresses its reliance upon such secernent agents in one or more
;; whitespaces.
;; 
;;   --------------------------------------------
;;   Final Brain# ciphers | brainfuck instruction
;;   ---------------------+----------------------
;;   52 56                | +
;;   ............................................
;;   52 57                | -
;;   ............................................
;;   53 48                | <
;;   ............................................
;;   53 49                | >
;;   ............................................
;;   53 50                | ,
;;   ............................................
;;   53 51                | .
;;   ............................................
;;   53 52                | [
;;   ............................................
;;   53 53                | ]
;;   --------------------------------------------
;; 
;; == THE MEMORY: AN INFINITE TAPE OF UNSIGNED BYTE-VALUED CELLS ==
;; A grateful recipient of its stockfather's cleronomy, Brain#'s memory
;; adheres to the acquainted ipsissima verba appropriation of
;; brainfuck's data castaldy notion in a bilaterally bourneless
;; dispansion of unsigned byte-valued cells, apposted in a seriatim
;; ordonnance.
;; 
;; Each such component's capacity concurs with the integral range of
;; [0, 255], wrapping around any of its marches' jumelle upon a
;; transgression.
;; 
;; Operating upon this tape, a dedicated cursor, the "cell pointer",
;; is apportioned that dever to select any instant the currently
;; active cell, thilk imposing the aefauld unit amenable to
;; perquisitions into and modifications applied to its content. The
;; cell pointer's mobile nature begets a homologation appertaining to
;; its gradual translation along both tape axes in order to alter the
;; cell selection.
;; 
;; 
;; Instructions
;; ============
;; Brain#'s potential, entalented with an equipollence of patration's
;; supputation to brainfuck's octuple instruction set, incarnates a
;; tantamount with only a syntactical disjunction from the entheus.
;; 
;; == OVERVIEW ==
;; The sequela affording the gendrure from the aboon scheme's
;; application, whose product comprises the brainfuck instructions,
;; will be the following description's subject.
;; 
;; The spatial constraints imposed by the documentation's compass
;; serves as certain abbreviations' provenance; the legends to the
;; compendiousness' warklumes, as well as any other file's caption,
;; shall be explicated in the next tabulation:
;; 
;;   ------------------------------------------------------------------
;;   Column title | Interpretation
;;   -------------+----------------------------------------------------
;;   Brain#       | Designates the twain of third-pass, and hence
;;                | ultimate, Brain# ciphers whose correspondence to
;;                | the respective brainfuck instruction is inclavated
;;                | in the "bf" column.
;;   ..................................................................
;;   bf           | The brainfuck instruction identifier associated
;;                | with the third-pass Brain# cipher twain.
;;   ..................................................................
;;   Effect       | The causatum commorant in the brainfuck instruction
;;                | represented in "bf".
;;   ------------------------------------------------------------------
;; 
;; Ensuing from the aboon elucidations, the correspondences betwixt the
;; Brain# encoding and the octuple brainfuck operations shall be the
;; following diorisms' cynosure:
;; 
;;   ------------------------------------------------------------------
;;   Brain# | bf | Effect
;;   -------+----+-----------------------------------------------------
;;   52 56  | +  | Increments the current cell value by one (1). If the
;;          |    | new cell state transcends the admissible upper march
;;          |    | of 255, the value wraps around to the minimum of
;;          |    | zero (0).
;;   ..................................................................
;;   52 57  | -  | Decrements the current cell value by one (1). If the
;;          |    | new cell state transcends the admissible lower march
;;          |    | of zero (0), the value wraps around to the maximum
;;          |    | of 255.
;;   ..................................................................
;;   53 48  | <  | Translates the cell pointer one step to the left.
;;   ..................................................................
;;   53 49  | >  | Translates the cell pointer one step to the right.
;;   ..................................................................
;;   53 50  | ,  | Queries the standard input conduit for a character
;;          |    | and stores its ASCII code in the current cell.
;;   ..................................................................
;;   53 51  | .  | Prints the character whose ASCII code corresponds to
;;          |    | the current cell value to the standard output
;;          |    | conduit.
;;   ..................................................................
;;   53 52  | [  | If the current cell value equals zero (0), moves the
;;          |    | instruction pointer (IP) forward to the matching "]"
;;          |    | token; otherwise proceeds as usual.
;;   ..................................................................
;;   53 53  | ]  | If the current cell value does not equal zero (0),
;;          |    | moves the instruction pointer (IP) backward to the
;;          |    | matching "[" token; otherwise proceeds as usual.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This interpreter implementation has been actuated in the programming
;; language Common Lisp, the focus of its angariation the direct
;; evaluation of the Brain#, whence ensues a transcription into a
;; tantamount brainfuck program, the effort thus translating into a
;; siclike immediacy in the decoded content's consumption.
;; 
;; A kenspeckle eidolon's incarnation recludes itself to the interpreter
;; source code's student, upon a concomitant gnarity with the
;; unambiguousness' governail betwixt the Brain# encoding ciphers and
;; the ensconced brainfuck instructions, according to both airts of
;; perusal: In lieu of an immediate transcription, tabular in its
;; beau ideal manifestation, betwixt the ultimate cipher twissel and
;; the represented brainfuck character, this project's approach resorts
;; to the nimiety of gradual conversions, the perambulation such as to
;; sojourn the first-, second-, and third-pass encoding stages in an
;; explicit effort --- and athwart, from a brainfuck provenance along
;; the decoding principles' medium, to the Brain# equiparation result.
;; 
;; The telos' pursuit for a dioristic inclination towards the convolute
;; solution is indebted to the deictic emoluments incorporated in this
;; supererogation, communicating via the appertaining nortelry in the
;; Brain# language's context a tmema of chrestomathic vallidom.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-12-07
;; 
;; Sources:
;;   [esolang2025:Brainhash]
;;   The Esolang contributors, "Brainhash", November 5th, 2025
;;   URL: "https://esolangs.org/wiki/Brainhash"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of the bespoke types.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype first-pass-cipher ()
  "The ``first-pass-cipher'' type defines the cipher associated with the
   Brain# encoding's first stage as an integral number occupying the
   closed interval [0, 7]."
  '(integer 0 7))

;;; -------------------------------------------------------

(deftype decimal-digit ()
  "The ``decimal-digit'' type defines a decimal digit as an integral
   object occupying the closed interval [0, 9]."
  '(integer 0 9))

;;; -------------------------------------------------------

(deftype ascii-code-of-digit ()
  "The ``ascii-code-of-digit'' type defines a decimal digit's the ASCII
   code representation as an integral value partaking of the closed
   interval [48, 57]."
  '(integer 48 57))

;;; -------------------------------------------------------

(deftype destination ()
  "The ``destination'' type defines a sink for output operations,
   amplecting in this diorism, among other members, the functions
   ``format'' and ``write-char''."
  '(or null (eql T) stream string))

;;; -------------------------------------------------------

(deftype list-of (&optional (element-type '*))
  "The ``list-of'' type defines a singly linked list whose elements
   in their entirety partake of the ELEMENT-TYPE's compliance, for the
   same is imposed the generic sentinel ``*'' as the default's
   governail."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (listp candidate)
            (every
              #'(lambda (current-element)
                  (declare (type T current-element))
                  (typep current-element element-type))
              (the list candidate)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype hash-table-of (&optional (key-type '*) (value-type' *))
  "The ``hash-table-of'' type defines a hash table whose componency's
   edification constitutes an inclavation upon zero or more entries,
   their keys complying with the KEY-TYPE, while the allied values
   subsume into the VALUE-TYPE, for both is imposed the nomothesy of
   the generic sentinel ``*'' as the default."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (candidate)
          (declare (type T candidate))
          (and
            (hash-table-p candidate)
            (loop
              for current-key
                of-type T
                being the hash-keys in (the hash-table candidate)
              using
                (hash-value current-value)
              always
                (and
                  (typep current-key   key-type)
                  (typep current-value value-type))))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype jump-table ()
  "The ``jump-table'' type defines a bidirectional association betwixt
   jump points in a brainfuck program, mediated by adminiculum of their
   zero-based positions into the source code string, and manifested in
   a hash table whose keys and values both assume ``fixnum'' indices."
  '(hash-table-of fixnum fixnum))

;;; -------------------------------------------------------

(deftype octet ()
  "The ``octet'' type defines an unsigned byte value whose conformation
   lays its amplection around eight (8) accolent bits, thus covering
   the closed integral interval of [0, 255]."
  '(unsigned-byte 8))

;;; -------------------------------------------------------

(deftype memory ()
  "The ``memory'' type defines the Brainhash memory as a sparse vector
   of unsigned byte-valued cells, amenable to addresses which represent
   the incolants of the bourneless signed integer vale, and being
   realized as a hash table whose integer keys furnish the indices into
   the allied values thilk accoutre the ``octet''-valued cell states."
  '(hash-table-of integer octet))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the logical operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-as-a-boolean-value (object)
  "Interprets the OBJECT in its aspect as a \"generalized boolean\"
   expression and returns a veridicous Boolean tantamount thereof,
   returning for a non-``NIL'' input a ``boolean'' value of ``T'';
   otherwise, for a ``NIL'' OBJECT, produces ``NIL''."
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
    (interpret-as-a-boolean-value
      (member candidate
        '(9 10 11 12 13 32)
        :key  #'code-char
        :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the string operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-is-empty-p (candidate)
  "Determines whether the CANDIDATE represents the empty string,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type string candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (string= candidate ""))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the tokenizer.                             -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun locate-the-next-token (source start-index)
  "Proceeding from the START-INDEX into the SOURCE, locates the nearest
   following token and returns two values:
     (1) The next token as a fresh string. Upon the SOURCE's inability
         to furnish such, an empty string is produced.
     (2) The position into the SOURCE immediately succeeding the
         extracted token."
  (declare (type string source))
  (declare (type fixnum start-index))
  (let* ((word-start-index
          (or (position-if-not #'whitespace-character-p source
                :start start-index)
              (length source)))
         (word-end-index
          (or (position-if #'whitespace-character-p source
                :start word-start-index)
              (length source))))
    (declare (type fixnum word-start-index))
    (declare (type fixnum word-end-index))
    (the (values string fixnum)
      (values
        (subseq source word-start-index word-end-index)
        word-end-index))))

;;; -------------------------------------------------------

(defmacro with-token-iterator ((driver-name source) &body body)
  "Evaluates the SOURCE, and evaluates the BODY forms, granting these
   the adit to the SOURCE's ensconced tokens by adminiculum of a local
   function of niladic signature, whose agnomination is communicated
   with the DRIVER-NAME, returning upon its invocation the next token
   as a fresh string, and finally returns the desinent BODY form's
   results."
  (let ((evaluated-source   (gensym))
        (search-start-index (gensym))
        (next-token         (gensym)))
    (declare (type symbol evaluated-source))
    (declare (type symbol search-start-index))
    (declare (type symbol next-token))
    `(let ((,evaluated-source   ,source)
           (,search-start-index 0)
           (,next-token         ""))
       (declare (type string ,evaluated-source))
       (declare (type fixnum ,search-start-index))
       (declare (type string ,next-token))
       (flet ((,driver-name ()
                "Locates the next token in the SOURCE and returns the
                 same as a string."
                (multiple-value-setq (,next-token ,search-start-index)
                  (locate-the-next-token
                    ,evaluated-source
                    ,search-start-index))
                (the string ,next-token)))
         ,@body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of the brainfuck identifiers table.               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type (simple-base-string 8) +BRAINFUCK-INSTRUCTIONS+))

;;; -------------------------------------------------------

(defconstant +BRAINFUCK-INSTRUCTIONS+
  (coerce "+-<>,.[]" 'simple-base-string)
  "The ``+BRAINFUCK-INSTRUCTIONS+'' global constant furnishes the
   Brainhash encoding table, its affiliations betwixt a brainfuck
   instruction symbol and its corresponding first-pass Brainhash cipher
   molded into the brainfuck identifier's zero-based position into this
   string.")

;;; -------------------------------------------------------

(defun brainfuck-instruction-p (candidate)
  "Determines whether the CANDIDATE represents one of the recognized
   octuple brainfuck instruction identifiers, returning on confirmation
   a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (interpret-as-a-boolean-value
      (find candidate +BRAINFUCK-INSTRUCTIONS+ :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the brainfuck-to-Brain# encoder.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; brainfuck token to integer in [0, 7].
(defun first-pass-encode-the-instruction (brainfuck-instruction)
  "Encodes the BRAINFUCK-INSTRUCTION into the first-pass cipher, an
   integral datum whose imposition ensues as a consectary of the Brain#
   standard's nomothesia, and returns this object."
  (declare (type standard-char brainfuck-instruction))
  (the first-pass-cipher
    (or (position brainfuck-instruction +BRAINFUCK-INSTRUCTIONS+
          :test #'char=)
        (error "Cannot encode the brainfuck token \"~c\"."
          brainfuck-instruction))))

;;; -------------------------------------------------------

;; Digit in [0, 9] to ASCII code in [48, 57].
(defun second-pass-encode-the-instruction (first-pass-cipher)
  "Encodes the FIRST-PASS-CIPHER into its corresponding second-pass
   cipher tantamount, which resolves to the former's supersession, when
   construed as a character, by its ASCII code."
  (declare (type decimal-digit first-pass-cipher))
  (the ascii-code-of-digit
    (+ 48 first-pass-cipher)))

;;; -------------------------------------------------------

;; ASCII code in [48, 57] to twissel of ASCII code in [48, 57].
(defun third-pass-encode-the-instruction (second-pass-cipher)
  "Encodes the SECOND-PASS-CIPHER, this constituting a decimal digit's
   ASCII code as an integer from the range [48, 57], into a twissel of
   ASCII codes by a segregation of the SECOND-PASS-CIPHER's two ASCII
   code digits and a subsequent transcription of these separate entities
   themselves, construed as decimal digits, into the ASCII realm, and
   returns two values:
     (1) The ASCII code of the first digit constituting the
         SECOND-PASS-CIPHER.
     (2) The ASCII code of the second digit constituting the
         SECOND-PASS-CIPHER."
  (declare (type ascii-code-of-digit second-pass-cipher))
  (multiple-value-bind (first-digit second-digit)
      (floor second-pass-cipher 10)
    (declare (type (integer 4 5) first-digit))
    (declare (type (integer 0 9) second-digit))
    (the (values ascii-code-of-digit ascii-code-of-digit)
      (values
        (second-pass-encode-the-instruction first-digit)
        (second-pass-encode-the-instruction second-digit)))))

;;; -------------------------------------------------------

(defun encode-the-brainfuck-code (code &key (destination NIL))
  "Converts the piece of brainfuck source CODE into its Brain# encoding
   and writes the resul to the DESTINATION, returning for a non-``NIL''
   DESTINATION the ``NIL'' value; otherwise, for a ``NIL'' DESTINATION,
   responds with a fresh string comprehending the encoded content."
  (declare (type string      code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (loop
        with first-output-p of-type boolean   = T
        
        for  current-token  of-type character across code
        
        when (brainfuck-instruction-p current-token) do
          (multiple-value-bind (left-cipher right-cipher)
              (third-pass-encode-the-instruction
                (second-pass-encode-the-instruction
                  (first-pass-encode-the-instruction current-token)))
            (declare (type ascii-code-of-digit left-cipher))
            (declare (type ascii-code-of-digit right-cipher))
            (format destination "~@[ ~*~]~d ~d"
              (not first-output-p)
              left-cipher
              right-cipher)
            (setf first-output-p NIL)))
      (with-output-to-string (encoded-program)
        (declare (type string-stream encoded-program))
        (encode-the-brainfuck-code code
          :destination encoded-program)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the Brain#-to-brainfuck decoder.           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-the-digit-by-its-ascii-code (character-code)
  "Returns the integral value of the decimal digit represented by its
   ASCII CHARACTER-CODE."
  (declare (type ascii-code-of-digit character-code))
  (the decimal-digit
    (digit-char-p
      (code-char character-code))))

;;; -------------------------------------------------------

;; Example: (53, 50) => 52
(defun decode-the-third-pass-cipher (third-pass-cipher-1
                                     third-pass-cipher-2)
  "Decodes the third-pass cipher, represented by the moeity's twain of
   THIRD-PASS-CIPHER-1 and THIRD-PASS-CIPHER-2, which constitute the
   ASCII codes of the decimal digits thilk, combined into a new
   two-digit number, form the second-pass cipher, itself an ASCII code,
   and returns the same."
  (declare (type ascii-code-of-digit third-pass-cipher-1))
  (declare (type ascii-code-of-digit third-pass-cipher-2))
  (the ascii-code-of-digit
    (+ (* (get-the-digit-by-its-ascii-code third-pass-cipher-1) 10)
       (get-the-digit-by-its-ascii-code third-pass-cipher-2))))

;;; -------------------------------------------------------

;; Example: 52 => 4
(defun decode-the-second-pass-cipher (second-pass-cipher)
  "Decodes the SECOND-PASS-CIPHER and returns the thus yielded
   first-pass cipher, this accompting to an equiparation with the
   SECOND-PASS-CIPHER, interpreted as an ASCII code, to its
   corresponding digit in its actual numeric form."
  (declare (type ascii-code-of-digit second-pass-cipher))
  (the first-pass-cipher
    (get-the-digit-by-its-ascii-code second-pass-cipher)))

;;; -------------------------------------------------------

;; Example: 4 => ","
(defun decode-the-first-pass-cipher (first-pass-cipher)
  "Decodes the FIRST-PASS-CIPHER into the corresponding brainfuck
   instruction token as imposed by the Brainhash language standard's
   nomothesia."
  (declare (type first-pass-cipher first-pass-cipher))
  (the standard-char
    (schar +BRAINFUCK-INSTRUCTIONS+ first-pass-cipher)))

;;; -------------------------------------------------------

(defun parse-the-ascii-code-representing-a-digit (source)
  "Parses the SOURCE, expecting thilk to represent a decimal digit's
   ASCII code, that is, an incolant of the closed integer interval
   [48, 57], on success returning the extracted code; otherwise signals
   an error of an unspecified type."
  (declare (type string source))
  (let ((extracted-integer (parse-integer source)))
    (declare (type integer extracted-integer))
    (the ascii-code-of-digit
      (if (<= 48 extracted-integer 57)
        extracted-integer
        (error "The number ~d does not represent a decimal digits ~
                ASCII code."
          extracted-integer)))))

;;; -------------------------------------------------------

(defun decode-the-brainhash-code (code &key (destination NIL))
  "Decodes the piece of Brainhash source CODE into its equivalent
   brainfuck program, writes thilk to the DESTINATION, and returns for
   a non-``NIL'' DESTINATION the ``NIL'' value; otherwise produces a
   fresh string comprehending the output."
  (declare (type string      code))
  (declare (type destination destination))
  (the (or null string)
    (if destination
      (with-token-iterator (request-the-next-token code)
        (flet ((request-the-next-third-pass-cipher ()
                "Reads the next token from the CODE, if thilk is not
                 exhausted, parses the same as a decimal digit's ASCII
                 code, and returns the resulting integral object;
                 otherwise the ``NIL'' sentinel is produced in order to
                 represent the absence of any further content."
                (the (or null ascii-code-of-digit)
                  (let ((next-token (request-the-next-token)))
                    (declare (type string next-token))
                    (unless (string-is-empty-p next-token)
                      (parse-the-ascii-code-representing-a-digit
                        next-token))))))
          (loop
            for third-pass-cipher-1
              of-type (or null ascii-code-of-digit)
              =       (request-the-next-third-pass-cipher)
            for third-pass-cipher-2
              of-type (or null ascii-code-of-digit)
              =       (request-the-next-third-pass-cipher)
            while third-pass-cipher-1 do
              (format destination "~c"
                (decode-the-first-pass-cipher
                  (decode-the-second-pass-cipher
                    (decode-the-third-pass-cipher
                      third-pass-cipher-1
                      third-pass-cipher-2)))))))
      (with-output-to-string (brainfuck-code)
        (declare (type string-stream brainfuck-code))
        (decode-the-brainhash-code code :destination brainfuck-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the jump table.                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-an-empty-jump-table ()
  "Creates and returns a ``jump-table'' whose state at its inchoacy
   ostends a perfect vacancy."
  (the jump-table
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun connect-the-jump-points (table start-point end-point)
  "Associates the jump START-POINT with the END-POINT in a bidirectional
   fashion, stores the twissel in the jump TABLE, and returns no value."
  (declare (type jump-table table))
  (declare (type fixnum     start-point))
  (declare (type fixnum     end-point))
  (psetf
    (gethash start-point table) end-point
    (gethash end-point   table) start-point)
  (values))

;;; -------------------------------------------------------

(defun supputate-the-jump-table-for (brainfuck-code)
  "Creates and returns a fresh ``jump-table'' which associates the
   jumelles of jump points in the piece of BRAINFUCK-CODE by adminiculum
   of their zero-based positions in the source code."
  (declare (type string brainfuck-code))
  (let ((jump-table   (prepare-an-empty-jump-table))
        (start-points NIL))
    (declare (type jump-table       jump-table))
    (declare (type (list-of fixnum) start-points))
    (loop
      for current-token    of-type character across brainfuck-code
      and current-position of-type fixnum    from   0 by 1
      do
        (case current-token
          (#\[
            (push current-position start-points))
          (#\]
            (if start-points
              (connect-the-jump-points
                jump-table
                (pop start-points)
                current-position)
              (error "Unmatched back jump point at position ~d."
                current-position)))
          (otherwise
            NIL))
      finally
        (when start-points
          (error "Unmatched forward jump point~p at position~:p ~
                  ~{~d~^, ~}."
            (length start-points)
            start-points)))
    (the jump-table jump-table)))

;;; -------------------------------------------------------

(defun locate-the-jump-destination (jump-table point-of-origin)
  "Returns the obverse jump point associated with the POINT-OF-ORIGIN
   in the JUMP-TABLE; or, upon its disrespondency, signals an error of
   an unspecified type."
  (declare (type jump-table jump-table))
  (declare (type fixnum     point-of-origin))
  (the fixnum
    (or (gethash point-of-origin jump-table)
        (error "No destination jump point is associated with the ~
                position ~d."
          point-of-origin))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the memory operations.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepare-a-pristine-memory ()
  "Creates and returns a fresh ``memory'' instance whose state at this
   point of inchoacy resort to the default of zero-valued cells."
  (the memory
    (make-hash-table :test #'eql)))

;;; -------------------------------------------------------

(defun cell-value-at (memory address)
  "Returns the byte value stored in the MEMORY cell amenable to the
   ADDRESS."
  (declare (type memory  memory))
  (declare (type integer address))
  (the octet
    (gethash address memory 0)))

;;; -------------------------------------------------------

(defun (setf cell-value-at) (new-value memory address)
  "Stores the NEW-VALUE in the MEMORY cell amenable to the ADDRESS,
   contingently yarked by a wrapping into the admissible unsigned byte
   range of [0, 255], and returns no value."
  (declare (type integer new-value))
  (declare (type memory  memory))
  (declare (type integer address))
  (setf (gethash address memory 0)
    (mod new-value 256))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of the interpreter.                           -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type character +NULL-CHARACTER+))

;;; -------------------------------------------------------

(defconstant +NULL-CHARACTER+
  (code-char 0)
  "Represents the \"null character\", associated with the ASCII code of
   zero (0), in an implementation-indepedent fashion.")

;;; -------------------------------------------------------

(defun interpret-the-brainfuck-code (brainfuck-code)
  "Interprets the piece of BRAINFUCK-CODE and returns no value."
  (declare (type string brainfuck-code))
  (let ((ip           0)
        (jump-table   (supputate-the-jump-table-for brainfuck-code))
        (memory       (prepare-a-pristine-memory))
        (cell-pointer 0))
    (declare (type fixnum     ip))
    (declare (type jump-table jump-table))
    (declare (type memory     memory))
    (declare (type integer    cell-pointer))
    (symbol-macrolet
        ((current-cell-value
          (the (or octet integer)
            (cell-value-at memory cell-pointer)))
         (current-token
          (the character
            (char brainfuck-code ip)))
         (program-is-exhausted-p
          (the boolean
            (not
              (array-in-bounds-p brainfuck-code ip)))))
      (declare (type (or octet integer) current-cell-value))
      (declare (type character          current-token))
      (declare (type boolean            program-is-exhausted-p))
      (loop until program-is-exhausted-p do
        (case current-token
          (#\<
            (decf cell-pointer))
          (#\>
            (incf cell-pointer))
          (#\+
            (incf current-cell-value))
          (#\-
            (decf current-cell-value))
          (#\[
            (when (zerop current-cell-value)
              (setf ip
                (locate-the-jump-destination jump-table ip))))
          (#\]
            (unless (zerop current-cell-value)
              (setf ip
                (locate-the-jump-destination jump-table ip))))
          (#\.
            (format T "~c"
              (code-char current-cell-value)))
          (#\,
            (format T "~&>> ")
            (finish-output)
            (setf current-cell-value
              (char-code
                (read-char NIL NIL +NULL-CHARACTER+)))
            (clear-input))
          (otherwise
            NIL))
        (incf ip))))
  (values))

;;; -------------------------------------------------------

(defun interpret-the-Brainhash-code (code)
  "Interprets the piece of Brainhash source CODE and returns no value."
  (declare (type string code))
  (interpret-the-brainfuck-code
    (decode-the-Brainhash-code code))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Execute the repeating cat program in brainfuck, tantamount to:
;;   ,[.,]
(interpret-the-Brainhash-code "53 50 53 52 53 51 53 50 53 53")

;;; -------------------------------------------------------

;; Execute the truth-machine in brainfuck, tantamount to:
;;   ,.[-->+[>>]<[.]<<]
(interpret-the-Brainhash-code
  "53 50 53 51 53 52 52 57 52 57 53 49 52 56 53 52 53 49
   53 49 53 53 53 48 53 52 53 51 53 53 53 48 53 48 53 53")

;;; -------------------------------------------------------

;; This "Hello, World!" program constitutes a translated dation from
;; the brainfuck equivalent:
;;   +[-->-[>>+>-----<<]<--<---]>-.>>>+.>>..+++[.>]<<<<.+++.------.<<-.>>>>+.
(interpret-the-Brainhash-code
  "52 56 53 52 52 57 52 57 53 49 52 57 53 52 53 49 53 49
   52 56 53 49 52 57 52 57 52 57 52 57 52 57 53 48 53 48
   53 53 53 48 52 57 52 57 53 48 52 57 52 57 52 57 53 53
   53 49 52 57 53 51 53 49 53 49 53 49 52 56 53 51 53 49
   53 49 53 51 53 51 52 56 52 56 52 56 53 52 53 51 53 49
   53 53 53 48 53 48 53 48 53 48 53 51 52 56 52 56 52 56
   53 51 52 57 52 57 52 57 52 57 52 57 52 57 53 51 53 48
   53 48 52 57 53 51 53 49 53 49 53 49 53 49 52 56 53 51")

;;; -------------------------------------------------------

;; Yield the repeating cat program in brainfuck:
;;   ,[.,]
(decode-the-brainhash-code "53 50 53 52 53 51 53 50 53 53")

;;; -------------------------------------------------------

;; Encode the repeating cat program from brainfuck into Brain#:
;;   53 50 53 52 53 51 53 50 53 53
(encode-the-brainfuck-code ",[.,]")
