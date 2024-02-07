;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Tuvars", invented by the Esolang user "SpaceByte" on July
;; 12th, 2022 and presented on the same day, the diorism of which
;; relates to its employment of a variables' twain, each capaciated for
;; a signed integer or floating-point number's castaldy, in order to
;; manipulate the console.
;; 
;; 
;; Concept
;; =======
;; The Tuvars programming language's kenspeckle proprium resides in the
;; utilization of a variable jumelle, "a" and "b" in their stevening,
;; for the manipulation of a graphical console, including input, output,
;; and several control flow constructs.
;; 
;; == THE PROGRAM MEMORY: TWO VARIABLES ==
;; The entirety of Tuvar's memory concept experiences its exhaustion in
;; a twissel of variables, fixated with the designations "a" and "b",
;; and entalented with such a capacity as to maintain a scalar integer
;; or floating-point number each.
;; 
;; == TUVARS FILES ARE IDENTIFIED BY ".tux" ==
;; The official file extension in affiliation with Tuvars source files
;; constitutes ".tux".
;; 
;; 
;; Syntax
;; ======
;; A Tuvars program's syntactical conformation proceeds by means of
;; lines, each one among these either a single instruction's commorancy,
;; a commentary expanse, or a blank member.
;; 
;; == INSTRUCTIONS ==
;; Each line not dedicated to vacancy or a commentary purpose embraces
;; at most a single instruction, its parasceve an identifier's
;; accommodation, whence proceeds zero or one argument.
;; 
;; == COMMENTS ==
;; The contingency for explicit comments tallies among the language's
;; vouchsafements, such supplementation's introduction instigated by
;; means of a slash jumelle, "//", and extending to the end of the line.
;; 
;; As an instrument of supererogation, any content not representative of
;; an instruction is considered an ineffectual contribution, and as thus
;; a comment's tantamount.
;; 
;; 
;; Instructions
;; ============
;; Tuvar's instruction set enumerates a tally of eleven members, to whom
;; the competences of character input, output in a character, numeric,
;; or arbitrary string format, as well as the interface console's
;; manpulation, and three control flow helming mechanisms are
;; vouchsafed.
;; 
;; Any instruction deviating from the acceptable conformation is
;; disqualified from effectivity, yet adhibited the vallidom and purpose
;; of a commentary unit.
;; 
;; == OVERVIEW ==
;; An apercu shall invest the reader with a cursory ilk of nortelry's
;; adhibition.
;; 
;; Please heed the following aspects:
;; 
;;   (1) Succedaneous segments by asterisks ("*"), intended for the
;;       supersession via actual code in the ultimate Tuvars program.
;;   (2) Lines deprived of a recognized command name as a parasceuastic
;;       element are admitted, yet assigned a commentary purpose.
;; 
;;   ------------------------------------------------------------------
;;   Command        | Effect
;;   ---------------+--------------------------------------------------
;;   print string   | Prints the {string} to the standard output, not
;;         ******   | succeeded by any linebreak.
;;                  |--------------------------------------------------
;;                  | {string} comprehends the entire character
;;                  | sequence succeeding the spaces after the "print"
;;                  | command identifier, not including the contingent
;;                  | newline character at the line's desinent. No
;;                  | particular designation of this string literal is
;;                  | required.
;;   ..................................................................
;;   char varName   | Prints the character whose ASCII code matches
;;        *******   | the value of the variable designated by {varName}
;;                  | to the standard output.
;;                  |--------------------------------------------------
;;                  | {varName} must be the name of a recognized
;;                  | variable, that is, either "a" or "b".
;;   ..................................................................
;;   number varName | Prints the verbatim numeric value stored by the
;;          ******* | variable designated by {varName} to the standard
;;                  | output.
;;                  |--------------------------------------------------
;;                  | {varName} must be the name of a recognized
;;                  | variable, that is, either "a" or "b".
;;   ..................................................................
;;   linebreak      | Prints a linebreak to the standard output.
;;   ..................................................................
;;   read varName   | Queries the standard input for a character and
;;        *******   | stores its ASCII code in the variable designated
;;                  | by {varName}.
;;                  |--------------------------------------------------
;;                  | {varName} must be the name of a recognized
;;                  | variable, that is, either "a" or "b".
;;   ..................................................................
;;   title text     | Sets the console window title to the {text}.
;;         ****     |--------------------------------------------------
;;                  | {text} comprehends the entire character sequence
;;                  | succeeding the spaces after the "title" command
;;                  | identifier, not including the contingent newline
;;                  | character at the line's desinent. No particular
;;                  | designation of this string literal is required.
;;   ..................................................................
;;   clear          | Clears the console window's content.
;;   ..................................................................
;;   close          | Closes the console window, concomitantly
;;                  | terminating the Tuvars program.
;;   ..................................................................
;;   goto lineNo    | Relocates the instruction pointer (IP) to the
;;        ******    | line located at the one-based line number
;;                  | {lineNo}.
;;                  |--------------------------------------------------
;;                  | If {lineNo} designates an invalid line index, the
;;                  | program immediately terminates.
;;                  |--------------------------------------------------
;;                  | {lineNo} must be a signed or unsigned integer
;;                  | literal.
;;   ..................................................................
;;   if varName     | If the value of the variable designated by
;;      *******     | {varName} does not equal zero (0), executes the
;;                  | next line, otherwise skips the same.
;;                  |--------------------------------------------------
;;                  | {varName} must be the name of a recognized
;;                  | variable, that is, either "a" or "b".
;;   ..................................................................
;;   ifnot varName  | If the value of the variable designated by
;;         *******  | {varName} equals zero (0), executes the next
;;                  | line, otherwise skips the same.
;;                  |--------------------------------------------------
;;                  | {varName} must be the name of a recognized
;;                  | variable, that is, either "a" or "b".
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; This implementation has been realized in the programming language
;; Common Lisp, the natural lacuna of a user interface console being
;; alleviated by a dedicated textual emulation.
;; 
;; 
;; Appendices
;; ==========
;; Certain topics' participation wones in the crepuscle betwixt
;; immediacy in pertinence and inferior significance anenst their
;; contribution to this documentation. Such elements, as a consectary,
;; have been relocated to this parhedral section.
;; 
;; == APPENDIX A: PROJECT FILES ==
;; The nimiety maintaining its commorancy in this project, with a
;; special vista upon the console's emulation, vindicated the
;; interpreter's distribution across multiple files.
;; 
;; The coming table's dever shall therefore be a cursory presentation
;; applied to the files an their agencies, respecting the order of their
;; deployment during the loading.
;; 
;; Please note that at least one infrastructure of nearly official
;; weight exists for such project management purposes, realized in the
;; definition of file interfaces using packages, and their orders and
;; relationships' enunciation by the "ASDF" system. This simple example,
;; however, has been adjudged as rather inflicted with a digressive
;; cumbrance in an advenient structuring's adhibition, rather than its
;; enjoying --- a more serious enterprise certainly would be assayed in
;; an athwart airt.
;; 
;;   ------------------------------------------------------------------
;;   No. | File             | Purpose
;;   ----+------------------+------------------------------------------
;;    1  | types.lisp       | Defines the custom types deployed in the
;;       |                  | the program and utilized by the
;;       |                  | subsequent project files, including,
;;       |                  | among others, ``list-of'',
;;       |                  | ``destination'', etc.
;;   ..................................................................
;;    2  | logic.lisp       | Implements the logical operations, such
;;       |                  | as the conversion of an arbitrary to a
;;       |                  | Boolean truth value.
;;   ..................................................................
;;    3  | characters.lisp  | Furnishes operations dedicated to the
;;       |                  | perquisition and manipulation of
;;       |                  | characters.
;;   ..................................................................
;;    4  | strings.lisp     | Comprehends operations pursuing the
;;       |                  | handling of strings.
;;   ..................................................................
;;    5  | text-layout.lisp | Realizes routines for string objects'
;;       |                  | partitioning into lines of a constrained
;;       |                  | extent, such is utible, in a paravaunt
;;       |                  | assessment, for the formatted output of a
;;       |                  | text-based console.
;;   ..................................................................
;;    6  | console.lisp     | Defines the interface, and a textual
;;       |                  | manifestation, of the program console.
;;   ..................................................................
;;    7  | lexer.lisp       | Furnishes the lexical analyzer, or lexer,
;;       |                  | the perimeter of which enumerates
;;       |                  | facilities for the recognition and
;;       |                  | extraction of significant objects from a
;;       |                  | piece of Tuvars source code in a string
;;       |                  | form.
;;   ..................................................................
;;    8  | commands.lisp    | Comprehends the class definitions that
;;       |                  | map to the Tuvars commands.
;;   ..................................................................
;;    9  | parser.lisp      | Implements the parser, the same assembles
;;       |                  | an executable Tuvars program by the
;;       |                  | lexer's adminiculum.
;;   ..................................................................
;;   10  | interpreter.lisp | Provides the interpreter as the entity
;;       |                  | entrusted with the application of actual
;;       |                  | effect to an executable Tuvars program.
;;   ..................................................................
;;   11  | tests.lisp       | Accoutres the test cases to mete the
;;       |                  | interpreter against.
;;   ..................................................................
;;   12  | main.lisp        | Establishes the project's entrance point
;;       |                  | by its import of the required Common Lisp
;;       |                  | source files in concord with the expected
;;       |                  | arrangement.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-02-06
;; 
;; Sources:
;;   [esolang2022Tuvars]
;;   The Esolang contributors, "Tuvars", July 12th, 2022
;;   URL: "https://esolangs.org/wiki/Tuvars"
;;   
;;   [oracle2023javaapiLineBreakMeasurer]
;;   Oracle America, Inc., "LineBreakMeasurer (Java SE 21 & JDK 21)",
;;     Version 21, September 2023
;;   URL: "https://docs.oracle.com/en/java/javase/21/docs/api/
;;         java.desktop/java/awt/font/LineBreakMeasurer.html"
;;   Notes:
;;     - Specification of the Java class "LineBreakMeasurer", dedicated
;;       to the supputation of line breaks for fragmenting a piece of
;;       text, whence are issued separate "TextLayout" instances, each
;;       such a row's encapsulation.
;;   
;;   [oracle2023javaapiTextLayout]
;;   Oracle America, Inc., "TextLayout (Java SE 21 & JDK 21)",
;;     Version 21, September 2023
;;   URL: "https://docs.oracle.com/en/java/javase/21/docs/api/
;;         java.desktop/java/awt/font/TextLayout.html"
;;   Notes:
;;     - Specification of the Java class "TextLayout", its bailiwick the
;;       encapsulation of a coherent piece of formatted text, including
;;       positioning and graphical representations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype file-source ()
  "The ``file-source'' type defines a source for an external file's
   obtention."
  '(or pathname stream string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of global variables and constants.                -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (type file-source +PROJECT-DIRECTORY+))

;;; -------------------------------------------------------

(defparameter +PROJECT-DIRECTORY+
  (make-pathname)
  "Defines the project directory, the circumference of which lays its
   amplectation around the Common Lisp source files.
   ---
   Please substitute the +PROJECT-DIRECTORY+ path by your personal
   directory which comprehends the requisite Common Lisp source files
   for the interpreter.
   ---
   Several facilities are offered by the Common Lisp standard library
   for engaging in such an activity, enumerating, for instance:
   
     ------------------------------------------------------------
     Function         | Exemplary invocation
     -----------------+------------------------------------------
     make-path-name   | (make-pathname
                      |   :device    \"C\"
                      |   :directory '(:absolute
                      |                 \"Users\"
                      |                 \"Kaveh\"
                      |                 \"Tuvars\"
                      |                 \"Tuvars_002\"))
     ............................................................
     parse-namestring | (parse-namestring
                      |   \"C:/Users/Kaveh/Tuvars/Tuvars_002/\")
     ------------------------------------------------------------")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of file access operations.                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun import-file (file-path)
  "Loads the Common Lisp source file from the FILE-PATH, evaluates and,
   and returns no value."
  (declare (type file-source file-path))
  (load (merge-pathnames +PROJECT-DIRECTORY+ file-path))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Import of project files.                                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import-file "types.lisp")
(import-file "logic.lisp")
(import-file "characters.lisp")
(import-file "strings.lisp")
(import-file "text-layout.lisp")
(import-file "console.lisp")
(import-file "lexer.lisp")
(import-file "commands.lisp")
(import-file "parser.lisp")
(import-file "interpreter.lisp")
(import-file "tests.lisp")
