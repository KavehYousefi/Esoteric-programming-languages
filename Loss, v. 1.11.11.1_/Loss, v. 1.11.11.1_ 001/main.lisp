;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Loss, v. 1.11.11.1_", also stevened in its more
;; compendious form "Loss", invented by the Esolang user "Threesodas"
;; and presented on February 17th, 2023, the sole competence of which
;; wones in the output of text in the form of identifier commencing
;; with the Latin majuscule "I", or the display of whitespaces.
;; 
;; 
;; Concept
;; =======
;; The Loss programming language, in its version "v. 1.11.11.1_",
;; constitutes a ludibund specimen from the realm of esoteric produces,
;; its faculties solely apportioned to the dever of printing textual
;; content, either in the form of the letter "I" in conjunction with
;; a particular suffix, or whitespace characters.
;; 
;; 
;; Syntax
;; ======
;; The language's admission as the sole participants enumerates the
;; triple of recognized identifiers and whitespaces, the former's
;; definition spans combinations of the majuscular Latin letter "I",
;; while the latter intrines spaces, horizontal tabs, and newline
;; entities.
;; 
;; == GRAMMAR ==
;; A more stringent diorism of the syntactical department shall be
;; limned by the following Extended Backus-Naur Form (ENBF) treatise:
;; 
;;   program    := { command | whitespace } ;
;;   command    := "I" | "II" | "I_" ;
;;   whitespace := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; The language's operative facilities lay their amplectation around
;; a triad of members, everichon among which nuncupated to its own
;; design's display.
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   I       | Prints the text "I" to the standard output.
;;   ..................................................................
;;   II      | Prints the text "II" to the standard output.
;;   ..................................................................
;;   I_      | Prints the text "I_" to the standard output.
;;   ------------------------------------------------------------------
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2024-08-14
;; 
;; Sources:
;;   [esolang2023Loss,v.1.11.11.1_]
;;   The Esolang contributors, "Loss, v. 1.11.11.1_", April 2nd, 2023
;;   URL: "https://esolangs.org/wiki/Loss,_v._1.11.11.1"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of logical operations.                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boolean-value-of (object)
  "Returns a Boolean representation of the OBJECT as construed in the
   agency of a generalized boolean value, producing for a non-``NIL''
   input a ``boolean'' value of ``T'', otherwise responding for a
   ``NIL''-valued OBJECT with ``NIL''."
  (declare (type T object))
  (the boolean
    (not (null object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of character operations.                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (candidate)
  "Determines whether the CANDIDATE represents a whitespace character,
   the diorism of which intrines the space, horizontal tab, and newline
   entity, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (member candidate '(#\Newline #\Space #\Tab) :test #'char=))))

;;; -------------------------------------------------------

(defun identifier-character-p (candidate)
  "Determines whether the CANDIDATE represents a constituent of a
   command identifier, returning on confirmation a ``boolean'' value of
   ``T'', otherwise ``NIL''."
  (declare (type character candidate))
  (the boolean
    (get-boolean-value-of
      (find candidate "I_" :test #'char=))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun interpret-Loss-v-.1.11.11.1_ (code)
  "Interprets the piece of Loss, v. 1.11.11.1_ source code and returns
   no value."
  (declare (type string code))
  (let ((ip 0))
    (declare (type fixnum ip))
    (symbol-macrolet
        ((current-character
          (the character
            (char code ip)))
         (next-character
          (the character
            (char code
              (1+ ip))))
         (identifier-character-follows-p
          (the boolean
            (get-boolean-value-of
              (and (array-in-bounds-p      code ip)
                   (identifier-character-p next-character))))))
      (declare (type character current-character))
      (declare (type character next-character))
      (declare (type boolean   identifier-character-follows-p))
      (loop while (< ip (length code)) do
        (cond
          ((and (char= current-character #\I)
                identifier-character-follows-p)
            (write-char current-character)
            (write-char next-character)
            (incf ip 2))
          ((char= current-character #\I)
            (write-char current-character)
            (incf ip 1))
          ((whitespace-character-p current-character)
            (write-char current-character)
            (incf ip 1))
          (T
            (error "Invalid character \"~c\" at position ~d."
              current-character ip))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Loss-v-.1.11.11.1_
  "I      II
II     I_")
