;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements a script for generating the Common Lisp code
;; capacitated to register the chemical elements.
;; 
;; The entries establishing the base for the element creation have been
;; appropriated from the Los Alamos National Laboratory
;; [losAlamos2021ptoeList].
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2023-07-19
;; 
;; Sources:
;;   [acs2023periodictable]
;;   American Chemical Society (ACS),
;;     "Periodic Table of Elements - American Chemical Society", 2023
;;   URL: "https://www.acs.org/education/whatischemistry/
;;         periodictable.html"
;;   Notes:
;;     - Provides information about the periodic table of the elements,
;;       including the structure of each table cell.
;;     - Presents the same table in several formats for viewing and
;;       downloading.
;;   
;;   [boudreaux2021atomicNumbers]
;;   Kevin A. Boudreaux , "Atomic Numbers", 2021
;;   URL: "https://www.angelo.edu/faculty/kboudrea/periodic/
;;         structure_numbers.htm"
;;   Notes:
;;     - Describes the periodic table of the elements.
;;     - Lists the chemical elements in ascending order of their atomic
;;       numbers.
;;   
;;   [esolang2023PTotE]
;;   The Esolang contributors, "PTotE", 2023
;;   URL: "https://esolangs.org/wiki/PTotE"
;;   
;;   [losAlamos2021ptoeAbout]
;;   Los Alamos National Laboratory,
;;     "Periodic Table of Elements: Los Alamos National Laboratory",
;;     2021
;;   URL: "https://periodic.lanl.gov/about.shtml"
;;   Notes:
;;     - Provides information about the periodic table of the elements.
;;   
;;   [losAlamos2021ptoeList]
;;   Los Alamos National Laboratory,
;;     "Periodic Table of Elements: Los Alamos National Laboratory",
;;     2021
;;   URL: "https://periodic.lanl.gov/list.shtml"
;;   Notes:
;;     - Lists the elements with their symbol and atomic number.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype tokenizer ()
  "The ``tokenizer'' type defines a niladic function which upon its
   invocation returns the next token from its source, this either
   constituting a string representation of a whitespace-delimited word
   or, upon the source exhaustion, the ``NIL'' object."
  '(function () (or null string)))

;;; -------------------------------------------------------

(deftype tokenizer-function ()
  "The ``tokenizer-function'' type defines a ``tokenizer'' in a form
   eligible for type specifiers, that is, as a general ``function'',
   omitting the argument list and result type definitions."
  'function)

;;; -------------------------------------------------------

(deftype positive-integer ()
  "The ``positive-integer'' type defines an integer number greater than
   or equal to one (1), fitten, in particular, for the representation of
   the chemical elements' atomic numbers."
  '(integer 1 *))



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

(defun end-of-word-p (source position)
  "Determines whether the POSITION into the SOURCE designates the end
   of a word, which encompasses either a location outside of the
   SOURCE's boundaries or a whitespace character, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type string source))
  (declare (type fixnum position))
  (the boolean
    (not (null
      (or
        (>= position (length source))
        (whitespace-character-p
          (char source position)))))))

;;; -------------------------------------------------------

(defun skip-whitespaces (source start)
  "Proceeding from the START position into the SOURCE, skips a sequence
   of zero or more accolent whitespace characters and returns the
   position into the SOURCE immediately following the first
   non-whitespace entity."
  (declare (type string source))
  (declare (type fixnum start))
  (the fixnum
    (loop
      for position of-type fixnum from start below (length source)
      while (whitespace-character-p (char source position))
      finally
        (return position))))

;;; -------------------------------------------------------

(defun read-word (source start)
  "Proceeding from the START position into the SOURCE, reads a word,
   delimited by whitespaces or the end of the SOURCE, and returns two
   values:
     (1) The thus consumed word as a string.
     (2) The position into the SOURCE immediately succeeding the
         desinent character of the consumed word."
  (declare (type string source))
  (declare (type fixnum start))
  (the (values string fixnum)
    (with-open-stream (word (make-string-output-stream))
      (declare (type string-stream word))
      (loop
        for   position of-type fixnum from start by 1
        until (end-of-word-p source position)
        do    (write-char (char source position) word)
        finally
          (return
            (values
              (get-output-stream-string word)
              position))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of tokenizer.                                 -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tokenizer (source)
  "Returns a ``tokenizer'' which analyzes the SOURCE while pursuing to
   extract its words.
   ---
   The thus created ``tokenizer'' object constitutes a niladic function
   that upon each invocation responds with the next word from the
   SOURCE, these token beings delineated by whitespaces. Upon its
   SOURCE's, any request is answered with the ``NIL'' value. The
   signature hence conforms to:
     lambda () => (or null string)"
  (declare (type string source))
  (let ((position 0))
    (declare (type fixnum position))
    (the tokenizer-function
      #'(lambda ()
          (setf position (skip-whitespaces source position))
          (the (or null string)
            (when (< position (length source))
              (multiple-value-bind (word new-position)
                  (read-word source position)
                (declare (type string word))
                (declare (type fixnum new-position))
                (setf position new-position)
                word)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of chemical element.                          -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Element
  (:constructor make-element (symbol name atomic-number)))
  "The ``Element'' class represents a chemical element, compact of its
   identifying symbol, name, and atomic number.
   ---
   The fact of its slots' strigent immutability renders any instance of
   this structure class effectively immune to modifications."
  (symbol        (error "Missing symbol.")
                 :type      string
                 :read-only T)
  (name          (error "Missing name.")
                 :type      string
                 :read-only T)
  (atomic-number (error "Missing atomic number.")
                 :type      positive-integer
                 :read-only T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of element list printer.                      -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((tk (make-tokenizer "
Ac	Actinium	89		Md	Mendelevium	101
Al	Aluminum	13		Hg	Mercury	80
Am	Americium	95		Mo	Molybdenum	42
Sb	Antimony	51		Ns	Neilsborium	107
Ar	Argon	18		Nd	Neodymium	60
As	Arsenic	33		Ne	Neon	10
At	Astatine	85		Np	Neptunium	93
Ba	Barium	56		Nh	Nihonium	113
Bk	Berkelium	97		Ni	Nickel	28
Be	Beryllium	4		Nb	Niobium	41
Bi	Bismuth	83		N	Nitrogen	7
Bh	Bohrium	107		No	Nobelium	102
B	Boron	5		Og	Oganesson	118
Br	Bromine	35		Os	Osmium	76
Cd	Cadmium	48		O	Oxygen	8
Ca	Calcium	20		Pd	Palladium	46
Cf	Californium	98		P	Phosphorus	15
C	Carbon	6		Pt	Platinum	78
Ce	Cerium	58		Pu	Plutonium	94
Cs	Cesium	55		Po	Polonium	84
Cl	Chlorine	17		K	Potassium	19
Cr	Chromium	24		Pr	Praseodymium	59
Co	Cobalt	27		Pm	Promethium	61
Cn	Copernicium	112		Pa	Protactinium	91
Cu	Copper	29		Ra	Radium	88
Cm	Curium	96		Rn	Radon	86
Ds	Darmstadtium	110		Re	Rhenium	75
Db	Dubnium	105		Rh	Rhodium	45
Dy	Dysprosium	66		Rg	Roentgenium	111
Es	Einsteinium	99		Rb	Rubidium	37
Er	Erbium	68		Ru	Ruthenium	44
Eu	Europium	63		Rf	Rutherfordium	104
Fm	Fermium	100		Sm	Samarium	62
Fl	Flerovium	114		Sc	Scandium	21
F	Fluorine	9		Sg	Seaborgium	106
Fr	Francium	87		Se	Selenium	34
Gd	Gadolinium	64		Si	Silicon	14
Ga	Gallium	31		Ag	Silver	47
Ge	Germanium	32		Na	Sodium	11
Au	Gold	79		Sr	Strontium	38
Hf	Hafnium	72		S	Sulfur	16
Hs	Hassium	108		Ta	Tantalum	73
He	Helium	2		Tc	Technetium	43
Ho	Holmium	67		Te	Tellurium	52
H	Hydrogen	1		Ts	Tennessine	117
In	Indium	49		Tb	Terbium	65
I	Iodine	53		Tl	Thallium	81
Ir	Iridium	77		Th	Thorium	90
Fe	Iron	26		Tm	Thulium	69
Kr	Krypton	36		Sn	Tin	50
La	Lanthanum	57		Ti	Titanium	22
Lr	Lawrencium	103		W	Tungsten	74
Pb	Lead	82		U	Uranium	92
Li	Lithium	3		V	Vanadium	23
Lv	Livermorium	116		Xe	Xenon	54
Lu	Lutetium	71		Yb	Ytterbium	70
Mg	Magnesium	12		Y	Yttrium	39
Mc	Moscovium	115		Zn	Zinc	30
Mn	Manganese	25		Zr	Zirconium	40
Mt	Meitnerium	109		 	 	 
")))
  (declare (type tokenizer tk))
  (the list
    (loop
      for symbol        of-type (or null string) = (funcall tk)
      for name          of-type (or null string) = (funcall tk)
      for atomic-number of-type (or null string) = (funcall tk)
      while symbol
      collect (make-element symbol name
                (parse-integer atomic-number))
      into    elements
      finally
        (let ((sorted-elements
                (sort elements #'< :key #'element-atomic-number)))
          (declare (type list sorted-elements))
          (dolist (element sorted-elements)
            (declare (type Element element))
            (format T "~&(register-element ~4@<~s~> ~15@<~s~> ~d)"
              (element-symbol        element)
              (element-name          element)
              (element-atomic-number element)))))))
