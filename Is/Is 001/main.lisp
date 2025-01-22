;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program implements an interpreter for the esoteric programming
;; language "Is", invented by the Esolang user "A" and presented on
;; July 5th, 2018, its kenspeckle designment proceeding from the
;; champarty of two memory components, this being, imprimis, a scalar
;; "current bit", and an array of bits, being operated upon by a mere
;; twissel of instructions, impounded in their potential to the
;; memory components' modulation.
;; 
;; 
;; Concept
;; =======
;; The Is programming language is founded upon the interaction betwixt
;; a scalar bit, nevened the "current bit", and a potentially infinite
;; dispansion of bits stored in a one-dimensional array, the
;; instruction identifiers being desumed from the eponymous set of
;; characters admitting "i" and "s".
;; 
;; == THE MEMORY: A SINGLE BIT AND A VECTOR THEREOF ==
;; The Is memory model enumerates a twain of components, imprimis, a
;; bit scalar salvatory; secondarily, a hypothetically infinite array
;; of bits.
;; 
;; == THE CURRENT BIT: A SCALAR STORAGE ==
;; The first memory component enumerates an aefauld bit, norned the
;; "current bit", its responsiveness exhausted already by an indagation
;; and an incrementing, which in the latter case limns a bit flipping's
;; tantamount. Its state at the program's inchoation resolves to the
;; value zero (0).
;; 
;; == THE BIT ARRAY: AN INFINITE DISPANSION OF BITS ==
;; Conceived in an abstract mode of ideation, the memory's second, and
;; more involved, moeity's firmament presents a vector of bits, at the
;; program's inception each assuming the standard value of zero (0).
;; The mete of the array's mickleness remains a dependency upon the
;; concrete implementation.
;; 
;; The memory's definition encompasses the permission to overflow upon
;; its accommodated parcel's transgression.
;; 
;; == THE BIT VECTOR COPIES THE CURRENT BIT ==
;; The coefficiency of the current bit and the vector bears the entirety
;; of the Is language's potential, the memory upon a respective behest
;; appropriating the current bit's state for its cells' complete
;; membership.
;; 
;; 
;; Syntax
;; ======
;; The Is programming language's syntactical perspective delineates a
;; sequence of zero or more "i" or "s" instruction identifiers, with the
;; contingency for whitespace, but no adscititious entity's toleration.
;; 
;; == GRAMMAR ==
;; A more formal conspection shall be adhibited to the syntaxis by
;; adminiculum of an Extended Backus-Naur Form (ENBF) treatise:
;; 
;;   program   := { "i" | "s" | whitespace } ;
;;   whitspace := " " | "\t" | "\n" ;
;; 
;; 
;; Instructions
;; ============
;; A twissel of operations already exhausts the Is programming
;; language's competences, comprehending the modificatino of the current
;; bit and the bit array.
;; 
;; == OVERVIEW ==
;; The following apercu's dation shall be realized in a cursory mete of
;; nortelry's communication anenst the language's instructions:
;; 
;;   ------------------------------------------------------------------
;;   Command | Effect
;;   --------+---------------------------------------------------------
;;   i       | Increments the current bit; upon its upper bourne's
;;           | transgression, which constitutes the value one (1),
;;           | wrapping around to the minimum state of zero (0).
;;           |---------------------------------------------------------
;;           | This operation constitutes a paregal to a bit state
;;           | flipping.
;;   ..................................................................
;;   s       | Sets all bits of the memory to the current bit's state.
;;           |---------------------------------------------------------
;;           | The program memory might overflow as an epiphenomenon of
;;           | this action.
;;   ------------------------------------------------------------------
;; 
;; 
;; Implementation
;; ==============
;; The interpreter's implementation is realized in the programming
;; language Common Lisp, operating immediately on the input source
;; string.
;; 
;; An extensible scheme for the program memory's bit array component
;; partakes of this software solution, begetting the capacitation to
;; select a static or dynamic variation; as well as, for developers
;; acquainted to the handling of Common Lisp, furnishing the
;; contingency for bespoke memory models' implementation.
;; 
;; --------------------------------------------------------------------
;; 
;; Author: Kaveh Yousefi
;; Date:   2025-01-19
;; 
;; Sources:
;;   [esolang2019Is]
;;   The Esolang contributors, "Is", December 25th, 2019
;;   URL: "https://esolangs.org/wiki/Is"
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Definition of interface "Memory".                            -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Memory ()
  ()
  (:documentation
    "The ``Memory'' interface serves in the furnishment of a common
     substratum for all concrete implementations of an Is program's
     memory, represented as a random-access vector of bits."))

;;; -------------------------------------------------------

(defgeneric set-memory-to (memory new-value)
  (:documentation
    "Sets all elements in the MEMORY to the NEW-VALUE, upon each
     modification printing the affected cell's updated state to the
     standard output, and returns no value."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Dynamic-Memory".                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Dynamic-Memory (Memory)
  ((cells
    :initform      (make-array 0
                     :element-type    'bit
                     :initial-element 0
                     :adjustable      T
                     :fill-pointer    T)
    :type          bit-vector
    :documentation "A theoretically \"infinite\" dispansion of
                    bit-valued cells consigned to a dynamically
                    expanding vector's castaldy."))
  (:documentation
    "The ``Dynamic-Memory'' class defines an Is program memory founded
     upon a theoretically infinite dispansion of bit-valued cells,
     its castaldy ultimately assigned to a dynamically expanding
     vector's bailiwick.
     ---
     This memory implementation simulates an array overflow during its
     cells' modification by a cyclic approach, commencing the setting
     of values new after the upper vector index bourne's
     transcendence in order to achieve a perpetual writing."))

;;; -------------------------------------------------------

(defun make-dynamic-memory ()
  "Creates and returns a fresh ``Dynamic-Memory'' whose cells are
   initialized to the default value of zero (0)."
  (the Dynamic-Memory
    (make-instance 'Dynamic-Memory)))

;;; -------------------------------------------------------

(defmethod set-memory-to ((memory Dynamic-Memory) (new-value integer))
  "Perpetually updates all of the MEMORY's \"infinite\" tally of cells
   to the NEW-VALUE, concomitantly printing the updated states, and,
   hypothetically, returns no value."
  (declare (type Dynamic-Memory memory))
  (declare (type bit            new-value))
  (with-slots (cells) memory
    (declare (type bit-vector cells))
    (flet ((print-cell-at (index)
            "Prints the cell stored in the CELL amenable to the INDEX
             to the standard output and returns no value."
            (declare (type fixnum index))
            (format T "~d"
              (bit cells index))
            (values)))
      (loop with current-cell-index of-type fixnum = 0 do
        (cond
          ;; Maximum homologated array size transgressed?
          ;; => Commence anew.
          ((>= current-cell-index array-total-size-limit)
            (setf current-cell-index 0))
          ;; Underlying CELLS vector's capacity suffices?
          ;; => Replace current element.
          ((< current-cell-index (length cells))
            (setf (bit cells current-cell-index) new-value)
            (print-cell-at current-cell-index)
            (incf current-cell-index))
          ;; Underlying CELLS vector's capacity exhausted?
          ;; => Expand CELLS and append NEW-VALUE to its new desinence.
          (T
            (vector-push-extend new-value cells)
            (print-cell-at current-cell-index)
            (incf current-cell-index))))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Static-Memory".                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Static-Memory (Memory)
  ((cells
    :initarg       :cells
    :type          simple-bit-vector
    :documentation "The fixed-size bit vector."))
  (:documentation
    "The ``Static-Memory'' class realizes a Is program memory founded
     upon a fixed-size array of bit values."))

;;; -------------------------------------------------------

(defun make-static-memory (size)
  "Creates and returns a fresh ``Static-Memory'' tallying the SIZE
   number of cells."
  (declare (type fixnum size))
  (the Static-Memory
    (make-instance 'Static-Memory :cells
      (make-array size
        :element-type    'bit
        :initial-element 0
        :adjustable      NIL
        :fill-pointer    NIL))))

;;; -------------------------------------------------------

(defmethod set-memory-to ((memory Static-Memory) (new-value integer))
  "Sets all cells extant in the MEMORY's compass to the NEW-VALUE,
   concomitantly printing their updated states, and returns no value."
  (declare (type Static-Memory memory))
  (declare (type bit           new-value))
  (with-slots (cells) memory
    (declare (type simple-bit-vector cells))
    (loop
      for current-index
        of-type fixnum
        from    0
        below   (length cells)
      do
        (setf (sbit cells current-index) new-value)
        (format T "~d"
          (sbit cells current-index))))
  (values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-default-program-memory ()
  "Creates and returns a fresh default ``Memory'' implementation for an
   Is program."
  (the Memory
    (make-dynamic-memory)))

;;; -------------------------------------------------------

(defun interpret-Is (code
                     &key (memory (make-default-program-memory)))
  "Interprets the piece of Is source CODE, optionally deploying a
   bespoke MEMORY instance for its operative purposes, and returns no
   value."
  (declare (type string code))
  (declare (type Memory memory))
  (let ((ip          0)
        (current-bit 0))
    (declare (type fixnum ip))
    (declare (type bit    current-bit))
    (symbol-macrolet
        ((current-token
          (the character
            (char code ip)))
         (program-has-completed-p
          (the boolean
            (not (array-in-bounds-p code ip)))))
      (declare (type character current-token))
      (declare (type boolean   program-has-completed-p))
      (loop until program-has-completed-p do
        (case current-token
          ((#\Newline #\Return #\Space #\Tab)
            (incf ip))
          (#\i
            (setf current-bit (- 1 current-bit))
            (incf ip))
          (#\s
            (set-memory-to memory current-bit)
            (incf ip))
          (otherwise
            (error "Unrecognized token \"~c\" at position ~d."
              current-token ip))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simulate a truth-machine with an input of zero (0).
(interpret-Is "s"
  :memory (make-static-memory 1))

;;; -------------------------------------------------------

;; Simulate a truth-machine with an input of one (1).
(interpret-Is "is")
