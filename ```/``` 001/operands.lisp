;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the instruction class and its related operands.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of operands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Operand
  "The ``Operand'' interface furnishes a foundry entreparted by all
   classes intent on the representation of ``` instruction operands.")

;;; -------------------------------------------------------

(defstruct (Immediate-Operand
  (:include     Operand)
  (:constructor make-immediate-operand (value)))
  "The ``Immediate-Operand'' class serves in the representation of an
   operand intended for its datum's construe as a literal integer
   value, disencumbered from the affiliation with the program memory."
  (value (error "Missing immediate operand value.")
         :type      integer
         :read-only T))

;;; -------------------------------------------------------

(defstruct (Referential-Operand
  (:include     Operand)
  (:constructor make-referential-operand (target)))
  "The ``Referential-Operand'' class serves in the representation of an
   operand intended for its datum's construe as an index into the
   program memory, whence the actually referenced value is elicited."
  (target (error "Missing referential operand target.")
          :type      integer
          :read-only T))

;;; -------------------------------------------------------

(defgeneric resolve-operand (operand memory)
  (:documentation
    "Obtains the OPERAND's value, founded upon the MEMORY's
     contemporaneous state.")
  
  (:method ((operand Immediate-Operand) (memory Memory))
    (declare (type Immediate-Operand operand))
    (declare (type Memory            memory))
    (declare (ignore                 memory))
    (the integer
      (immediate-operand-value operand)))
  
  (:method ((operand Referential-Operand) (memory Memory))
    (declare (type Referential-Operand operand))
    (declare (type Memory              memory))
    (the integer
      (cell-value memory
        (referential-operand-target operand)))))

;;; -------------------------------------------------------

(defun locate-memory-address (start offset memory)
  "Supputates from the START index into the MEMORY and the OFFSET
   relative from the former location the ultimate cell address."
  (declare (type Operand start))
  (declare (type Operand offset))
  (declare (type Memory  memory))
  (the integer
    (+ (resolve-operand start  memory)
       (resolve-operand offset memory))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "Instruction".                       -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Instruction
  "The ``Instruction'' class is apportioned that onus appertaining to
   a ``` operation configuration's castaldy, ligated into this dever by
   a governance over both the source's address mode and data, as well
   as the destination memory's index."
  (source-mode        (error "Missing source mode.")
                      :type      address-mode
                      :read-only T)
  (source-start       (error "Missing source start.")
                      :type      Operand
                      :read-only T)
  (source-offset      (make-immediate-operand 0)
                      :type      Operand
                      :read-only T)
  (destination-start  (error "Missing destination start.")
                      :type      Operand
                      :read-only T)
  (destination-offset (make-immediate-operand 0)
                      :type      Operand
                      :read-only T))

;;; -------------------------------------------------------

(defun resolve-instruction-source (instruction memory)
  "Returns the datum addressed by the INSTRUCTION's SOURCE with respect
   to the MEMORY's contemporaneous state."
  (declare (type Instruction instruction))
  (declare (type Memory      memory))
  (the integer
    (case (instruction-source-mode instruction)
      (:immediate
        (locate-memory-address
          (instruction-source-start  instruction)
          (instruction-source-offset instruction)
          memory))
      (:referential
        (cell-value memory
          (locate-memory-address
            (instruction-source-start  instruction)
            (instruction-source-offset instruction)
            memory)))
      (otherwise
        (error "Invalid instruction source mode: ~s."
          (instruction-source-mode instruction))))))

;;; -------------------------------------------------------

(defun locate-instruction-destination (instruction memory)
  "Determines the address into the MEMORY at which the INSTRUCTION
   destination would desiderate a new datum's insertion."
  (declare (type Instruction instruction))
  (declare (type Memory      memory))
  (the integer
    (locate-memory-address
      (instruction-destination-start  instruction)
      (instruction-destination-offset instruction)
      memory)))

;;; -------------------------------------------------------

(defun get-destination-and-source (instruction memory)
  "Extracts the INSTRUCTION's destination address and source value in
   dependence upon the MEMORY and returns two values:
     (1) The destination address into the MEMORY to which the
         INSTRUCTION's source value is intended to be written.
     (2) The resolved source value of the INSTRUCTION that shall be
         written to the destination address in the MEMORY."
  (declare (type Instruction instruction))
  (declare (type Memory      memory))
  (the (values integer integer)
    (values
      (locate-instruction-destination instruction memory)
      (resolve-instruction-source     instruction memory))))
