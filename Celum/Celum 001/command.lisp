;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the implementation of the various Celum commands
;; in the form of dedicated classes.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct Command
  "The ``Command'' interface affords a common base for all classes
   dedicated to the representation of Celum commands.")

;;; -------------------------------------------------------

(defstruct (Cellular-Automaton-Command
  (:include Command))
  "The ``Cellular-Automaton-Command'' class represents the Celum command
   composed of two consecutive hexadecimal digits, their decimal integer
   value designating one of the 256 possible elementary cellular
   automaton rules, intended for the application on the entire tape."
  (rule (error "Missing rule.") :type automaton-rule))

;;; -------------------------------------------------------

(defstruct (Input-Command
  (:include Command))
  "The ``Input-Command'' class reprsents the Celum command \"i\",
   dedicated to the input of a bit and its transfer into the tape's
   central cell.")

;;; -------------------------------------------------------

(defstruct (Output-Command
  (:include Command))
  "The ``Output-Command'' class represents the Celum command \"o\",
   dedicated to tape's center bit's output.")

;;; -------------------------------------------------------

(defstruct (Search-Label-Command
  (:include Command))
  "The ``Search-Label-Command'' class represents the Celum command
   \"!\", dedicated to the toggling of the Boolean flag and the
   subsequent search for a line by a label identifier."
  (name (error "Missing label.") :type string))

;;; -------------------------------------------------------

(defstruct (Search-Prefix-Above-Command
  (:include Command))
  "The ``Search-Prefix-Above-Command'' class represents the Celum
   command \"{\", dedicated to the toggling of the tape's center bit and
   the subsequent search for a line bearing a prefix bit equivalent to
   the new center value, while being located above the currently
   processed line.")

;;; -------------------------------------------------------

(defstruct (Search-Prefix-Below-Command
  (:include Command))
  "The ``Search-Prefix-Above-Command'' class represents the Celum
   command \"}\", dedicated to the toggling of the tape's center bit and
   the subsequent search for a line bearing a prefix bit equivalent to
   the new center value, while being located below the currently
   processed line.")

;;; -------------------------------------------------------

(defstruct (Skip-Command
  (:include Command))
  "The ``Skip-Command'' class represents the Celum command \"?\",
   dedicated to the conditional skipping of a line's remaining commands
   in the case of the program's Boolean flag assuming a value of zero
   (0).")
