;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the "NGVariable" class, a representation of a
;; variable in the NeverGonna programming language.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of class "NGVariable".                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (NGVariable
  (:constructor make-ngvariable (name)))
  "The ``NGVariable'' class serves as a representation for variables in
   a NeverGonna program, composed of the identifying agnomination, a
   flag whose dever it constitutes to communicate whether the variable
   has been assigned a value following its declaration, and the value
   itself as a third component."
  (name          (error "Missing name.")
                 :type      string
                 :read-only T)
  (initialized-p NIL
                 :type      boolean
                 :read-only NIL)
  (value         NIL
                 :type      (or null NGObject)
                 :read-only NIL))
